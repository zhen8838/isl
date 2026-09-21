/*
 * Copyright 2008-2009 Katholieke Universiteit Leuven
 *
 * Use of this software is governed by the MIT license
 *
 * Written by Sven Verdoolaege, K.U.Leuven, Departement
 * Computerwetenschappen, Celestijnenlaan 200A, B-3001 Leuven, Belgium
 */

#include <isl_ctx_private.h>
#include <isl_map_private.h>
#include "isl_basis_reduction.h"
#include <isl_factorization.h>
#include "isl_scan.h"
#include <isl_seq.h>
#include "isl_tab.h"
#include <isl_val_private.h>
#include <isl_vec_private.h>

struct isl_counter {
	struct isl_scan_callback callback;
	isl_int count;
	isl_int max;
};

static isl_stat increment_counter(struct isl_scan_callback *cb,
	__isl_take isl_vec *sample)
{
	struct isl_counter *cnt = (struct isl_counter *)cb;

	isl_int_add_ui(cnt->count, cnt->count, 1);

	isl_vec_free(sample);

	if (isl_int_is_zero(cnt->max) || isl_int_lt(cnt->count, cnt->max))
		return isl_stat_ok;
	return isl_stat_error;
}

static int increment_range(struct isl_scan_callback *cb, isl_int min, isl_int max)
{
	struct isl_counter *cnt = (struct isl_counter *)cb;

	isl_int_add(cnt->count, cnt->count, max);
	isl_int_sub(cnt->count, cnt->count, min);
	isl_int_add_ui(cnt->count, cnt->count, 1);

	if (isl_int_is_zero(cnt->max) || isl_int_lt(cnt->count, cnt->max))
		return 0;
	isl_int_set(cnt->count, cnt->max);
	return -1;
}

/* Call callback->add with the current sample value of the tableau "tab".
 */
static int add_solution(struct isl_tab *tab, struct isl_scan_callback *callback)
{
	struct isl_vec *sample;

	if (!tab)
		return -1;
	sample = isl_tab_get_sample_value(tab);
	if (!sample)
		return -1;

	return callback->add(callback, sample);
}

static isl_stat scan_0D(__isl_take isl_basic_set *bset,
	struct isl_scan_callback *callback)
{
	struct isl_vec *sample;

	sample = isl_vec_alloc(bset->ctx, 1);
	isl_basic_set_free(bset);

	if (!sample)
		return isl_stat_error;

	isl_int_set_si(sample->el[0], 1);

	return callback->add(callback, sample);
}

/* Look for all integer points in "bset", which is assumed to be bounded,
 * and call callback->add on each of them.
 *
 * We first compute a reduced basis for the set and then scan
 * the set in the directions of this basis.
 * We basically perform a depth first search, where in each level i
 * we compute the range in the i-th basis vector direction, given
 * fixed values in the directions of the previous basis vector.
 * We then add an equality to the tableau fixing the value in the
 * direction of the current basis vector to each value in the range
 * in turn and then continue to the next level.
 *
 * The search is implemented iteratively.  "level" identifies the current
 * basis vector.  "init" is true if we want the first value at the current
 * level and false if we want the next value.
 * Solutions are added in the leaves of the search tree, i.e., after
 * we have fixed a value in each direction of the basis.
 */
isl_stat isl_basic_set_scan(__isl_take isl_basic_set *bset,
	struct isl_scan_callback *callback)
{
	isl_size dim;
	struct isl_mat *B = NULL;
	struct isl_tab *tab = NULL;
	struct isl_vec *min;
	struct isl_vec *max;
	struct isl_tab_undo **snap;
	int level;
	int init;
	enum isl_lp_result res;

	dim = isl_basic_set_dim(bset, isl_dim_all);
	if (dim < 0) {
		bset = isl_basic_set_free(bset);
		return isl_stat_error;
	}

	if (dim == 0)
		return scan_0D(bset, callback);

	min = isl_vec_alloc(bset->ctx, dim);
	max = isl_vec_alloc(bset->ctx, dim);
	snap = isl_alloc_array(bset->ctx, struct isl_tab_undo *, dim);

	if (!min || !max || !snap)
		goto error;

	tab = isl_tab_from_basic_set(bset, 0);
	if (!tab)
		goto error;
	if (isl_tab_extend_cons(tab, dim + 1) < 0)
		goto error;

	tab->basis = isl_mat_identity(bset->ctx, 1 + dim);
	if (1)
		tab = isl_tab_compute_reduced_basis(tab);
	if (!tab)
		goto error;
	B = isl_mat_copy(tab->basis);
	if (!B)
		goto error;

	level = 0;
	init = 1;

	while (level >= 0) {
		int empty = 0;
		if (init) {
			res = isl_tab_min(tab, B->row[1 + level],
				    bset->ctx->one, &min->el[level], NULL, 0);
			if (res == isl_lp_empty)
				empty = 1;
			if (res == isl_lp_error || res == isl_lp_unbounded)
				goto error;
			isl_seq_neg(B->row[1 + level] + 1,
				    B->row[1 + level] + 1, dim);
			res = isl_tab_min(tab, B->row[1 + level],
				    bset->ctx->one, &max->el[level], NULL, 0);
			isl_seq_neg(B->row[1 + level] + 1,
				    B->row[1 + level] + 1, dim);
			isl_int_neg(max->el[level], max->el[level]);
			if (res == isl_lp_empty)
				empty = 1;
			if (res == isl_lp_error || res == isl_lp_unbounded)
				goto error;
			snap[level] = isl_tab_snap(tab);
		} else
			isl_int_add_ui(min->el[level], min->el[level], 1);

		if (empty || isl_int_gt(min->el[level], max->el[level])) {
			level--;
			init = 0;
			if (level >= 0)
				if (isl_tab_rollback(tab, snap[level]) < 0)
					goto error;
			continue;
		}
		if (level == dim - 1 && callback->add == increment_counter) {
			if (increment_range(callback,
					    min->el[level], max->el[level]))
				goto error;
			level--;
			init = 0;
			if (level >= 0)
				if (isl_tab_rollback(tab, snap[level]) < 0)
					goto error;
			continue;
		}
		isl_int_neg(B->row[1 + level][0], min->el[level]);
		if (isl_tab_add_valid_eq(tab, B->row[1 + level]) < 0)
			goto error;
		isl_int_set_si(B->row[1 + level][0], 0);
		if (level < dim - 1) {
			++level;
			init = 1;
			continue;
		}
		if (add_solution(tab, callback) < 0)
			goto error;
		init = 0;
		if (isl_tab_rollback(tab, snap[level]) < 0)
			goto error;
	}

	isl_tab_free(tab);
	free(snap);
	isl_vec_free(min);
	isl_vec_free(max);
	isl_basic_set_free(bset);
	isl_mat_free(B);
	return isl_stat_ok;
error:
	isl_tab_free(tab);
	free(snap);
	isl_vec_free(min);
	isl_vec_free(max);
	isl_basic_set_free(bset);
	isl_mat_free(B);
	return isl_stat_error;
}

isl_stat isl_set_scan(__isl_take isl_set *set,
	struct isl_scan_callback *callback)
{
	int i;

	if (!set || !callback)
		goto error;

	set = isl_set_cow(set);
	set = isl_set_make_disjoint(set);
	set = isl_set_compute_divs(set);
	if (!set)
		goto error;

	for (i = 0; i < set->n; ++i)
		if (isl_basic_set_scan(isl_basic_set_copy(set->p[i]),
					callback) < 0)
			goto error;

	isl_set_free(set);
	return isl_stat_ok;
error:
	isl_set_free(set);
	return isl_stat_error;
}

int isl_basic_set_count_upto(__isl_keep isl_basic_set *bset,
	isl_int max, isl_int *count)
{
	struct isl_counter cnt = { { &increment_counter } };

	if (!bset)
		return -1;

	isl_int_init(cnt.count);
	isl_int_init(cnt.max);

	isl_int_set_si(cnt.count, 0);
	isl_int_set(cnt.max, max);
	if (isl_basic_set_scan(isl_basic_set_copy(bset), &cnt.callback) < 0 &&
	    isl_int_lt(cnt.count, cnt.max))
		goto error;

	isl_int_set(*count, cnt.count);
	isl_int_clear(cnt.max);
	isl_int_clear(cnt.count);

	return 0;
error:
	isl_int_clear(cnt.count);
	return -1;
}

int isl_set_count_upto(__isl_keep isl_set *set, isl_int max, isl_int *count)
{
	struct isl_counter cnt = { { &increment_counter } };

	if (!set)
		return -1;

	isl_int_init(cnt.count);
	isl_int_init(cnt.max);

	isl_int_set_si(cnt.count, 0);
	isl_int_set(cnt.max, max);
	if (isl_set_scan(isl_set_copy(set), &cnt.callback) < 0 &&
	    isl_int_lt(cnt.count, cnt.max))
		goto error;

	isl_int_set(*count, cnt.count);
	isl_int_clear(cnt.max);
	isl_int_clear(cnt.count);

	return 0;
error:
	isl_int_clear(cnt.count);
	return -1;
}

struct isl_factor_count {
	isl_int count;
};

/* Multiply the number of elements in "bset" into data->count.
 * This function is used as an isl_factorizer_every_factor_basic_set
 * callback, so "bset" is known to be independent of the other factors.
 */
static isl_bool count_factor(__isl_keep isl_basic_set *bset, void *user)
{
	struct isl_factor_count *data = user;
	isl_int count;

	isl_int_init(count);
	if (isl_basic_set_count_upto(bset, bset->ctx->zero, &count) < 0) {
		isl_int_clear(count);
		return isl_bool_error;
	}

	isl_int_mul(data->count, data->count, count);
	isl_int_clear(count);

	return isl_bool_true;
}

/* Count the number of elements in "bset", exploiting any factorization
 * into independent groups of variables.
 *
 * The divs in "bset" are assumed to be known.  Lift them to ordinary
 * set dimensions such that isl_basic_set_factorizer can take them into
 * account.  Since known divs have a unique value for each element of the
 * original basic set, this lifting does not change the number of elements.
 *
 * If no factorization can be found, fall back to the regular scan on the
 * original basic set.  Otherwise, count each factor separately and multiply
 * the results.
 */
static int isl_basic_set_count_factorized(__isl_keep isl_basic_set *bset,
	isl_int *count)
{
	isl_bool every;
	isl_basic_set *lifted;
	isl_factorizer *f;
	struct isl_factor_count data;

	lifted = isl_basic_set_lift(isl_basic_set_copy(bset));
	f = isl_basic_set_factorizer(lifted);
	if (!f) {
		isl_basic_set_free(lifted);
		return -1;
	}

	if (f->n_group == 0) {
		isl_factorizer_free(f);
		isl_basic_set_free(lifted);
		return isl_basic_set_count_upto(bset, bset->ctx->zero, count);
	}

	isl_int_init(data.count);
	isl_int_set_si(data.count, 1);
	every = isl_factorizer_every_factor_basic_set(f, &count_factor, &data);
	isl_factorizer_free(f);
	isl_basic_set_free(lifted);
	if (every < 0) {
		isl_int_clear(data.count);
		return -1;
	}

	isl_int_set(*count, data.count);
	isl_int_clear(data.count);

	return 0;
}

/* Count the total number of elements in a parameter-free "set".
 *
 * Make the basic sets disjoint and compute their divs, just like
 * isl_set_scan.  Count each basic set using any available factorization
 * and add the results.
 */
static int isl_set_count_factorized(__isl_keep isl_set *set, isl_int *count)
{
	int i;
	isl_int total;
	isl_int n;

	set = isl_set_make_disjoint(isl_set_copy(set));
	set = isl_set_compute_divs(set);
	if (!set)
		return -1;

	isl_int_init(total);
	isl_int_init(n);
	isl_int_set_si(total, 0);

	for (i = 0; i < set->n; ++i) {
		if (isl_basic_set_count_factorized(set->p[i], &n) < 0)
			goto error;
		isl_int_add(total, total, n);
	}

	isl_int_set(*count, total);
	isl_int_clear(n);
	isl_int_clear(total);
	isl_set_free(set);

	return 0;
error:
	isl_int_clear(n);
	isl_int_clear(total);
	isl_set_free(set);
	return -1;
}

int isl_set_count(__isl_keep isl_set *set, isl_int *count)
{
	isl_size nparam;
	isl_size nset;

	if (!set)
		return -1;

	/* Parameters are shared by all factors.  Multiplying scalar counts
	 * would count their possible values more than once, so preserve the
	 * original scan for sets involving parameters.  Preserve it for
	 * zero- and one-dimensional sets as well, where no factorization is
	 * possible and the final range is already counted in a single step.
	 */
	nparam = isl_set_dim(set, isl_dim_param);
	nset = isl_set_dim(set, isl_dim_set);
	if (nparam < 0 || nset < 0)
		return -1;
	if (nparam == 0 && nset > 1)
		return isl_set_count_factorized(set, count);

	return isl_set_count_upto(set, set->ctx->zero, count);
}

/* Count the total number of elements in "set" (in an inefficient way) and
 * return the result.
 */
__isl_give isl_val *isl_set_count_val(__isl_keep isl_set *set)
{
	isl_val *v;

	if (!set)
		return NULL;
	v = isl_val_zero(isl_set_get_ctx(set));
	v = isl_val_cow(v);
	if (!v)
		return NULL;
	if (isl_set_count(set, &v->n) < 0)
		v = isl_val_free(v);
	return v;
}
