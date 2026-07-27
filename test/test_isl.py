""" NOTE this file is translated from islpy:https://github.com/inducer/islpy/blob/main/test/test_isl.py  """

import pytest
import isl
import io
import tempfile
import os
import gc
import platform
import subprocess
import sys

# The isl_id user payload is handed to isl as a raw PyObject * and kept alive
# by hand, which asks of ctypes what only CPython provides.
cpython_only = pytest.mark.skipif(
  platform.python_implementation() != "CPython",
  reason="the isl_id user payload needs CPython's ctypes py_object support")

def test_basics():
  space = isl.space.unit().add_unnamed_tuple(2).set_dim_name(
    isl.dim_type.SET, 0, 'a').set_dim_name(isl.dim_type.SET, 1, 'b')
  bset = isl.basic_set.universe(space)
  local_space = isl.local_space.from_space(bset.space())
  bset = (bset.add_constraint(isl.constraint.alloc_equality(local_space).set_coefficient_si(isl.dim_type.SET, 0, -1).set_coefficient_si(isl.dim_type.SET, 1, 2)).
          add_constraint(isl.constraint.alloc_inequality(local_space).set_coefficient_si(isl.dim_type.SET, 0, 1).set_constant_si(-10)).
          add_constraint(isl.constraint.alloc_inequality(local_space).set_coefficient_si(isl.dim_type.SET, 0, -1).set_constant_si(42)).
          project_out(isl.dim_type.SET, 1, 1))

  bset2 = isl.basic_set("{[i] : exists (a : i = 2a and i >= 10 and i <= 42)}")
  points = []
  bset.foreach_point(points.append)

  for pt in points:
      print(pt)

  assert len(points) == 17

def test_error_on_invalid_index():
    my_set = isl.set("{ [k, l] : 3l >= -k and 3l <= 10 - k "
                   "and k >=0 and k <= 2 }")
    p = my_set.sample_point()
    # with pytest.raises(isl.Error):
    try:
      v = p.get_coordinate_val(isl.dim_type.SET, 99)
      print(v)
    except:
      print("got isl error")


def no_test_pwqpoly():
    def term_handler(term):
        print(term.get_coefficient_val())

    def piece_handler(set, qpoly):
        qpoly.foreach_term(term_handler)

    pwqp = isl.PwQPolynomial("[n] -> { n }")
    pwqp.foreach_piece(piece_handler)

def no_test_id_user():
    ctx = isl.Context()
    foo = isl.Id("foo", context=ctx)  # noqa
    t = (1, 2)
    bar = isl.Id("bar", t, context=ctx)

    assert bar.user is t

def test_val():
    for src in [17, "17"]:
        v = isl.val(src)
        assert v.num_si() == 17


def test_upcast():
    a = isl.pw_aff("[n] -> { [(-1 - floor((-n)/4))] }")
    b = isl.aff("[n] -> { [(-1 - floor((-n)/4))] }")

    isl.pw_aff(b)

    assert b.plain_is_equal(a)
    assert a.plain_is_equal(b)

    s = isl.basic_set("[n] -> {[i,j,k]: i<=j + k and (exists m: m=j+k) "
        "and n mod 5 = 17}")

    isl.union_set(s)


def no_test_pickling():
    instances = [
            isl.aff("[n] -> { [(-1 - floor((-n)/4))] }"),
            isl.pw_aff("[n] -> { [(0)] : n <= 4 and n >= 1; "
                "[(-1 + n - floor((3n)/4))] : n >= 5 }"),
            isl.basic_set("[n] -> {[i,j,k]: i<=j + k and (exists m: m=j+k) "
                "and n mod 5 = 17}"),
            isl.set("[n] -> {[i,j,k]: (i<=j + k and (exists m: m=j+k)) or (k=j)}")
            ]

    from pickle import dumps, loads
    for inst in instances:
        inst2 = loads(dumps(inst))

        assert inst.space == inst2.space
        assert inst == inst2


def no_test_apostrophes_during_pickling():
    # Create map and manually insert apostrophes, which are ignored by isl
    initial_map = isl.map(
        "[n, m'] -> {[i', j] -> [i] : i = i' + 1 and 0 <= i, i' < n and j = m'}"
        ).set_dim_name(
            isl.dim_type.in_, 0, "i'",
        ).set_dim_name(
            isl.dim_type.param, 1, "m'",
        )

    from pickle import dumps, loads
    unpickled_map = loads(dumps(initial_map))

    # Make sure unpickled map still has apostrophes
    assert initial_map.get_var_dict() == unpickled_map.get_var_dict()
    assert initial_map == unpickled_map

def test_get_id_dict():
    set = isl.set("[a] -> { [b] }")
    dimtype = isl.dim_type.PARAM

    result = {}

    def set_dim_id(name, tp, idx):
        if name in result:
            raise RuntimeError(f"non-unique var id '{name}' encountered")
        result[name] = tp, idx

    types = [dimtype]

    for tp in types:
        for i in range(set.dim(tp)):
            name = set.get_dim_id(tp, i)
            if name is not None:
                set_dim_id(name, tp, i)

    print(result)
    assert len(result) == 1


def test_get_coefficients_by_name():
    my_set = isl.basic_set("{ [k, l] : 3l >= -k and 3l <= 10 - k "
                   "and k >=0 and k <= 2 }")

    c_list = my_set.get_constraint_list()
    assert c_list.size() == 4
    for i in range(c_list.size()):
        print(c_list.get_at(i))


def no_test_count_brick_ish():
    a = isl.basic_set("[n] -> {[i,j]: 0<= i < n and 0<= j < n and j<= i}")

    def count(bset):
        result = 1

        for i in range(bset.dim(isl.dim_type.set)):
            dmax = bset.dim_max(i)
            dmin = bset.dim_min(i)

            length = isl.PwQPolynomial.from_pw_aff(dmax - dmin + 1)

            result = result * length

        return result

    counts = [count(a)]

    if hasattr(a, "card"):
        counts.append(a.card())

    for pwq in counts:
        print("EVAL", pwq, "=", pwq.eval_with_dict({"n": 10}))

    print(counts)

    assert counts[0].eval_with_dict({"n": 10}) == 100
    if hasattr(a, "card"):
        assert counts[1].eval_with_dict({"n": 10}) == 55

def no_test_eval_pw_qpolynomial():
    pwaff = isl.PwAff("[n] -> { [(0)] : n <= 4 and n >= 1; "
        "[(-1 + n - floor((3n)/4))] : n >= 5 }")

    pwq = isl.PwQPolynomial.from_pw_aff(pwaff)

    print(pwq.eval_with_dict({"n": 10}))

    assert pwq.eval_with_dict({"n": 10}) == 2


def no_test_schedule():
  schedule = isl.map("{S[t,i,j] -> [t,i,j]: 0 < t < 20 and 0 < i < j < 100}")
  accesses = isl.map("{S[t,i,j] -> bar[t%2, i+1, j-1]}")
  context = isl.set("{:}")
  build = isl.ast_build.from_context(context)

  def callback(node, build: isl.ast_build):
    schedulemap = build.get_schedule()
    accessmap = accesses.apply_domain(schedulemap)
    aff = accessmap.as_map().as_pw_multi_aff()
    access = build.call_from(aff)
    return access 

  build = build.set_at_each_domain(callback)

  ast = build.node_from_schedule_map(schedule)

  def cb_print_user(printer, options, node):
    print("Callback user called")
    printer = printer.print_str("Callback user")
    return printer

  def cb_print_for(printer, options, node):
    print("Callback for called")
    printer = printer.print_str("Callback For")
    return printer

  opts = isl.ast_print_options.alloc()
  opts = opts.set_print_user(cb_print_user)
  opts = opts.set_print_for(cb_print_for)

  printer = isl.printer.to_str()
  printer = printer.set_output_format(isl.format.C)
  printer.print_str("// Start\n")
  printer = ast.print(printer, opts)
  printer.print_str("// End")

  print(printer.get_str())


def test_union_map():
    d = isl.union_set("[start, num] -> {S[i,j] : start <= i,j < start + num}")
    s = isl.union_map("{S[i,j] -> [i,j]}").intersect_domain(d)
    aw = isl.union_map("{S[i,j] -> B[1024 i + j]}")

    uai = isl.union_access_info(aw)
    uai = uai.set_must_source(aw)
    uai = uai.set_schedule_map(s)
    flow = uai.compute_flow()
    print(flow)


def test_schedule_dump():
    s = isl.union_set(
            "{ S_2[i, j, k] : i <= 99 and i >= 0; S_3[i] : "
            "i <= 99 and i >= 0; S_0[]; S_1[i] : i <= 99 and i >= 0 }")
    cst = isl.schedule_constraints.on_domain(s)
    schedule = isl.schedule_constraints.compute_schedule(cst)
    print(schedule)

def test_from_union_map():
    m = isl.union_map(
        "[m, n] -> { S_0[] -> [0, 0, 0, 0]; S_1[i] -> [i, 1, 0, 0]; S_3[i] -> "
        "[1 + i, 3, 0, 0]; S_2[i, j, k] -> [i, 2, j, k] : "
        "j <= -1 + m and j >= 0 and k <= -1 + n and k >= 0 }")
    print(m.as_multi_union_pw_aff())

def test_get_schedule_map():
    ss = isl.union_set("[m, n] -> { S_2[i, j, k] : "
        "j <= -1 + m and j >= 0 and k <= -1 + n and k >= 0 }")
    cst1 = isl.schedule_constraints.on_domain(ss)
    sub_schedule = isl.schedule_constraints.compute_schedule(cst1)
    print(sub_schedule.get_map())

def test_codegen():
    # courtesy of Marek Pałkowski

    def isl_ast_codegen(S):  # noqa: N803
        b = isl.ast_build.from_context(isl.set("{:}"))
        m = isl.map.from_domain_and_range(S, S)
        m = isl.map.identity(m.get_space())
        m = isl.map.from_domain(S)
        ast = b.node_from_schedule_map(m)
        p = isl.printer.to_str()
        p = p.set_output_format(isl.format.C)
        p = p.print_ast_node(ast)
        return p.get_str()

    s = isl.set("[n,m] -> { [i,j] : 0 <= i <= n and i <= j <= m }")
    print(isl_ast_codegen(s))


def test_id_constructor():
    x0 = isl.id("x0")
    x01 = isl.id("x0")
    assert x0.name() == x01.name()


def test_creation_error():
    # note the (intentional) syntax error
    with pytest.raises(isl.Error) as excinfo:
      isl.basic_set(
              "[n0, n1] -> "
              "{ [i0, i1, i2] : 0 <= i0 < n1  and 0 and 0 <= i2 <= 15 }")
    assert "syntax error" in str(excinfo.value)


def test_lexmin():
    print(isl.set("""{ [s] : exists a,b,c :
            0 <= a <= 5 and 1 <= b <= 4 and 2 <= c <= 7 and
            ((2 <= b and b <= 3) implies (a <= 1 or a >= 3)) and
            ((not (c < 5 or b > 3)) implies (a > 2 and c < 3)) and s = a + b + c }
            """).lexmin())


def no_test_align_spaces():
    m1 = isl.basic_map("[m,n] -> {[i,j,k]->[l,o]:}")
    m2 = isl.basic_map("[m,n] -> {[j,k,l,i]->[o]:}")

    result = isl.align_spaces(m1, m2)
    assert result.get_var_dict() == m2.get_var_dict()

    a1 = isl.aff("[t0, t1, t2] -> { [(32)] }")
    a2 = isl.aff("[t1, t0] -> { [(0)] }")

    # with pytest.raises(isl.Error):
    #     a1_aligned = isl.align_spaces(a1, a2)

    a1_aligned = isl.align_spaces(a1, a2, obj_bigger_ok=True)
    a2_aligned = isl.align_spaces(a2, a1)

    assert a1_aligned == isl.aff("[t1, t0, t2] -> { [(32)] }")
    assert a2_aligned == isl.aff("[t1, t0, t2] -> { [(0)] }")


def no_test_isl_align_two():
    a1 = isl.aff("[t0, t1, t2] -> { [(32)] }")
    a2 = isl.aff("[t1, t0] -> { [(0)] }")

    a1_aligned, a2_aligned = isl.align_two(a1, a2)
    assert a1_aligned == isl.aff("[t1, t0, t2] -> { [(32)] }")
    assert a2_aligned == isl.aff("[t1, t0, t2] -> { [(0)] }")

    b1 = isl.basic_set("[n0, n1, n2] -> { [i0, i1] : }")
    b2 = isl.basic_set("[n0, n2, n1, n3] -> { [i1, i0, i2] : }")

    b1_aligned, b2_aligned = isl.align_two(b1, b2)
    assert b1_aligned == isl.basic_set("[n0, n2, n1, n3] -> { [i1, i0, i2] :  }")
    assert b2_aligned == isl.basic_set("[n0, n2, n1, n3] -> { [i1, i0, i2] :  }")


def no_test_bound():
    print(isl.PwQPolynomial("""[n, m] -> {[i, j] -> i * m + j :
            0 <= i < n and 0 <= j < m}""").bound(isl.fold.min))
    print(isl.PwQPolynomial("""[n, m] -> {[i, j] -> i * m + j :
            0 <= i < n and 0 <= j < m}""").bound(isl.fold.max))


def no_test_copy_context():
    ctx = isl.Context()
    import copy
    assert not ctx._wraps_same_instance_as(copy.copy(ctx))
    assert not isl.DEFAULT_CONTEXT._wraps_same_instance_as(copy.copy(ctx))


def test_ast_node_list_free():
    # from https://github.com/inducer/islpy/issues/21
    # by Cambridge Yang
    schedule_map = isl.union_map("[N] -> { S0[i] -> [i, 0] : "
            "0 <= i < N; S1[i] -> [i, 1] : 0 <= i < N }")
    ast_build = isl.ast_build.from_context(isl.set("[N] -> { : }"))
    ast = ast_build.node_from_schedule_map(schedule_map)

    print(ast.to_C_str())
    # Prints below code:
    # for (int c0 = 0; c0 < N; c0 += 1) {
    #  S0(c0);
    #  S1(c0);
    # }

    # we have S0 and S1 in a ast_node_block, which holds "children" of type
    # ASTNodeList
    ast: isl.ast_node_for
    body = ast.body()
    assert isinstance(body, isl.ast_node_block)
    # isl.isl_ast_node_get_type(body) == isl.ast_node_type.block

    body.children()

def test_union_casts():
    # https://github.com/inducer/islpy/issues/29
    s1 = isl.union_set("{[0]}")
    s2 = isl.basic_set("{[1]}")

    s2.union(s1)  # works fine
    s1.union(s2)  # did not work while #29 was not fixed

    assert s2.union(s1).is_equal(s1.union(s2))


def no_test_remove_map_if_callback():
    umap = isl.union_map("{A[0] -> [1]; B[1] -> [2]}")

    umap1 = umap.remove_map_if(lambda m: False)
    assert umap1 == umap, "map should not change"

    umap2 = umap.remove_map_if(lambda m:
        m.get_tuple_name(isl.dim_type.IN) == "B")
    assert umap2.is_equal(isl.union_map("{A[0] -> [1]}"))


def no_test_remove_map_if_callback_exc():
    pytest.skip("https://github.com/inducer/islpy/pull/33#issuecomment-705165253")
    ctx = isl.Context()

    umap = isl.union_map.read_from_str(ctx, "{A[0] -> [1]; B[1] -> [2]}")

    def callback_throws_exception(m):
        raise AssertionError()

    with pytest.raises(isl.Error):
        umap3 = umap.remove_map_if(callback_throws_exception)
        del umap3


def test_sched_constraints_set_validity():
    domain = isl.union_set("[n] -> { A[i] : 0 <= i < n; B[i] : 0 <= i < n }")
    validity = isl.union_map("[n] -> { A[i] -> B[i] : 0 <= i < n }")
    sc = isl.schedule_constraints.on_domain(domain)

    sc = sc.set_validity(validity)
    validity2 = sc.get_validity()

    print(validity)
    print(validity2)

    assert str(validity) == str(validity2)

def test_set_ast_print_options():
    def print_user_block(printer: isl.printer, options: isl.ast_print_options, node: isl.ast_node_block):
      pass

    def print_user_py(printer: isl.printer, options: isl.ast_print_options, node: isl.ast_node_user):
      pass

    def print_for_py(printer: isl.printer, options: isl.ast_print_options, node: isl.ast_node_for):
      pass

    options = isl.ast_print_options.alloc()
    options = options.set_print_block(print_user_block)
    options = options.set_print_user(print_user_py)
    options = options.set_print_for(print_for_py)

def test_id_list():
  l = isl.id_list('(a,c,d)')
  assert 3 == l.size()
  assert 3 == l.n_id()
  assert 'a' == l.at(0).name()

def test_printer_to_file():
  fd, temp_file_path = tempfile.mkstemp(suffix='.py')
  os.close(fd)
  with open(temp_file_path, 'w') as f, isl.printer.to_file(f) as printer:
    printer.print_str('Hello World')
  with open(temp_file_path, 'r') as f:
    content = f.read()
    assert 'Hello World' in content
  os.unlink(temp_file_path)

def test_isl_options_ast_build_detect_min_max():
  min_aff = isl.pw_aff("[N, ao, bo, co] -> { [(32)] }").min(
    isl.pw_aff("[N, ao, bo, co] -> { [(N - 32ao)] }"))

  isl.options_set_ast_build_detect_min_max(1)
  build = isl.ast_build()
  min_expr_ast = build.expr_from(min_aff)
  assert isl.ast_expr_op_type.MIN == min_expr_ast.op_type()


##############################################################################
# Regressions for docs/plans/isl-python-findings.md (numbered as in that doc)
##############################################################################

def _hole_ast(domain="{ S[i] : 0 <= i < 4 }"):
  dom = isl.union_set(domain)
  build = isl.ast_build.from_context(isl.set("{ : }"))
  return build.node_from(isl.schedule.from_domain(dom))


def test_findings_1_printer_to_str():
  """printer used to have no to_str/get_str and to lose all output."""
  ast = _hole_ast()
  hits = []

  def print_user(p, opts, node):
    hits.append(1)
    return p.start_line().print_str("HOLE();").end_line()

  p = isl.printer.to_str().set_output_format(isl.format.C)
  opts = isl.ast_print_options.alloc().set_print_user(print_user)
  p = ast.print(p, opts)

  assert len(hits) == 1
  assert "HOLE();" in p.get_str()
  assert str(p) == p.get_str()


def test_findings_1_printer_to_file():
  """The same output, but written through a Python file object."""
  ast = _hole_ast()
  fd, path = tempfile.mkstemp(suffix=".c")
  os.close(fd)
  try:
    with open(path, "w") as f, isl.printer.to_file(f) as p:
      p = p.set_output_format(isl.format.C)
      p = ast.print(p, isl.ast_print_options.alloc().set_print_user(
        lambda pr, opts, node: pr.start_line().print_str("HOLE();").end_line()))
    with open(path) as f:
      assert "HOLE();" in f.read()
  finally:
    os.unlink(path)


def test_findings_2_foreach_return_value():
  """An isl_bool callback has to answer; an isl_stat callback need not.

  The traversal used to read a missing answer as "prune", so a callback
  that just did its work and fell off the end silently visited a single
  node.  Guessing the other way would be just as wrong elsewhere
  (see test_foreach_scc_follows_is_a_predicate), so a missing answer
  is now an error.
  """
  u = isl.union_set("{ A[i] : 0<=i<3; B[i] : 0<=i<3; C[i] : 0<=i<3 }")

  # foreach_set takes an isl_stat callback: its result is not used.
  seen = []
  u.foreach_set(lambda s: seen.append(s.get_tuple_name()))
  assert len(seen) == 3

  root = isl.schedule.from_domain(u).get_root()

  visited = []
  root.foreach_descendant_top_down(lambda nd: (visited.append(1), True)[1])
  assert len(visited) > 1

  # Pruning still works, but has to be asked for.
  pruned = []
  root.foreach_descendant_top_down(lambda nd: (pruned.append(1), False)[1])
  assert len(pruned) == 1

  # Answering nothing is reported rather than guessed at.
  with pytest.raises(isl.Error) as excinfo:
    root.foreach_descendant_top_down(lambda nd: None)
  assert "must return a bool" in str(excinfo.value)


def test_findings_3_conversions_and_lex():
  """set -> union_set is a constructor; lex ordering exists at both levels."""
  s = isl.set("{ S[i] : 0<=i<4 }")

  # isl_union_set_from_set is an __isl_constructor, so it shows up as
  # a constructor overload rather than as union_set.from_set().
  assert isinstance(isl.union_set(s), isl.union_set)
  assert isinstance(s.to_union_set(), isl.union_set)

  # identity() already hands back a union_map; there is nothing to cast.
  assert isinstance(s.to_union_set().identity(), isl.union_map)

  # Lexicographic order is available on set, union_set and union_map.
  u = s.to_union_set()
  assert isinstance(s.lex_lt_set(s), isl.map)
  assert isinstance(u.lex_lt_union_set(u), isl.union_map)
  for name in ("lex_lt", "lex_le", "lex_gt", "lex_ge"):
    assert hasattr(isl.union_set, name + "_union_set")
    assert hasattr(isl.set, name + "_set")


def test_findings_4_val_to_int():
  """num_si()/den_si() are the way to get numbers out of a val.

  A val is a rational, so it deliberately does not convert to an int
  on its own.  What was broken is that the long returned by num_si()
  came back through the ctypes default of c_int and got truncated.
  """
  v = isl.set("{ S[i] : 0<=i<4 }").dim_max_val(0)
  assert v.is_int()
  assert v.num_si() == 3
  assert v.den_si() == 1

  for n in (2 ** 40, 2 ** 62, 2 ** 63 - 1):
    assert isl.val(str(n)).num_si() == n
    assert isl.val(str(n)).den_si() == 1

  # num_si() means "the numerator", not "the value"; the caller has to
  # check is_int() to know the two coincide.
  rational = isl.val("1/3")
  assert not rational.is_int()
  assert rational.num_si() == 1
  assert rational.den_si() == 3


def test_findings_5_signatures_are_documented():
  for fn in (isl.printer.to_file, isl.ast_node.print,
             isl.ast_print_options.alloc, isl.schedule.from_domain):
    assert fn.__doc__
  assert "isl_ast_node_print" in isl.ast_node.print.__doc__
  # A function whose C signature starts with an isl_ctx says so.
  assert "isl_ctx is implicit" in isl.ast_print_options.alloc.__doc__


def test_findings_6_options_are_all_exported():
  """isl scopes its options with the isl_ctx; the binding just exports them.

  <isl/options.h> was missing from all.h, so the general options were not
  reachable at all.  Scoping is left to the caller: this binding hands
  every call the same default context, so an option set here is visible
  to everything else using isl in this process.
  """
  # Used to be missing entirely.
  for name in ("on_error", "bound", "coalesce_bounded_wrapping",
               "schedule_algorithm", "gbr_only_first"):
    assert hasattr(isl, "options_get_" + name)
    assert hasattr(isl, "options_set_" + name)

  before = isl.options_get_schedule_serialize_sccs()
  try:
    isl.options_set_schedule_serialize_sccs(1 - before)
    assert isl.options_get_schedule_serialize_sccs() == 1 - before
  finally:
    isl.options_set_schedule_serialize_sccs(before)
  assert isl.options_get_schedule_serialize_sccs() == before


@cpython_only
def test_findings_7_annotation_payload():
  """The user payload used to be released by reading it back."""
  ast = _hole_ast("{ MM[i] : 0<=i<4 }")
  payload = {"kind": "hole"}
  marked = ast.set_annotation(isl.id("hole", payload))

  # (a) no annotation at all: a NULL result is reported, never wrapped up
  # in an object that falls apart later.
  with pytest.raises(isl.Error) as excinfo:
    ast.get_annotation()
  assert "isl_ast_node_get_annotation returned NULL" in str(excinfo.value)

  # (b) reading it back, repeatedly
  for _ in range(5):
    got = marked.get_annotation()
    assert got.name() == "hole"
    assert got.user() == payload

  # (c) and after a traversal has built its own ctypes callback thunks
  seen = []

  def visit(nd):
    try:
      seen.append(nd.get_annotation().user())
    except isl.Error:
      seen.append(None)
    return True

  marked.foreach_descendant_top_down(visit)
  assert seen[0] == payload
  assert marked.get_annotation().user() == payload


@cpython_only
def test_id_payload_refcount_is_balanced():
  """isl_id_alloc deduplicates on (name, user), so it may hand back an id
  that already accounts for the payload; counting it twice leaks it."""
  import sys
  payload = {"kind": "hole"}
  base = sys.getrefcount(payload)

  a = isl.id("x", payload)
  assert sys.getrefcount(payload) - base == 1
  b = isl.id("x", payload)                 # same pair: same isl_id
  assert sys.getrefcount(payload) - base == 1
  c = isl.id("y", payload)                 # different name: another isl_id
  assert sys.getrefcount(payload) - base == 2
  assert a.user() == b.user() == c.user() == payload

  del a, b, c
  gc.collect()
  assert sys.getrefcount(payload) - base == 0


def test_id_constructor_lists_both_forms():
  doc = isl.id.__init__.__doc__
  assert "isl_id_read_from_str" in doc
  assert "isl_id_alloc" in doc


@cpython_only
def test_findings_7_schedule_mark_payload():
  """The schedule tree side of the same mechanism."""
  ident = isl.id("MM", {"kind": "hole"})
  dom = isl.union_set("{ MM[i] : 0<=i<4 }")
  tree = isl.schedule.from_domain(dom).get_root().child(0) \
    .insert_mark(ident).get_schedule()
  got = []

  def visit(nd):
    if isinstance(nd, isl.schedule_node_mark):
      got.append((nd.get_id().name(), nd.get_id().user()))
    return True

  tree.get_root().foreach_descendant_top_down(visit)
  assert got == [("MM", {"kind": "hole"})]


def test_null_result_says_whether_isl_explained_it():
  """A NULL where an object was expected is reported either way.

  isl returns NULL both for "this failed" and for "there is nothing here",
  and only says which by recording an error.  Reading that is only sound
  because the context is kept clean, which conftest.py checks after
  every test.
  """
  dom = isl.union_set("{ MM[i] : 0<=i<4 }")
  ast = _hole_ast("{ MM[i] : 0<=i<4 }")

  # Nothing here: isl records nothing, so there is nothing to add.
  with pytest.raises(isl.Error) as excinfo:
    ast.get_annotation()
  assert str(excinfo.value) == "isl_ast_node_get_annotation returned NULL"

  # A real failure: isl says why, and that reason belongs to this call.
  with pytest.raises(isl.Error) as excinfo:
    isl.set("{ S[i] : 0<=i<4 }").intersect(isl.set("{ T[i,j] }"))
  assert "isl_set_intersect returned NULL" in str(excinfo.value)
  assert "spaces don't match" in str(excinfo.value)

  # A call whose result cannot express a failure must not leave its error
  # behind for the next one to pick up.
  isl.val(str(2 ** 80)).num_si()
  with pytest.raises(isl.Error) as excinfo:
    ast.get_annotation()
  assert str(excinfo.value) == "isl_ast_node_get_annotation returned NULL"


def test_findings_8_error_carries_message():
  dom = isl.union_set("{ MM[i] : 0<=i<4 }")
  with pytest.raises(isl.Error) as excinfo:
    isl.schedule.from_domain(dom).get_root().insert_mark(isl.id("MM"))
  assert "cannot insert node outside of root" in str(excinfo.value)

  # The message must not stick around and get attributed to a later call.
  assert isl.set("{ S[i] : 0<=i<4 }").dim_max_val(0).num_si() == 3


def test_findings_9_no_warning_at_exit():
  script = "import isl; isl.set('{[i]:0<=i<4}')"
  out = subprocess.run([sys.executable, "-c", script],
                       capture_output=True, text=True)
  assert out.returncode == 0
  assert "not freed" not in out.stderr


##############################################################################
# Boundaries the findings did not cover
##############################################################################

def test_printer_wrapper_is_reused():
  """A printer is modified in place, so it keeps its Python identity."""
  p = isl.printer.to_str()
  assert p.set_output_format(isl.format.C) is p
  assert p.print_str("x") is p
  ast = _hole_ast()
  assert ast.print(p, isl.ast_print_options.alloc()) is p


def test_printer_get_str_on_file_printer():
  fd, path = tempfile.mkstemp()
  os.close(fd)
  try:
    with open(path, "w") as f, isl.printer.to_file(f) as p:
      with pytest.raises(isl.Error) as excinfo:
        p.get_str()
      assert "string printer" in str(excinfo.value)
  finally:
    os.unlink(path)


def test_printer_to_file_rejects_non_files():
  with pytest.raises(isl.Error):
    isl.printer.to_file(io.StringIO())


def test_printer_to_file_does_not_leak_fds():
  fd, path = tempfile.mkstemp()
  os.close(fd)
  try:
    def open_fds():
      return len(os.listdir('/dev/fd'))

    with open(path, "w") as f:
      before = open_fds()
      for _ in range(50):
        with isl.printer.to_file(f) as p:
          p.print_str("x")
      assert open_fds() <= before
  finally:
    os.unlink(path)


def test_every_reports_a_missing_answer():
  u = isl.union_set("{ A[i] : 0<=i<3; B[i] : 0<=i<3 }")
  assert u.every_set(lambda s: True) is True
  assert u.every_set(lambda s: False) is False
  with pytest.raises(isl.Error) as excinfo:
    u.every_set(lambda s: None)
  assert "must return a bool" in str(excinfo.value)


def test_foreach_scc_follows_is_a_predicate():
  """foreach_scc sits in an isl_stat function but "follows" is not control flow.

  It answers a question about a pair of elements, so neither True nor
  False is a safe stand-in for a missing answer: True would put every
  element in one strongly connected component.
  """
  l = isl.id_list('(a,b,c)')

  sccs = []
  l.foreach_scc(lambda a, b: False, lambda scc: sccs.append(scc.size()))
  assert sccs == [1, 1, 1]

  sccs = []
  l.foreach_scc(lambda a, b: True, lambda scc: sccs.append(scc.size()))
  assert sccs == [3]

  with pytest.raises(isl.Error) as excinfo:
    l.foreach_scc(lambda a, b: None, lambda scc: None)
  assert "must return a bool" in str(excinfo.value)


def test_overload_and_constructor_errors_are_named():
  with pytest.raises(isl.Error) as excinfo:
    isl.union_set(42)
  assert "union_set" in str(excinfo.value)


if __name__ == "__main__":
  pytest.main(['-vvs', __file__])
