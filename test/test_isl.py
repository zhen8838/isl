""" NOTE this file is translated from islpy:https://github.com/inducer/islpy/blob/main/test/test_isl.py  """

import pytest
import isl
import tempfile
import os

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

  with tempfile.NamedTemporaryFile(delete=False) as temp_file:
    temp_file_path = temp_file.name

  print(temp_file_path)
  printer = isl.printer.to_file_path(temp_file_path)
  printer = printer.set_output_format(isl.format.C)
  printer.print_str("// Start\n")
  printer = ast.print(printer, opts)
  printer.print_str("// End")
  printer.flush()

  with open(temp_file_path, "r") as f:
    print(f.read())


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
        temp_file_path = None
        with  tempfile.NamedTemporaryFile(delete=False) as temp_file:
          temp_file_path = temp_file.name
        p = isl.printer.to_file_path(temp_file_path)
        p = p.set_output_format(isl.format.C)
        p = p.print_ast_node(ast)
        p.flush()
        with open(temp_file_path, 'r') as f:
          return f.read()

    s = isl.set("[n,m] -> { [i,j] : 0 <= i <= n and i <= j <= m }")
    print(isl_ast_codegen(s))


def test_id_constructor():
    x0 = isl.id("x0")
    x01 = isl.id("x0")
    assert x0.name() == x01.name()


def test_creation_error():
    # note the (intentional) syntax error
    # with pytest.raises(isl.Error):
    try:
      x = isl.basic_set(
              "[n0, n1] -> "
              "{ [i0, i1, i2] : 0 <= i0 < n1  and 0 and 0 <= i2 <= 15 }")
      print(x)
    except:
      print("got isl error")


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
  print(os.getpid())
  fd, temp_file_path = tempfile.mkstemp(suffix='.py')
  with open(temp_file_path, 'w') as f:
    printer = isl.printer.to_file(f)
    printer = printer.print_str('Hello World')
    printer.flush()
  with open(temp_file_path, 'r') as f:
    content = f.read()
    print(content)
    assert 'Hello World' in content

if __name__ == "__main__":
  pytest.main(['-vvs', __file__])
