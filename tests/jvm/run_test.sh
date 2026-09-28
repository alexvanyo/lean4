#!/usr/bin/env bash
source_init "$1"
run_before "$1"

capture_only "$1" \
  lean --run "$1"

normalize_measurements
check_out_file
check_exit_is_success

RUNTIME_JAR="../../runtime/kmp/build/libs/lean-runtime.jar"
NEED_BUILD=0
if [[ ! -f "$RUNTIME_JAR" ]]; then
  NEED_BUILD=1
else
  for f in $(find ../../runtime/kmp/src -name "*.kt"); do
    if [[ "$f" -nt "$RUNTIME_JAR" ]]; then
      NEED_BUILD=1
      break
    fi
  done
fi

if [[ "$NEED_BUILD" -eq 1 ]]; then
  (cd ../../runtime/kmp && ./gradlew standaloneRuntimeJar --quiet)
fi

TEST_BASE=$(basename "$1" .lean)

case "$TEST_BASE" in
  classfile_encoding)
    if [[ -f "SimpleMath.class" ]] && command -v javap &>/dev/null; then
      javap -c -p SimpleMath.class > /dev/null || fail "javap failed to parse generated SimpleMath.class"
      rm -f SimpleMath.class
    fi
    ;;
  bytecode_exec)
    if [[ -f "lean/test/ExecTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ExecTest)
      rm -f lean/test/ExecTest.class
      if [[ "$OUTPUT" != "42" ]]; then
        fail "ExecTest failed: expected '42', got '$OUTPUT'"
      fi
    fi
    ;;
  closure_chain)
    if [[ -f "lean/test/ClosureTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ClosureTest)
      rm -f lean/test/ClosureTest.class
      if [[ "$OUTPUT" != "Closure currying and over-application verified" ]]; then
        fail "ClosureTest failed: expected 'Closure currying and over-application verified', got '$OUTPUT'"
      fi
    fi
    ;;
  tailrec_loop)
    if [[ -f "lean/test/TailRecTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.TailRecTest)
      rm -f lean/test/TailRecTest.class
      if [[ "$OUTPUT" != "5050" ]]; then
        fail "TailRecTest failed: expected '5050', got '$OUTPUT'"
      fi
    fi
    ;;
  inductive_tree)
    if [[ -f "lean/test/TreeTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.TreeTest)
      rm -f lean/test/TreeTest.class
      if [[ "$OUTPUT" != "2" ]]; then
        fail "TreeTest failed: expected '2', got '$OUTPUT'"
      fi
    fi
    ;;
  big_ctor)
    if [[ -f "lean/test/BigCtorTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.BigCtorTest)
      rm -f lean/test/BigCtorTest.class
      if [[ "$OUTPUT" != "42" ]]; then
        fail "BigCtorTest failed: expected '42', got '$OUTPUT'"
      fi
    fi
    ;;
  array_test)
    if [[ -f "lean/test/ArrayTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'#[0, 1, 2, 3]\n4\n2'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ArrayTest)
      rm -f lean/test/ArrayTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "ArrayTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  tuple_test)
    if [[ -f "lean/test/TupleTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.TupleTest)
      rm -f lean/test/TupleTest.class
      if [[ "$OUTPUT" != "6" ]]; then
        fail "TupleTest failed: expected '6', got '$OUTPUT'"
      fi
    fi
    ;;
  strict_and_or)
    if [[ -f "lean/test/StrictAndOrTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'false\ntrue\ntrue\ntrue\nfalse\nfalse\nfalse\ntrue'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.StrictAndOrTest)
      rm -f lean/test/StrictAndOrTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "StrictAndOrTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  uset_test)
    if [[ -f "lean/test/USetTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.USetTest)
      rm -f lean/test/USetTest.class
      if [[ "$OUTPUT" != "42" ]]; then
        fail "USetTest failed: expected '42', got '$OUTPUT'"
      fi
    fi
    ;;
  thunk_test)
    if [[ -f "lean/test/ThunkTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ThunkTest)
      rm -f lean/test/ThunkTest.class
      if [[ "$OUTPUT" != "42" ]]; then
        fail "ThunkTest failed: expected '42', got '$OUTPUT'"
      fi
    fi
    ;;
  string_ops)
    if [[ -f "lean/test/StringOpsTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'hello world\n11\nhello'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.StringOpsTest)
      rm -f lean/test/StringOpsTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "StringOpsTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  large_closure)
    if [[ -f "lean/test/LargeClosureTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.LargeClosureTest)
      rm -f lean/test/LargeClosureTest.class
      if [[ "$OUTPUT" != "155" ]]; then
        fail "LargeClosureTest failed: expected '155', got '$OUTPUT'"
      fi
    fi
    ;;
  direct_compile)
    if [[ -f "lean/test/SampleModule.class" ]] && command -v javap &>/dev/null; then
      javap -p lean/test/SampleModule.class | grep -q "f_addOne" || fail "f_addOne method missing from SampleModule.class"
      javap -p lean/test/SampleModule.class | grep -q "f_pairSwap" || fail "f_pairSwap method missing from SampleModule.class"
      rm -f lean/test/SampleModule.class lean/test/SampleModule.lean
    fi
    ;;
  apply_m_overapp)
    if [[ -f "lean/test/ApplyOverappTest.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ApplyOverappTest)
      rm -f lean/test/ApplyOverappTest.class lean/test/ChainClosure.class
      if [[ "$OUTPUT" != "42" ]]; then
        fail "ApplyOverappTest failed: expected '42', got '$OUTPUT'"
      fi
    fi
    ;;
  uint_fold)
    if [[ -f "lean/test/UIntFoldTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'12760\n12720\n11\n6\n44'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.UIntFoldTest)
      rm -f lean/test/UIntFoldTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "UIntFoldTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  bytearray_ops)
    if [[ -f "lean/test/ByteArrayOpsTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'2\n10\n42'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ByteArrayOpsTest)
      rm -f lean/test/ByteArrayOpsTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "ByteArrayOpsTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  reusebug)
    if [[ -f "lean/test/ReuseBugTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'2\n0\n2'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ReuseBugTest)
      rm -f lean/test/ReuseBugTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "ReuseBugTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  prelude_resolution)
    if [[ -f "lean/test/PreludeResolutionTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'1\n1\n0\n5\n16\n123\n1'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.PreludeResolutionTest)
      rm -f lean/test/PreludeResolutionTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "PreludeResolutionTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  nat_shiftr)
    if [[ -f "lean/test/NatShiftTest.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'16\n1\n2\n1\n0'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.NatShiftTest)
      rm -f lean/test/NatShiftTest.class
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "NatShiftTest failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  closure_curry_chain)
    if [[ -f "lean/test/ClosureCurryRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ClosureCurryRunner)
      rm -f lean/test/ClosureCurryRunner.class lean/mod_l_lean_test_ClosureCurryModule*.class lean/test/ClosureCurryModule.lean
      if [[ "$OUTPUT" != "70" ]]; then
        fail "ClosureCurryRunner failed: expected '70', got '$OUTPUT'"
      fi
    fi
    ;;
  closure_capture_ctor)
    if [[ -f "lean/test/ClosureCaptureCtorRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ClosureCaptureCtorRunner)
      rm -f lean/test/ClosureCaptureCtorRunner.class lean/mod_l_lean_test_ClosureCaptureCtorModule*.class lean/test/ClosureCaptureCtorModule.lean
      if [[ "$OUTPUT" != "37" ]]; then
        fail "ClosureCaptureCtorRunner failed: expected '37', got '$OUTPUT'"
      fi
    fi
    ;;
  closure_in_loop)
    if [[ -f "lean/test/ClosureInLoopRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ClosureInLoopRunner)
      rm -f lean/test/ClosureInLoopRunner.class lean/mod_l_lean_test_ClosureInLoopModule*.class lean/test/ClosureInLoopModule.lean
      if [[ "$OUTPUT" != "15" ]]; then
        fail "ClosureInLoopRunner failed: expected '15', got '$OUTPUT'"
      fi
    fi
    ;;
  expr_diff)
    if [[ -f "lean/test/ExprDiffRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ExprDiffRunner)
      rm -f lean/test/ExprDiffRunner.class lean/mod_l_lean_test_ExprDiffModule*.class lean/test/ExprDiffModule.lean
      if [[ "$OUTPUT" != "5" ]]; then
        fail "ExprDiffRunner failed: expected '5', got '$OUTPUT'"
      fi
    fi
    ;;
  tree_map)
    if [[ -f "lean/test/TreeMapRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.TreeMapRunner)
      rm -f lean/test/TreeMapRunner.class lean/mod_l_lean_test_TreeMapModule*.class lean/test/TreeMapModule.lean
      if [[ "$OUTPUT" != "430" ]]; then
        fail "TreeMapRunner failed: expected '430', got '$OUTPUT'"
      fi
    fi
    ;;
  lazylist_fib)
    if [[ -f "lean/test/LazyListRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.LazyListRunner)
      rm -f lean/test/LazyListRunner.class lean/mod_l_lean_test_LazyListModule*.class lean/test/LazyListModule.lean
      if [[ "$OUTPUT" != "13" ]]; then
        fail "LazyListRunner failed: expected '13', got '$OUTPUT'"
      fi
    fi
    ;;
  qsort_test)
    if [[ -f "lean/test/QSortRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.QSortRunner)
      rm -f lean/test/QSortRunner.class lean/mod_l_lean_test_QSortModule*.class lean/test/QSortModule.lean
      if [[ "$OUTPUT" != "#[1, 2, 3, 4, 5]" ]]; then
        fail "QSortRunner failed: expected '#[1, 2, 3, 4, 5]', got '$OUTPUT'"
      fi
    fi
    ;;
  list_append)
    if [[ -f "lean/test/ListAppendRunner.class" ]] && command -v java &>/dev/null; then
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.ListAppendRunner)
      rm -f lean/test/ListAppendRunner.class lean/mod_l_lean_test_ListAppendModule*.class lean/test/ListAppendModule.lean
      if [[ "$OUTPUT" != "2000" ]]; then
        fail "ListAppendRunner failed: expected '2000', got '$OUTPUT'"
      fi
    fi
    ;;
  float_ops)
    if [[ -f "lean/test/FloatOpsRunner.class" ]] && command -v java &>/dev/null; then
      EXPECTED="3.000000|-1.000000|6.000000|1.500000|8.000000|false|true|false|true|false|true|0.000000|42.000000|-42.000000|255|65535|4294967295|true|true|true|2.333333|3.500000|[1.500000, 2.000000]|true"
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.FloatOpsRunner)
      rm -f lean/test/FloatOpsRunner.class lean/mod_l_lean_test_FloatOpsModule*.class lean/test/FloatOpsModule.lean
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "FloatOpsRunner failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  char_escape)
    if [[ -f "lean/test/CharEscapeRunner.class" ]] && command -v java &>/dev/null; then
      EXPECTED="4|4|4|1|97|98|99"
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.test.CharEscapeRunner)
      rm -f lean/test/CharEscapeRunner.class lean/mod_l_lean_test_CharEscapeModule*.class lean/test/CharEscapeModule.lean
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "CharEscapeRunner failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  module_init)
    if [[ -f "lean/mod_l_lean_test_ModuleInitModule.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'started the program\nhello world\n30\n#[hello, world, foo]'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.mod_l_lean_test_ModuleInitModule)
      rm -f lean/mod_l_lean_test_ModuleInitModule*.class lean/test/ModuleInitModule.lean
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "ModuleInit failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
  module_init_unboxed)
    if [[ -f "lean/mod_l_lean_test_ModuleInitUnboxedModule.class" ]] && command -v java &>/dev/null; then
      EXPECTED=$'0\nfalse\n1\n0.500000\n16'
      OUTPUT=$(java -cp .:"$RUNTIME_JAR" lean.mod_l_lean_test_ModuleInitUnboxedModule)
      rm -f lean/mod_l_lean_test_ModuleInitUnboxedModule*.class lean/test/ModuleInitUnboxedModule.lean
      if [[ "$OUTPUT" != "$EXPECTED" ]]; then
        fail "ModuleInitUnboxed failed: expected '$EXPECTED', got '$OUTPUT'"
      fi
    fi
    ;;
esac

rmdir lean/test lean 2>/dev/null || true

run_after "$1"
