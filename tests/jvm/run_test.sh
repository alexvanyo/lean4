#!/usr/bin/env bash
source_init "$1"
run_before "$1"

capture_only "$1" \
  lean --run "$1"

normalize_measurements
check_out_file
check_exit_is_success

RUNTIME_JAR="../../runtime/jvm/build/lean-runtime.jar"
NEED_BUILD=0
if [[ ! -f "$RUNTIME_JAR" ]]; then
  NEED_BUILD=1
else
  for f in ../../runtime/jvm/src/lean/runtime/*.java; do
    if [[ "$f" -nt "$RUNTIME_JAR" ]]; then
      NEED_BUILD=1
      break
    fi
  done
fi

if [[ "$NEED_BUILD" -eq 1 ]] && command -v javac &>/dev/null; then
  mkdir -p "$(dirname "$RUNTIME_JAR")/classes"
  javac -d "$(dirname "$RUNTIME_JAR")/classes" ../../runtime/jvm/src/lean/runtime/*.java
  jar cf "${RUNTIME_JAR}.tmp.$$" -C "$(dirname "$RUNTIME_JAR")/classes" .
  mv -f "${RUNTIME_JAR}.tmp.$$" "$RUNTIME_JAR" 2>/dev/null || true
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
esac

run_after "$1"
