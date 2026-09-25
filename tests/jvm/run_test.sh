#!/usr/bin/env bash
source_init "$1"
run_before "$1"

capture_only "$1" \
  lean --run "$1"

normalize_measurements
check_out_file
check_exit_is_success

RUNTIME_JAR="../../runtime/jvm/build/lean-runtime.jar"
if [[ ! -f "$RUNTIME_JAR" ]] && command -v javac &>/dev/null; then
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
esac

run_after "$1"
