#!/usr/bin/env bash
source_init "$1"
run_before "$1"

KT_OUT="$TMP_DIR/$(basename "$1" .lean).kt"

if [[ -n "${TEST_EXPECT_FAIL:-}" ]]; then
  capture_only "$1" \
    lean -K "$KT_OUT" -Dcompiler.postponeCompile=false ${TEST_LEAN_ARGS[@]+"${TEST_LEAN_ARGS[@]}"} "$1"
  check_exit_is_fail
else
  run_only lean -K "$KT_OUT" -Dcompiler.postponeCompile=false ${TEST_LEAN_ARGS[@]+"${TEST_LEAN_ARGS[@]}"} "$1"
  check_exit_is_success
  capture_only "$1" cat "$KT_OUT"
  check_exit_is_success
fi

normalize_measurements
check_out_file

if command -v kotlinc &>/dev/null && [[ -f "$KT_OUT" && -z "${TEST_EXPECT_FAIL:-}" ]]; then
  kotlinc -Werror -nowarn -d "$TMP_DIR" "$KT_OUT" || fail "kotlinc failed to compile generated Kotlin code"
fi

run_after "$1"
