TEST_EXPECT_FAIL=1
TEST_LEAN_ARGS=("-Dcompiler.kotlin.preamble=class Buffer(var size: Int, var capacity: Int) {
// @LeanMembers(Buffer)
}")
