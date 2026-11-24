#!/bin/sh
# Run from the package root. Use PRISM_NATIVE_SANITIZERS=1 for ASan/UBSan.
set -eu
prism_tmpbase=${TMPDIR:-$PWD}
if [ ! -d "$prism_tmpbase" ] || [ ! -w "$prism_tmpbase" ]; then prism_tmpbase=$PWD; fi
prism_test_dir=$(mktemp -d "$prism_tmpbase/prismrepl-native.XXXXXX")
trap 'rm -rf "$prism_test_dir"' EXIT HUP INT TERM
prism_sanitizers=""
if [ "${PRISM_NATIVE_SANITIZERS:-0}" = "1" ]; then
  prism_sanitizers="-fsanitize=address,undefined -fno-omit-frame-pointer"
fi
${CC:-cc} -std=c99 -Wall -Wextra -Werror $prism_sanitizers -Isrc \
  tools/native/test-keyparse.c src/keyparse.c -o "$prism_test_dir/keyparse"
"$prism_test_dir/keyparse"
