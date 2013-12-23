#!/bin/sh
ghc sanitize.hs || exit 0
rm -rf sanitize_testcase
cp -r sanitize_testcase_org sanitize_testcase
./sanitize sanitize_testcase
diff -brq sanitize_testcase sanitize_testcase_result
