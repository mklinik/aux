#!/bin/sh
rm -rf sanitize_testcase
cp -r sanitize_testcase_org sanitize_testcase
runhaskell sanitize.hs sanitize_testcase
diff -brq sanitize_testcase sanitize_testcase_result
