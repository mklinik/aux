#!/bin/sh
rm -rf sanitize_testcase
cp -r sanitize_testcase_org sanitize_testcase
find sanitize_testcase | ./sanitize.sh | sh
diff -brq sanitize_testcase sanitize_testcase_result
