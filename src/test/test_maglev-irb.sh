#! /bin/bash
#
# Quick kluge to use for testing maglev-irb
#
# Take files from vmunit.conf, turn them into a shell script
# which passes them via STDIN to maglev-irb.
# nohup that script and look through the results for "Maybe IRB bug"
# save the results in irb_bad.txt

awk -f create_irb_test.awk vmunit.conf > test_irb_tmp.sh
chmod 755 test_irb_tmp.sh
./test_irb_tmp.sh > test_irb.out
egrep "^Maybe|^==>" test_irb.out | awk -f extract_irb_bad.awk > irb_bad.out
