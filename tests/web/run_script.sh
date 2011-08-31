#!/usr/bin/env sh
#
# Since maglev has neither fork/exec, nor spawn, we need to write a wrapper
# shell script to run this in the background and report the PID.
logfile="./testlog.out"
echo "\n\n=== $1   $(date) ===" >> $logfile
sh $1 >> $logfile 2>&1 &
echo $!
