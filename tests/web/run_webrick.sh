#!/usr/bin/env sh
#
# Since maglev has neither fork/exec, nor spawn, we need to write a wrapper
# shell script to run this in the background and report the PID.
maglev-ruby time_server.rb > webrick.out 2>&1 &
echo $!
