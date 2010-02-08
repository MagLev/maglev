# test_irb.awk
# turns vmunit.conf into a shell script that runs its tests through maglev-irb
/^#/ {
    print
    next
}

{ print "echo '==> Running " $1 "'; maglev-irb -f < '" $1 "'" }
