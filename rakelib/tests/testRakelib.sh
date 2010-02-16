# TODO: Need to write real test case for this.
# I created it so I could make sure the messages were consistent.
# Use bash -v testRakelib.sh and look through output for consistency

# First create stone test.rakelib unless it already exists
conf_exists=0
[ -f $MAGLEV_HOME/etc/conf.d/test.rakelib.conf ] && conf_exists=1
[ $conf_exists -eq 0 ] && rake stone:create[test.rakelib]

# Test that test.rakelib shows up
rake stone:list

# Test startup
rake test.rakelib:start

# Test global status while running
rake

# Test local status while running
rake test.rakelib:status

# Test we are actually using the stone we expect
maglev-ruby --stone test.rakelib -e 'puts "test.rakelib should == #{Maglev::System.stone_name}"'

# Test startup while running
rake test.rakelib:start

# Test restart while running
rake test.rakelib:restart
rake test.rakelib:status

# Test reload while running
rake test.rakelib:reload    
rake test.rakelib:status

# Test snapshot while running
rake test.rakelib:take_snapshot
rake test.rakelib:status

# Test restore while running
rake test.rakelib:restore_snapshot
rake test.rakelib:status

# Test restart from stopped state
rake test.rakelib:stop
rake test.rakelib:restart
rake test.rakelib:status

# Test shutdown
rake test.rakelib:stop

#==== while stopped ===
# None of these should restart the stone

# Test global status while stopped
rake

# Test local status while stopped
rake test.rakelib:status

# Test reload while stopped
rake test.rakelib:reload    
rake test.rakelib:status

# Test snapshot while stopped
rake test.rakelib:take_snapshot
rake test.rakelib:status

# Test restore while stopped
rake test.rakelib:restore_snapshot
rake test.rakelib:status

#==== NetLDI ===

# Test starting a NetLDI
rake netldi:start

# Test stopping a NetLDI
rake netldi:stop

#==== Parse Server ===

# Test starting parser (needs MRI 1.8.6p287 and ParseTree 3.0.3)
rake parser:start
rake parser:status 

# Test stopping parser
rake parser:stop
rake parser:status

# Finally run destroy test.rakelib if it didn't exist when we started
[ $conf_exists -eq 0 ] && rake stone:destroy[test.rakelib]
