# TODO: Need to write real test case for this.
# I created it so I could make sure the messages were consistent.
# Use bash -v testRakelib.sh and look through output for consistency

# First run `rake stone:create[aaa]` unless aaa already exists

# Test that aaa shows up
rake stone:list

# Test startup
rake aaa:start

# Test global status while running
rake

# Test local status while running
rake aaa:status

# Test startup wile running
rake aaa:start

# Test restart while running
rake aaa:restart
rake aaa:status

# Test reload while running
rake aaa:reload    
rake aaa:status

# Test snapshot while running
rake aaa:take_snapshot
rake aaa:status

# Test restore while running
rake aaa:restore_snapshot
rake aaa:status

# Test restart from stopped state
rake aaa:stop
rake aaa:restart
rake aaa:status

# Test shutdown
rake aaa:stop

#==== while stopped ===
# None of these should restart the stone

# Test global status while stopped
rake

# Test local status while stopped
rake aaa:status

# Test reload while stopped
rake aaa:reload    
rake aaa:status

# Test snapshot while stopped
rake aaa:take_snapshot
rake aaa:status

# Test restore while stopped
rake aaa:restore_snapshot
rake aaa:status

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

# Finally run `rake stone:destroy[aaa]`
