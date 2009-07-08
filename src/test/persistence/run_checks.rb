# This script assumes persistence_tests.rb and run_tests.rb have been run.
#
unless defined? PTEST
  puts "WARNING: Persistence tests have NOT already been run on this stone...."
end

PTEST.run_checks
