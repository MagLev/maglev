$:.unshift File.expand_path(File.dirname(__FILE__))
# This script assumes persistence_tests.rb has been run.
#
if defined? PTEST
  puts "WARNING: Persistence tests have already been run on this stone...."
end

Maglev.persistent do
  PTEST = MagLevPersistenceTests.new
  Maglev.commit_transaction
end

# run_tests creates its own Maglev.persistent block to run the tests
# and commits after the tests are done.
PTEST.run_tests
