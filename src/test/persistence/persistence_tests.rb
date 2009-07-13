$:.unshift File.expand_path(File.dirname(__FILE__))
puts "$: at top of file: #{$:.inspect}"
# Until Trac # 552 is fixed, just code all of the tests in this file.
# Later, after 552 is fixed, we can break them apart a bit.
#
#require 'globals_tests'
#Maglev.abort_transaction  # Pick up any commits during the requires


Maglev.persistent do

  # This class defines a small unit test framework, and it defines many
  # test method pairs.  A single persistence test is broken into two
  # methods: test_NNN() and check_NNN().  The test_NNN methods each setup
  # some persistent test data, and the check_NNN tests that the data was
  # saved. The test_NNN methods are run, in order, in a fresh VM within a
  # Maglev.persistent block (by calling MagLevPersistenceTests#run_tests).
  # The check_NNN methods are then run, in order, in a new VM, so they
  # should see the persistent changes done by the first VM.
  #
  # This file just defines the class.  See the driver script for more
  # details.

  class MagLevPersistenceTests
    #include GlobalsTests

    ########################################
    # Persistence Tests methods
    ########################################
    def test_001
      Maglev::PERSISTENT_ROOT[:hat] = "A New Hat"
    end

    def check_001
      test(Maglev::PERSISTENT_ROOT[:hat], "A New Hat", :check_001)
    end

#  test_002 waiting on track 553
#
#     def test_002
#       # PTestPersistentConstant and PTestTransientConstant are set
#       # below, this method makes sure const_set works correctly in
#       # both transient and persistent modes
#       Maglev.transient do
#         Object.const_set('PTestTransientConstant2', 33)
#       end
#       Maglev.persistent do
#         Object.const_set('PTestPersistentConstant2', 44)
#       end

#       test(PTestPersistentConstant, true, "PTestPersistentConstant")
#       test(PTestPersistentConstant2,  44, "PTestPersistentConstant2")

#       test(PTestTransientConstant,  true, "PTestTransientConstant")
#       test(PTestTransientConstant2,   33, "PTestTransientConstant2")
#     end

#     def check_002
#       test(PTestPersistentConstant, true, "PTestPersistentConstant")
#       test(PTestPersistentConstant2,  44, "PTestPersistentConstant2")

#       test(defined? PTestTransientConstant,  nil, "PTestTransientConstant")
#       test(defined? PTestTransientConstant2,  nil, "PTestTransientConstant2")
#     end

    def test_003
      # Test that a persisted class has its constants, instance variables
      # and class variables saved.
      puts "$: in test_003: #{$:.inspect}"
      require 't003'
    end

    def check_003
      test(C003.foo, "foo", "Class variable")
      test(C003.bar, "bar", "Class instance variable")
    end

    ########################################
    # Test Framework Methods
    ########################################
    def initialize
      @failed = []
      @count = 0
    end

    def run_tests
      Maglev.persistent do
        puts "== In Maglev.persistent block..."
        self.methods.grep(/^test_/).sort.each do |m|
          puts "== Running: #{m}"
          send m
        end
        Maglev.commit_transaction
        puts "== Maglev.commit_transaction"
      end
      report
    end

    def run_checks
      puts "======================="
      p self.methods
      puts "======================="
      self.methods.grep(/^check_/).sort.each do |m|
        puts "== Checking: #{m}"
        send m
      end
      report
    end

    def test(actual, expected, msg)
      @count += 1
      register_failure(msg, expected, actual) unless (expected == actual)
    end

    def report
      num = @failed.size
      puts "=== Ran #{@count} tests.  Failed: #{@failed.size}"
      @failed.each { |f| puts f }
      raise "Failed #{@failed.size} tests" unless @failed.empty?
      true
    end

    def failed_test(msg, expected, actual)
      @count += 1
      register_failure(msg, expected, actual)
    end

    def register_failure(msg, expected, actual)
      @failed << "ERROR: #{msg} Expected: #{expected.inspect} actual: #{actual.inspect}"
      x = @failed
      unless ENV['SIMPLE_NO_PAUSE']  # don't pause if the env says not to...
        nil.pause if defined? RUBY_ENGINE # Keep MRI from trying to pause
      end
    end

  end

  PTestPersistentConstant = true  # A top level persistent constant
end

Maglev.transient do
  PTestTransientConstant = true  # A top level transient constant
end

Maglev.commit_transaction
