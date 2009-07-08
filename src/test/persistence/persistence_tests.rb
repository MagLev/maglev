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

    ########################################
    # Persistence Tests methods
    ########################################
    def test_001
      Maglev::PERSISTENT_ROOT[:hat] = "A New Hat"
    end

    def check_001
      test(Maglev::PERSISTENT_ROOT[:hat], "A New Hat", :check_001)
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
    end

    def run_checks
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

end
Maglev.commit_transaction
