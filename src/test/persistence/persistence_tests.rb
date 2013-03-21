$:.unshift File.expand_path(File.dirname(__FILE__))

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

    # Test nested Maglev.persistent and Maglev.transient blocks and the
    # Maglev.persistent? and Maglev.transient? predicates.
    def test_000
      # Quick check to make sure the env is setup for the test
      raise "Expecting transient mode" unless Maglev.transient?


      Maglev.persistent do
        test(Maglev.persistent?, true, "000 a: Maglev.persistent?")
        test(Maglev.transient?, false, "000 a: Maglev.transient?")

        Maglev.transient do
          test(Maglev.persistent?, false, "000 b: Maglev.persistent?")
          test(Maglev.transient?, true, "000 b: Maglev.transient?")

          Maglev.persistent do
            test(Maglev.persistent?, true, "000 c: Maglev.persistent?")
            test(Maglev.transient?, false, "000 c: Maglev.transient?")

            Maglev.persistent do
              test(Maglev.persistent?, true, "000 d: Maglev.persistent?")
              test(Maglev.transient?, false, "000 d: Maglev.transient?")

              Maglev.transient do
                test(Maglev.persistent?, false, "000 e: Maglev.persistent?")
                test(Maglev.transient?, true, "000 e: Maglev.transient?")

                Maglev.transient do
                  test(Maglev.persistent?, false, "000 f: Maglev.persistent?")
                  test(Maglev.transient?, true, "000 f: Maglev.transient?")
                end
              end
            end
          end
        end
      end
    end

    def check_000
      # Nothing
    end

    def test_001
      Maglev::PERSISTENT_ROOT[:hat] = "A New Hat"
      test(Maglev::PERSISTENT_ROOT[:hat], "A New Hat", '001 test: hat')
    end

    def check_001
      test(Maglev::PERSISTENT_ROOT[:hat], "A New Hat", '001 check: hat')
    end

    def test_002
      # Tests for: track 553
      #
      # PTestPersistentConstant and PTestTransientConstant are set
      # below, this method makes sure const_set works correctly in
      # both transient and persistent modes
      Maglev.transient do
        Object.const_set('PTestTransientConstant', 33)
      end
      Maglev.persistent do
        Object.const_set('PTestPersistentConstant', 44)
      end

      test(PTestTransientConstant,  33, "002 a: PTestTransientConstant")
      test(PTestPersistentConstant, 44, "002 a: PTestPersistentConstant")
    end

    def check_002
      test(defined? PTestTransientConstant,  nil, "002 b: PTestTransientConstant")
      test(PTestPersistentConstant,  44, "002 b: PTestPersistentConstant2")
    end


    def test_003
      # Tests for: https://magtrac.gemstone.com/ticket/599
      #
      # Test that a persisted class has its constants, instance variables
      # and class variables saved.
      Maglev.persistent do
        require 't003'
      end
      Maglev.commit_transaction
    end

    def check_003
      test(C003::XYZ,                   45, "003: C003::XYZ")
      test(C003.class_variable,         22, "003: Class variable")
      test(C003.class_instance_variable, 2, "003: Class instance variable")
      test(C003.new.instance_variable,   3, "003: Class instance variable")
      test(C003.maglev_persistable?,  true, "003: C003.maglev_persistable?")
    end

    def test_004
      require 't004'  # Commits class C004 with methods
      # Now remove the methods

      Maglev.persistent do
        C004.remove_method(:im_one)
        class << C004; remove_method(:cm_one); end
      end
      Maglev.commit_transaction

      c = C004.new

      test(C004.respond_to?(:cm_one), false, 'test_004 a: cm_one not there')
      test(C004.cm_two, :self_cm_two, 'test_004 a: cm_two still here')
      test(C004.cm_three, :self_cm_three, 'test_004 a: cm_three still here')

      test(c.respond_to?(:im_one), false, 'test_004 a: im_one not there')
      test(c.im_two, :im_two, 'test_004 a: im_two still here')
      test(c.im_three, :im_three, 'test_004 a: im_three still here')

      #      Maglev.transient do
      #        C004.remove_method(:im_three) # raises exception

      #         class << C004; remove_method(:cm_three); end
      #       end
      #       Maglev.commit_transaction # should be no-op, but just testing...

      #       test(C004.respond_to?( :cm_one), false, 'test_004 b: cm_one not there')
      #       test(C004.cm_two, :self_cm_two, 'test_004 b: cm_two still here')
      #       test(C004.respond_to?(:cm_three), 'test_004 b: cm_three still here')

      #       test(c.respond_to?(:im_one), false, 'test_004 b: im_one not there')
      #       test(c.im_two, :im_two, 'test_004 b: im_two still here')
      #       test(c.respond_to?(:im_three), false, 'test_004 b: im_three not there')
    end

    def check_004
      c = C004.new

      test(C004.respond_to?(:cm_one), false, 'check_004 a: cm_one not there')
      test(C004.cm_two, :self_cm_two, 'check_004 a: cm_two still here')
      test(C004.cm_three, :self_cm_three, 'check_004 a: cm_three still here')

      test(c.respond_to?(:im_one), false, 'check_004 a: im_one not there')
      test(c.im_two, :im_two, 'check_004 a: im_two still here')
      test(c.im_three, :im_three, 'check_004 a: im_three still here')
    end

    def test_005
      require 't005'
      check_005  # the checks are valid both in this VM and the next
    end

    def check_005
      employees = Maglev::PERSISTENT_ROOT[:employees]
      test(employees.size, 3, "005: check right number of employees")
      employees.each do |employee|
        test(employee.name.nil?, false,  "005: Bad name for #{employee}")
        test(employee.salary > 0, true,  "005: Bad salary for #{employee}")
      end
    end

    def test_006
      # Use Case from the persistence-api doc

      # Assume Maglev::PERSISTENT_ROOT[:maybe] is nil:
      test(Maglev::PERSISTENT_ROOT[:maybe], nil, "006: :maybe initially nil")

      # Stage an object for persistence
      s = "I want to be persistent...but..."
      Maglev::PERSISTENT_ROOT[:maybe] = s

      # create a local variable that will be unaffected by the
      # abort_transaction:
      $clueless = "Yup"

      test(Maglev::PERSISTENT_ROOT[:maybe], s, "006: :maybe value before abort")
      test($clueless, "Yup", "006: $clueless value before abort")

      Maglev.abort_transaction

      # At this point, the state of Maglev::PERSISTENT_ROOT is reset to the
      # default value but local variables are unaffected

      test(Maglev::PERSISTENT_ROOT[:maybe], nil, "006: :maybe value after abort")
# TODO: This currently fails...
#      test($clueless, "Yup", "006: $clueless value after abort")

      #Maglev::PERSISTENT_ROOT[:maybe] # => nil
      #$clueless # => "Yup"
    end

    def check_006
      # Null  all checks are in test_006
    end

    def test_007
      require 't007'
    end

    def check_007
      test(C007::A_CONST, 1, "007: A_CONST")
      test(defined?(C007::A_NON_PERSISTENT_CONST), nil, "007: A_NON_PERSISTENT_CONST")
      test(C007::A_SECOND_PERSISTENT_CONST, 53, "007: A_SECOND_PERSISTENT_CONST")
      c = C007.new
      test(c.respond_to?(:a_persistent_method), true, "007: C007#a_persistent_method")
      test(c.respond_to?(:an_ambiguous_method), true, "007: C007#an_ambiguous_method")
      test(c.respond_to?(:a_non_persistent_method), false, "007: C007#a_non_persistent_method")
    end

    # Basic test that Module#maglev_persistable= and
    # Module#maglev_persitable? work
    #
    def test_008
      require 't008'
    end

    def check_008
      foos = Maglev::PERSISTENT_ROOT[:my_favorite_foos]
      test(foos.size, 1, '008: Maglev::PERSISTENT_ROOT[:my_favorite_foos].size')
      test(Foo456.maglev_persistable?, true, '008: Foo456.maglev_persistable?')
    end

#     # Test that wrapping Maglev.persistent and Maglev.transient around a
#     # require works
#     def test_009
#     end
#     def check_009
#     end

    # https://magtrac.gemstone.com/ticket/553  Trac 553
    def test_010
      require 't010'
      test(X10, 123, '010 test: X10')
      test(X20, 678, '010 test: X20')
    end

    def check_010
      # New VM should not see the transient value
      test(X10, 123, '010 check: X10')
      test(X20, 345, '010 check: X20')
    end

    # https://magtrac.gemstone.com/ticket/552  Trac 552
    def test_011
      Maglev.persistent do
        require 't011_trac552'
      end
      Maglev.commit_transaction
      test(C.new.m_meth, 'M', '011 test: m_meth')
      test(C.new.methods.grep(/^m_meth$/), ['m_meth'], '011 test: C methods')
      test(M.methods.grep(/^m_meth$/), [], '011 test: M methods')
    end

    def check_011
      test(C.new.m_meth, 'M', '011 check: m_meth')
      test(C.new.methods.grep(/^m_meth$/), ['m_meth'], '011 check: C methods')
      test(M.methods.grep(/^m_meth$/), [], '011 check: M methods')
    end

    def test_012
      # Tests for autoload
      Maglev.persistent do
        require 't012'
      end
      Maglev.commit_transaction
    end

    def check_012
      # Double check that the module got committed
      test(M012.class, Module, '012 check: Reference M012')
      # Check that the autoload setup in test_012 is active: i.e.,
      # reference the constant and expect the file to loaded now.
      test(M012::Builder.class, Class, '012 check: Reference autoload association M012::Builder')

      # TODO: Check both a transient and persistent reference to an autoload
    end

    def test_013
      require 't013_trac595.rb'
      test(1, 1, "t013_595 passes if no exception raised during require")
    end
    def check_013
      # Nothing to check
    end

    def test_014
      Maglev.transient do
        require 't014.rb'
      end
    end

    def check_014
      test(M014.method_defined?(:m014), false, "maglev_persistable does not cause methods to be persisted")
    end

    def test_015
      Maglev.transient do
        require 't015.rb'
      end
    end

    def check_015
      test(M015.method_defined?(:m015_1), true, "maglev_persistable(true) causes methods to be persisted")
      test(M015.method_defined?(:m015_2), true, "maglev_persistable(true) causes methods to be persisted, even afterwards.")
    end

    def test_016
      Maglev.transient do
        require 't016.rb'
      end
      test(T016.included_modules.include?(M016), true, "Included module is accessible via klass.included_modules.")
    end

    def check_016
      test(T016.included_modules.include?(M016), true, "maglev_persistable(true) persists included modules.")
    end

    def test_017
      Maglev.transient do
        require 't017.rb'
      end
    end

    def check_017
      test(T017.respond_to?(:t017_c), true, "maglev_persistable(true) persists class methods")
    end

    def test_018
      Maglev.transient do
        require 't018.rb'
      end
      test(T018.respond_to?(:t018_c), true, "maglev_persistable(true) persists class methods extended from a module")
    end

    def check_018
      test(T018.respond_to?(:t018_c), true, "maglev_persistable(true) persists class methods extended from a module")
    end


    ########################################
    # Test Framework Methods
    ########################################
    def initialize
      @failed = []
      @count = 0
    end

    def run_tests
      puts "== In Maglev.persistent block..."
      self.methods.grep(/^test_/).sort.each do |m|
        puts "== Running: #{m}: Maglev.transient?: #{Maglev.transient?}"
        send m
      end
      report
    end

    def run_checks
      self.methods.grep(/^check_/).sort.each do |m|
        puts "== Checking: #{m}:  Maglev.transient?: #{Maglev.transient?}"
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
      @failed << "ERROR: #{msg}. Expected: #{expected.inspect} actual: #{actual.inspect}"
      puts @failed
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
