Maglev.persistent do
  module GlobalsTests
    def test_globals_001
      # No-op setup is for a global
    end

    def check_globals_001
      test(defined? FooPersistent, true, "GlobalsTests: FooPersistent")
    end

    def test_globals_002
      # No-op setup is for a global
    end

    def check_globals_002
      test(defined? FooTransient, false, "GlobalsTests: FooTransient")
    end
  end

  FooPersistent = true
end

Maglev.transient do
  FooTransient = true
end

Maglev.commit_transaction
