def assert_constants
  raise "TestCase wasn't exposed properly" unless defined? Gh96::StTestCase
  raise "TestCase wasn't exposed properly" unless defined? Gh96::Gh96::StTestCase
  raise "TestCase wasn't exposed properly" if defined? StTestCase
  raise "StTestCase doesn't report its Ruby name" unless Gh96::StTestCase.name == "Gh96::StTestCase"
  raise "StTestCase doesn't report its first Ruby name" unless Gh96::Gh96::StTestCase.name == "Gh96::StTestCase"
  raise "StTestCase exposure doesn't expose the same class" unless Gh96::Gh96::StTestCase == Gh96::StTestCase
end

def remove_constants
  Gh96::Gh96.remove_const :StTestCase
  Gh96.remove_const :StTestCase
  Gh96.remove_const :Gh96
  Object.remove_const :Gh96
end


Maglev.persistent do
  module Gh96
    expose_smalltalk_global_as("TestCase", "StTestCase")
    class Gh96
      expose_smalltalk_global_as("TestCase", "StTestCase")
    end
  end
  assert_constants
  Maglev.commit_transaction
  raise "Persistently exposed global wasn't committed to the stone, but should!" unless system("maglev-ruby -e 'Gh96::StTestCase' >/dev/null 2>/dev/null")
  remove_constants
end

module Gh96
  expose_smalltalk_global_as("TestCase", "StTestCase")
  class Gh96
    expose_smalltalk_global_as("TestCase", "StTestCase")
  end
end
assert_constants
Maglev.commit_transaction
raise "Transiently exposed global was committed to the stone, but shouldn't!" if system("maglev-ruby -e 'Gh96::StTestCase' >/dev/null 2>/dev/null")
remove_constants
