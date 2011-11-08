def assert_constants
  raise "RcKeyValueDictionary wasn't exposed properly" unless defined? Gh96::RCHash
  raise "RcKeyValueDictionary wasn't exposed properly" unless defined? Gh96::Gh96::RCHash
  raise "RcKeyValueDictionary wasn't exposed properly" if defined? RCHash
  raise "RCHash doesn't report its Ruby name" unless Gh96::RCHash.name == "Gh96::RCHash"
  raise "RCHash doesn't report its first Ruby name" unless Gh96::Gh96::RCHash.name == "Gh96::RCHash"
  raise "RCHash exposure doesn't expose the same class" unless Gh96::Gh96::RCHash == Gh96::RCHash
end

def remove_constants
  Gh96::Gh96.remove_const :RCHash
  Gh96.remove_const :RCHash
  Gh96.remove_const :Gh96
  Object.remove_const :Gh96
end


Maglev.persistent do
  module Gh96
    expose_smalltalk_global_as("RcKeyValueDictionary", "RCHash")
    class Gh96
      expose_smalltalk_global_as("RcKeyValueDictionary", "RCHash")
    end
  end
  assert_constants
  Maglev.commit_transaction
  raise "Persistently exposed global wasn't committed to the stone, but should!" unless system("maglev-ruby -e 'Gh96::RCHash' >/dev/null 2>/dev/null")
  remove_constants
end

module Gh96
  expose_smalltalk_global_as("RcKeyValueDictionary", "RCHash")
  class Gh96
    expose_smalltalk_global_as("RcKeyValueDictionary", "RCHash")
  end
end
assert_constants
Maglev.commit_transaction
raise "Transiently exposed global was committed to the stone, but shouldn't!" if system("maglev-ruby -e 'Gh96::RCHash' >/dev/null 2>/dev/null")
remove_constants
