module Gh96
  expose_smalltalk_global_as("RcKeyValueDictionary", "RCHash")
  class Gh96
    expose_smalltalk_global_as("RcKeyValueDictionary", "RCHash")
  end
end

raise "RcKeyValueDictionary wasn't exposed properly" unless defined? Gh96::RCHash
raise "RcKeyValueDictionary wasn't exposed properly" unless defined? Gh96::Gh96::RCHash
raise "RcKeyValueDictionary wasn't exposed properly" if defined? RCHash
raise "RCHash doesn't report its Ruby name" unless Gh96::RCHash.name == "RCHash"
raise "RCHash doesn't report its Ruby name" unless Gh96::Gh96::RCHash.name == "RCHash"
raise "RCHash exposure doesn't expose the same class" unless Gh96::Gh96::RCHash == Gh96::RCHash
