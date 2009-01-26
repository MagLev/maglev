module Haml
  def self.version
    puts "Testing @@version.."
    return @@version if defined?(@@version)
    puts "Setting @@version"  # MagLev does not get here
    @@version = { :string => "foo" }
  end
  puts "Calling Haml.version"
  VERSION = version[:string]
end
puts Haml::VERSION
