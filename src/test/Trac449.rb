# Incorrect: returns nil, not the singleton class
oc = class << Object; end
puts "should be nil: #{oc.inspect}"
unless oc.equal?(nil) ; raise 'error'; end
unless oc.is_a?(Object) ; raise 'error'; end
if oc.is_a?(Class) ; raise 'error'; end
if oc.is_a?(Module) ; raise 'error'; end

puts "---"
# Correct: Does return the singleton class
oc2 = class << Object; self; end
puts "should be #<Class:Object>: #{oc2}"
if RUBY_VERSION == "1.8.7"
  className = ""
elsif RUBY_VERSION == "1.9.3"
  className = Object.name
end

unless (xx = oc2.name) == className ; raise "name was: #{xx.inspect} but expected: #{className.inspect}"; end 
unless (xx = oc2.inspect) == '#<Class:Object>' ; raise "inspect was: #{xx.inspect}"; end
unless oc2.is_a?(Object) ; raise 'error'; end
unless oc2.is_a?(Class) ; raise 'error'; end
unless oc2.is_a?(Module) ; raise 'error'; end
puts "Done"
#################### Trac Info
# ID:         449
# Summary:    MetaClass being used when it should be a Ruby class
# Changetime: 2009-09-30 16:14:31+00:00
###

#  Somehow MetaClass is being sent a ruby method when it should be Class.  In Ruby the class hierarchy is Class as the class of Class and there should be no way to get to MetaClass.
#  
#  96 Exception >> _executeHandler:            (envId 0) @3 line 8
#  97 Exception >> signal                      (envId 0) @1 line 1
#  98 Exception >> signal:                     (envId 0) @3 line 7
#  99 Kernel >> raise::                        (envId 1) @4 line 3
#  100 Kernel >> method_missing:*               (envId 1) @7 line 4
#  101 Object >> _doesNotUnderstand:args:envId: (envId 0) @16 line 22
#  102 Metaclass >> _compileClass               (envId 1) @30 line 46
#  103 [] in  RubyCompiler >> extend:rubyMethod: (envId 0) @14 line 11
#  104 ExecBlock >> ensure:                     (envId 0) @2 line 10
#  105 RubyCompiler >> extend:rubyMethod:       (envId 0) @7 line 12
#  106 Base class >> _compileClass              (envId 1) @15 line 92
#  
#  102 Metaclass >> _compileClass               (envId 1) @30 line 46
#      receiver [105905153  Metaclass]   Base class
#  topaz 1> list
#         class << self
#           # Deprecated and no longer has any effect.
#           def allow_concurrency
#             ActiveSupport::Deprecation.warn("ActiveRecord::Base.allow_concurrency has been deprecated and no longer has any effect. Please remove all references to allow_concurrency.")
#           end
#     
#           # Deprecated and no longer has any effect.
#           def allow_concurrency=(flag)
#             ActiveSupport::Deprecation.warn("ActiveRecord::Base.allow_concurrency= has been deprecated and no longer has any effect. Please remove all references to allow_concurrency=.")
#           end
#     
#           # Deprecated and no longer has any effect.
#           def verification_timeout
#             ActiveSupport::Deprecation.warn("ActiveRecord::Base.verification_timeout has been deprecated and no longer has any effect. Please remove all references to verification_timeout.")
#           end
#     
#           # Deprecated and no longer has any effect.
#           def verification_timeout=(flag)
#             ActiveSupport::Deprecation.warn("ActiveRecord::Base.verification_timeout= has been deprecated and no longer has any effect. Please remove all references to verification_timeout=.")
#           end
#     
#           # Returns the connection currently associated with the class. This can
#           # also be used to "borrow" the connection to do database work unrelated
#           # to any of the specific Active Records.
#           def connection
#             retrieve_connection
#           end
#     
#           def connection_pool
#             connection_handler.retrieve_connection_pool(self)
#           end
#     
#           def retrieve_connection
#             connection_handler.retrieve_connection(self)
#           end
#     
#           # Returns true if +ActiveRecord+ is connected.
#           def connected?
#             connection_handler.connected?(self)
#           end
#     
#           def remove_connection(klass = self)
#             connection_handler.remove_connection(klass)
#           end
#     
#           delegate :clear_active_connections!, :clear_reloadable_connections!,
#   *  ^30                                                                       
#     
#     # method starts at line 92 of file /Users/lattam/Projects/MagLev/working/maglev_test/vendor/rails/activerecord/lib/active_record/connection_adapters/abstract/connection_specification.rb 
#  
#  
#  
#  106 Base class >> _compileClass              (envId 1) @15 line 92
#      receiver [105905409  Base class]  Base
#  topaz 1> list
#     puts "Loading connection_specification.rb"
#     
#     module ActiveRecord
#       class Base
#         class ConnectionSpecification #:nodoc:
#           attr_reader :config, :adapter_method
#           def initialize (config, adapter_method)
#             @config, @adapter_method = config, adapter_method
#           end
#         end
#     
#         ##
#         # :singleton-method:
#         # The connection handler
#         cattr_accessor :connection_handler, :instance_writer => false
#         @@connection_handler = ConnectionAdapters::ConnectionHandler.new
#     
#         # Returns the connection currently associated with the class. This can
#         # also be used to "borrow" the connection to do database work that isn't
#         # easily done without going straight to SQL.
#         def connection
#           self.class.connection
#         end
#     
#         # Establishes the connection to the database. Accepts a hash as input where
#         # the <tt>:adapter</tt> key must be specified with the name of a database adapter (in lower-case)
#         # example for regular databases (MySQL, Postgresql, etc):
#         #
#         #   ActiveRecord::Base.establish_connection(
#         #     :adapter  => "mysql",
#         #     :host     => "localhost",
#         #     :username => "myuser",
#         #     :password => "mypass",
#         #     :database => "somedatabase"
#         #   )
#         #
#         # Example for SQLite database:
#         #
#         #   ActiveRecord::Base.establish_connection(
#         #     :adapter => "sqlite",
#         #     :database  => "path/to/dbfile"
#         #   )
#         #
#         # Also accepts keys as strings (for parsing from YAML for example):
#         #
#         #   ActiveRecord::Base.establish_connection(
#         #     "adapter" => "sqlite",
#         #     "database"  => "path/to/dbfile"
#         #   )
#         #
#         # The exceptions AdapterNotSpecified, AdapterNotFound and ArgumentError
#         # may be returned on an error.
#         def self.establish_connection(spec = nil)
#           case spec
#             when nil
#               raise AdapterNotSpecified unless defined? RAILS_ENV
#               establish_connection(RAILS_ENV)
#             when ConnectionSpecification
#               @@connection_handler.establish_connection(name, spec)
#             when Symbol, String
#               if configuration = configurations[spec.to_s]
#                 establish_connection(configuration)
#               else
#                 raise AdapterNotSpecified, "#{spec} database is not configured"
#               end
#             else
#               spec = spec.symbolize_keys
#               unless spec.key?(:adapter) then raise AdapterNotSpecified, "database configuration does not specify adapter" end
#     
#               begin
#                 require 'rubygems'
#                 gem "activerecord-#{spec[:adapter]}-adapter"
#                 require "active_record/connection_adapters/#{spec[:adapter]}_adapter"
#               rescue LoadError
#                 begin
#                   require "active_record/connection_adapters/#{spec[:adapter]}_adapter"
#                 rescue LoadError
#                   raise "Please install the #{spec[:adapter]} adapter: `gem install activerecord-#{spec[:adapter]}-adapter` (#{$!})"
#                 end
#               end
#     
#               adapter_method = "#{spec[:adapter]}_connection"
#               if !respond_to?(adapter_method)
#                 raise AdapterNotFound, "database configuration specifies nonexistent #{spec[:adapter]} adapter"
#               end
#     
#               remove_connection
#               establish_connection(ConnectionSpecification.new(spec, adapter_method))
#           end
#         end
#     
#         class << self
#   * ^15                                                                *******
#     
#     # method starts at line 1 of file /Users/lattam/Projects/MagLev/working/maglev_test/vendor/rails/activerecord/lib/active_record/connection_adapters/abstract/connection_specification.rb 
#  
#  
#  