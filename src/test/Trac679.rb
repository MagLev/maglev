# From Rails3
#
# Maglev gives:
#  $ maglev-ruby src/test/TracXXX.rb
#  error , NoMethodError: undefined method `_validators' for Foo,
#            during /Users/pmclain/GemStone/checkouts/git/src/test/TracXXX.rb
#  ERROR 2010, NoMethodError: undefined method `_validators' for Foo (NoMethodError)
#

module ActiveSupport
  module Concern
    def self.extended(base)
      base.instance_variable_set("@_dependencies", [])
    end

    def append_features(base)
      if base.instance_variable_defined?("@_dependencies")
        base.instance_variable_get("@_dependencies") << self
        return false
      else
        if base < self
          return false 
        end
        @_dependencies.each { |dep| base.send(:include, dep) }
        super
        base.extend const_get("ClassMethods") if const_defined?("ClassMethods")
        base.send :include, const_get("InstanceMethods") if const_defined?("InstanceMethods")
        if instance_variable_defined?("@_included_block")
          base.class_eval(&@_included_block) 
        end
      end
    end

    def included(base = nil, &block)
      if base.nil?
        @_included_block = block
      else
        super
      end
    end
  end
end

class Class
  def class_attribute(*attrs)
    s = singleton_class
    attrs.each do |attr|
      s.send(:define_method, attr) { }
      # s.send(:define_method, "#{attr}?") { !!send(attr) }
      s.send(:define_method, "#{attr}=") do |value|
        singleton_class.send(:define_method, attr) { value }
      end
    end
  end
end

class Object
  # Returns the object's singleton class.
  def singleton_class
    class << self
      self
    end
  end unless respond_to?(:singleton_class)
end

module Validations
  extend ActiveSupport::Concern

  included do
    class_attribute( :_validators)
    self._validators = Hash.new { |h,k| h[k] = [] }
  end

  module ClassMethods
    def validates_with
      _validators[:foo] << 679
    end
    def validates_each(*attr_names, &block)
      validates_with
    end
  end
end

ax = class Foo
  include Validations
  attr_accessor :first_name
  validates_each :first_name do |record, attr, value|
    puts "validating"
  end
end

unless ax == [ 679 ] ; raise 'error'; end
true
