##############################################################################
# Code from Rails
##############################################################################

# rails/activesupport/lib/active_support/core_ext/module/aliasing.rb
class Module
  def alias_method_chain(target, feature)
    # Strip out punctuation on predicates or bang methods since
    # e.g. target?_without_feature is not a valid method name.
    aliased_target, punctuation = target.to_s.sub(/([?!=])$/, ''), $1
    yield(aliased_target, punctuation) if block_given?

    with_method, without_method = "#{aliased_target}_with_#{feature}#{punctuation}", "#{aliased_target}_without_#{feature}#{punctuation}"

    alias_method without_method, target
    alias_method target, with_method

    case
      when public_method_defined?(without_method)
        public target
      when protected_method_defined?(without_method)
        protected target
      when private_method_defined?(without_method)
        private target
    end
  end

  def alias_attribute(new_name, old_name)
    module_eval <<-STR, __FILE__, __LINE__+1
      def #{new_name}; self.#{old_name}; end          # def subject; self.title; end
      def #{new_name}?; self.#{old_name}?; end        # def subject?; self.title?; end
      def #{new_name}=(v); self.#{old_name} = v; end  # def subject=(v); self.title = v; end
    STR
  end
end

# rails/activesupport/lib/active_support/concern.rb
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
        return false if base < self
        @_dependencies.each { |dep| base.send(:include, dep) }
        super
        base.extend const_get("ClassMethods") if const_defined?("ClassMethods")
        base.send :include, const_get("InstanceMethods") if const_defined?("InstanceMethods")
        base.class_eval(&@_included_block) if instance_variable_defined?("@_included_block")
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

##############################################################################
# END Code from Rails
##############################################################################

module Optimistic
  extend ActiveSupport::Concern

  included do
    alias_method_chain :update, :lock
  end

  private
  def update_with_lock
    update_without_lock + '_with_lock'
  end
end

class Foo
  def update
    'update'
  end
end
      

class Foo
  include Optimistic
end


p Foo.new.update
