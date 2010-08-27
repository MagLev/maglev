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

class Object
  def singleton_class
    class << self; self; end
  end
end

class Class
  def class_attribute(*attrs)
    instance_writer = !attrs.last.is_a?(Hash) || attrs.pop[:instance_writer]

    attrs.each do |name|
      class_eval <<-RUBY, __FILE__, __LINE__ + 1
        def self.#{name}() nil end
        def self.#{name}?() !!#{name} end

        def self.#{name}=(val)
          singleton_class.class_eval do
            remove_possible_method(:#{name})
            define_method(:#{name}) { val }
          end
        end

        def #{name}
          defined?(@#{name}) ? @#{name} : singleton_class.#{name}
        end

        def #{name}?
          !!#{name}
        end
      RUBY

      attr_writer name if instance_writer
    end
  end
end

class Module
  def remove_possible_method(method)
    remove_method(method)
  rescue NameError
  end
end

module ActiveModel
  module MassAssignmentSecurity
    extend ActiveSupport::Concern
    included do
      class_attribute :_protected_attributes
      class_attribute :_active_authorizer
    end

    module ClassMethods
      def active_authorizer
        self._active_authorizer ||= protected_attributes
      end

      def protected_attributes
        self._protected_attributes ||= X.new
      end
    end


    def sanitize_for_mass_assignment(attributes)
      a = mass_assignment_authorizer
      a.sanitize(attributes)
#      mass_assignment_authorizer.sanitize(attributes)
    end

    def mass_assignment_authorizer
      klass = self.class
      auth = klass.active_authorizer
      auth
#      self.class.active_authorizer
    end

  end
end

class Post
  include ActiveModel::MassAssignmentSecurity
end

class X
  def sanitize(*args)
    puts  "--- sanitize #{args.inspect}"
  end
end

post = Post.new
post.sanitize_for_mass_assignment({ })

