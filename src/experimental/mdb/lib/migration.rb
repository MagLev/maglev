module Maglev

  # The Migration module provides support for managing versions of a class
  # file.  Services include
  # * upload a new version of a class file, install the class as a fresh
  #   instance and migrate instances.
  #
  # == Known issues with migration
  #
  # 1. If application code maintains references to Class objects (e.g., in
  #    a Hash or Array), the application is responsible for updating the
  #    class references to the new class.  One way around this is if the
  #    application refers to the classes by name, rather than by reference.
  #
  module Migration

    # Migrate to a new version of Class or Module.  The new class / module
    # definition is in file.  +klass+ is a string or symobl that represents
    # the fully qualified class name of the Class or Module to migrate
    # (e.g., 'Maglev::Foo').
    #
    #
    # TODO:
    # * What about other classes/modules in the file?
    # * What if klass is not in file? (should allow data migrations)
    # * What about nested classes etc.
    # * How do you migrate an entire inheritance hierarchy?
    # * What about classes defined programatically in the file?
    def migrate(klass, file)
    end

    # Remove the Class or Module named +path_to_class+ from its parent's
    # namespace.  +path_to_class+ is a string or symbol for the path to a
    # class or module, e.g., 'Object::Foo::Bar', or "Foo::Bar".
    # Raises NameError if +path_to_class+ does not name a constant.
    def remove_from_parent(path_to_class)
      path = get_path path_to_class
      target = path.pop
      parent = path.pop || Object
      raise "#{path_to_class} not a class or module" unless Module === target
      raise "#{parent.inspect} (parent of #{path_to_class}) is not a class or module" unless Module === parent
      parent.send :remove_const, target.name.to_sym
      # TODO: Check for Object etc?
    end
    module_function :remove_from_parent

    # Walk the path +klass+ and return the class or module referenced by
    # it.  +klass+ is a string or symbol like 'Foo::Quux'.  Returns an
    # array of the namespaces down to klass, e.g., [Object, Foo, Quux].
    def get_path(klass)
      result = klass.to_s.split('::').inject([Object]) do |acc,el|
        parent = acc[-1]
        raise NameError unless Module === parent
        acc << parent.const_get(el) unless el.empty?
        acc
      end
      result.shift if result[0] == result[1] # If klass is 'Object' or '::Object'
      result
    end
    module_function :get_path

  end
end
