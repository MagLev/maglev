require 'maglev/ruby_compiler'

class Object
  primitive_nobridge 'become', 'become:'
end

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
    # definition is in the string +ruby_code+.  +klass+ is a string or
    # symobl that represents the fully qualified class name of the Class or
    # Module to migrate (e.g., 'Maglev::Foo').  This method calls
    # Maglev.abort_transaction and Maglev.commit_transaction.
    #
    # TODO:
    # * What about other classes/modules in the file?
    # * What if klass is not in file? (should allow data migrations)
    # * What about nested classes etc.
    # * How do you migrate an entire inheritance hierarchy?
    # * What about classes defined programatically in the file?
    def migrate(klass, ruby_code, klass_exists=true)
      Maglev.abort_transaction

      old_class = remove_from_parent klass if klass_exists

      compiler = RubyCompiler.new
      Maglev.persistent { compiler.compile ruby_code }
      new_class = get_path(klass)[-1]
      puts "-- old_class:  #{old_class.inspect} (#{old_class.__id__})"
      puts "-- new_class:  #{new_class.inspect} (#{new_class.__id__})"
      puts "-- new_class methods: #{new_class.methods(false).inspect}"
      raise "Migrate: Can't find new version of #{klass}" unless new_class
      Maglev.commit_transaction # instance migration needs new txn context

      if klass_exists
        instances = Repository.instance.list_instances([old_class])[0]
        puts "-- Migrating #{instances.size} instances of #{klass.inspect}"
        # TODO: What to do if the new class does not define a migrate_from?
        instances.each do |old_instance|
          new_instance = new_class.allocate
          new_instance.migrate_from old_instance
          new_instance.become old_instance
        end
        Maglev.commit_transaction
      end
    end
    module_function :migrate

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
      Maglev.persistent { parent.send :remove_const, target.name.to_sym }
      # TODO: Check for Object etc?
      # TODO: Commit here?
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
