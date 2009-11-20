require 'maglev/ruby_compiler'

require 'maglev/repository'

# TODO: Move these to bootstrap
class Object
  # We use _becomeMinimalChecks: because self is on the stack (call to
  # method_missing, and the call to the original method that triggered the
  # method_missing), and become: doesn't allow that.
  #
  # TODO: Still need to check for indexes
  primitive_nobridge 'become', '_becomeMinimalChecks:'
  def migrate_from(other)
    # default migration implementation just copies existing instance
    # variables.
    other.instance_variables.each do |name|
      sym = name.to_s
      val = other.instance_variable_get(sym)
      instance_variable_set(sym, val)
    end
  end
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
    class MigrationFailed < RuntimeError
      attr_reader :old_class, :new_class, :original_exception
      def initialize(msg, old_class, new_class, root=nil)
        super(msg)
        @old_class = old_class
        @new_class = new_class
        @original_exception = root
      end
    end

    # Migrate to a new version of Class or Module.  The new class / module
    # definition is in the string +ruby_code+.  +klass+ is a string or
    # symobl that represents the fully qualified class name of the Class or
    # Module to migrate (e.g., 'Maglev::Foo').  +migrate_instances+ is a
    # boolean that controls whether or not to migrate old instances to new
    # instances.  If +migrate_instances+ is true, then the new class
    # definition should define a migrate_from method(old_instance), which
    # will be called during the migration to migrate to the new instance.
    #
    # Returns the version of klass in exsitence at the start of the method
    # call (the "old" version).  This is useful in case the ruby_code
    # commits fine (replacing klass with the new version), but the
    # migration fails.  You can then reissue a migration call.
    #
    # This method calls Maglev.abort_transaction and
    # Maglev.commit_transaction.
    #
    # TODO:
    # * What about other classes/modules in the file?
    # * What if klass is not in file? (should allow data migrations)
    # * What about nested classes etc.
    # * How do you migrate an entire inheritance hierarchy?
    # * What about classes defined programatically in the file?
    # * What about singleton classes...the migrate_from method needs to deal with it?
    def migrate(klass, ruby_code, migrate_instances=true)
      Maglev.abort_transaction

      old_class = remove_from_parent klass if migrate_instances

      compiler = RubyCompiler.new

      Maglev.persistent { compiler.compile ruby_code }
      new_class = get_path(klass)[-1]
      # TODO: should we abort if we are going to raise this?
      raise "Migrate: Can't find new version of #{klass}" unless new_class
      Maglev.commit_transaction

      migrate_instances(old_class, new_class) if migrate_instances
      return old_class
    end
    module_function :migrate

    # Migrate instances of old_class to new_class.  This does an abort and a commit.
    def migrate_instances(old_class, new_class)
      instances = Maglev::Repository.instance.list_instances([old_class])[0]
      # TODO: What to do if the new class does not define a migrate_from?
      instances.each do |old_instance|
        new_instance = new_class.allocate
        new_instance.migrate_from old_instance
        new_instance.become old_instance
      end
      Maglev.commit_transaction
    rescue Exception => e
      m = "Migration failed from #{old_class.name} => #{new_class.name}"
      raise MigrationFailed.new(m, old_class, new_class, e)
    end
    module_function :migrate_instances

    # Migrate to a new version of Class or Module.  The new class / module
    # definition is in the string +ruby_code+.  +klass+ is a string or
    # symobl that represents the fully qualified class name of the Class or
    # Module to migrate (e.g., 'Maglev::Foo').  This method calls
    # Maglev.abort_transaction and Maglev.commit_transaction.
    #
    # Instances will permanently migrate, only if there is a commit.  This
    # code does not execute a per-instance commit.  It is up to the code
    # calling a potential old method to do the commit.
    #
    # This code does a commit (to commit the changes to effect lazy
    # migration).
    #
    # TODO:
    # * What about other classes/modules in the file?
    # * What if klass is not in file? (should allow data migrations)
    # * What about nested classes etc.
    # * How do you migrate an entire inheritance hierarchy?
    # * What about classes defined programatically in the file?
    def migrate_lazily(old_class_name, new_class_name, ruby_code)
      Maglev.abort_transaction

      old_class = remove_from_parent old_class_name

      compiler = RubyCompiler.new
      Maglev.persistent { compiler.compile ruby_code }
      new_class = get_path(new_class_name)[-1]
      raise "Migrate: Can't find new version of #{new_class_name}" unless new_class
      Maglev.commit_transaction # instance migration needs new txn context
      skip_methods = ['instance_variable_get', 'method_missing', 'puts']
      mclass = class << old_class; self end

      # Setup old class so that any method send, will trigger the
      # migration, and then resend on the new instance.
      Maglev.persistent do
        # If the instances have singleton classes, then those will still be
        # active under the current scheme....
        #
        # Don't override the class methods.???
        #
        old_class.module_eval do
          @@_target_class = new_class
          #puts "Set #{self}'s target class to #{new_class}"

          def method_missing(name, *args, &blk)
            new_instance = @@_target_class.allocate
            new_instance.migrate_from self
            new_instance.become self
            # At this point, self and new_instance have swapped identities, i.e.,
            # new_instance now points to the old instance. We want to execute
            # the method on the new instance, which is now self...
            self.send(name, *args, &blk)
          end
        end

        # Now we undefine all the methods we can on the class.  Use
        # undef_method so we don't search superclasses.  We'll need a few
        # methods during migration, so we skip a few.
        old_class.instance_methods(false).each { |m|
          unless skip_methods.include? m
            #puts "-- #{old_class.inspect}: undef_method  #{m.inspect}  (#{m.to_sym})"
            old_class.send(:undef_method, m.to_sym)
          end
        }
        # TODO: protect any methods needed by migration....
        mclass.instance_methods(true).each { |m|
          #puts "-- #{self} undef_method (class) #{m.inspect}"
          mclass.undef_method m.to_sym
        }
      end
      Maglev.commit_transaction
    end
    module_function :migrate_lazily

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
        acc << parent.const_get(el.to_sym) unless el.empty?
        acc
      end
      result.shift if result[0] == result[1] # If klass is 'Object' or '::Object'
      result
    end
    module_function :get_path
  end
end
