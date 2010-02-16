# These are ruby utilities that mirror rb_* methods from the MRI source.
# They are used in various libraries, not necessarily from the kernel.

module Maglev
  module RubyUtils
    # rb_path2class from Variable.c
    #
    # Given the path to a class, return the class.  E.g.,
    # path might be ::Foo::Bar
    def rb_path2class(path)
      if path[0] == "#"
        raise ArgumentError, "can't retrieve anonymous class #{path}"
      end

      ns = Object
      path.to_s.split('::').each do |n|
        next if n.empty?
        if ns.const_defined?(n)
          ns = ns.const_get(n)
        else
          raise ArgumentError, "undefined class/module #{n}"
        end
        case ns
        when Class, Module
          # ok
        else
          raise TypeError, "#{ns} in #{path} does not refer to class/module"
        end
      end
      ns
    end
    module_function :rb_path2class

  end
end
