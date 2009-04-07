# file   common/misc.rb    no longer used.

#  RecursionGuard is implemented in bootstrap/RecursionGuard.rb

#  Do not use Rubinius implementation of metaclass .
#  any non-Singleton object for which it is invoked
#  will have a singleton class added, which is very inefficient.
#
#  class Object
#    # Rubinius uses metaclass() in several files
#    def metaclass
#      class << self;self;end
#    end
#    alias_method :__metaclass__, :metaclass
#  end
