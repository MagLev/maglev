#  This file adds entries in the Ruby session methods dictionary,
#    i.e. the session methods dictionary for environment 1.
#  The added entries will have keys being Ruby mangled selectors, 
#  and values being Smalltalk methods already in the base server image.
#
#  The ruby method "primitive"  takes one or two String arguments
#     The first arg is an un-mangled Ruby selector, it will be mangled by
#        numArgs timesRepeat: [rubySel := rubySel copyWith: $: ]
#     where numArgs comes from the method found in the base server image.
#  The one exception is methods which take block args, which should include a
#  "&" at the end to indicate this.
#
#     The second arg is the Smalltalk selector.
#     If second arg is not present is is assumed to be exactly the first arg.
#
#     The receiver is a Class in which the session method 
#     will be installed, and it is assumed that for that Class,
#     Ruby and Smalltalk classes are identical.  The Smalltalk method
#     installed will be obtained by sending lookupSelector: to
#     the Smalltalk Class which is identical to the receiver.
#
#     The implementation of "primitive" uses 
#       Behavior>>addRubySelector:method:category:

#
# Bootstrap
#
RUBY.class.primitive 'require', 'requireFileNamed:'
RUBY.class.primitive 'load', 'loadFileNamed:'
RUBY.class.primitive 'global', 'installGlobal:'
RUBY.class.primitive 'global', 'installGlobal:name:'

def require(name)
    RUBY.require(name)
end

def load(name)
  RUBY.load(name)
  true
end

require 'kernel/bootstrap/Globals.rb'

#
# Core
#
require 'kernel/core/Object.rb'
require 'kernel/core/Class.rb'
require 'kernel/core/Fixnum.rb'
require 'kernel/core/Integer.rb'
require 'kernel/core/Float.rb'
require 'kernel/core/Numeric.rb'
require 'kernel/core/Boolean.rb'
require 'kernel/core/Proc.rb'
require 'kernel/core/Array.rb'
require 'kernel/core/Hash.rb'
require 'kernel/core/Range.rb'
require 'kernel/core/String.rb'
require 'kernel/core/Symbol.rb'
require 'kernel/core/Regexp.rb'
require 'kernel/core/Exception.rb'
require 'kernel/core/File.rb'
require 'kernel/core/Env.rb'
require 'kernel/core/Time.rb'
require 'kernel/core/IO.rb'
require 'kernel/core/Socket.rb'
require 'kernel/core/Comparable.rb'
require 'kernel/core/Set.rb'
require 'kernel/core/Method.rb'
require 'kernel/core/Process.rb'
require 'kernel/core/ThrowCatch.rb'
require 'kernel/core/GC.rb'
