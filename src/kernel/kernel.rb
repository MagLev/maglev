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
#     The ruby method "primitive" is installed into env 1 by
#         RubyContext>>installPrimitiveBootstrap
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
require 'kernel/bootstrap/GlobalErrors.rb'
require 'kernel/bootstrap/Gemstone.rb'

#
# bootstrap
#
require 'kernel/bootstrap/Type.rb'
require 'kernel/bootstrap/Kernel.rb'
require 'kernel/bootstrap/Module.rb'
require 'kernel/bootstrap/Behavior.rb'
require 'kernel/bootstrap/Class.rb'
require 'kernel/bootstrap/Object.rb'

require 'kernel/bootstrap/GsHelper.rb'

require 'kernel/bootstrap/Fixnum.rb'
require 'kernel/bootstrap/Integer.rb'
require 'kernel/bootstrap/Float.rb'
require 'kernel/bootstrap/Numeric.rb'
require 'kernel/bootstrap/Boolean.rb'
require 'kernel/bootstrap/Proc.rb'
require 'kernel/bootstrap/Array.rb'
require 'kernel/bootstrap/Hash.rb'
require 'kernel/bootstrap/String.rb'
require 'kernel/bootstrap/Symbol.rb'
require 'kernel/bootstrap/Regexp.rb'
require 'kernel/bootstrap/MatchData.rb'
require 'kernel/bootstrap/Exception.rb'

require 'kernel/bootstrap/Env.rb'
require 'kernel/bootstrap/Errno.rb'
require 'kernel/bootstrap/Dir.rb'
require 'kernel/bootstrap/File.rb'

require 'kernel/bootstrap/Range.rb'

require 'kernel/bootstrap/Time.rb'
require 'kernel/bootstrap/IO.rb'
require 'kernel/bootstrap/Socket.rb'
require 'kernel/bootstrap/Comparable.rb'
require 'kernel/bootstrap/Set.rb'
require 'kernel/bootstrap/UnboundMethod.rb'
require 'kernel/bootstrap/Method.rb'
require 'kernel/bootstrap/ProcessTms.rb'
require 'kernel/bootstrap/Process.rb'
require 'kernel/bootstrap/ThrowCatch.rb'
require 'kernel/bootstrap/GC.rb'
require 'kernel/bootstrap/FileStat.rb'

require 'kernel/bootstrap/Struct.rb'
require 'kernel/bootstrap/Signal.rb'
require 'kernel/bootstrap/Math.rb'

# Include the common code after the basic primitives.  This is code that
# should be identical to, or very close to, the Rubinius code.
require 'kernel/common/misc.rb'
require 'kernel/common/Enumerable.rb'
require 'kernel/common/struct.rb'

# Include the delta code
require 'kernel/delta/Array.rb'
require 'kernel/delta/Dir.rb'
require 'kernel/delta/Range.rb'
