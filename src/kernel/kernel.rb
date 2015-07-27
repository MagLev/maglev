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
# We use RUBY.* in this file, since we are bootstrapping.  Normal code,
# after this file is read, can use Kernel#require etc.
RUBY.class.primitive 'require', 'requireFileNamed:'
RUBY.class.primitive 'load', 'loadFileNamed:'
RUBY.class.primitive 'global', 'installConstant:name:'

RUBY.require 'kernel/bootstrap/Globals.rb'
RUBY.require 'kernel/bootstrap/GlobalErrors.rb'

RUBY.require 'kernel/bootstrap/Object1.rb'

#
# bootstrap
#
RUBY.require 'kernel/bootstrap/Type.rb'
RUBY.require 'kernel/bootstrap/Kernel.rb'
RUBY.require 'kernel/bootstrap/Module.rb'
# deleted Behavior.rb
RUBY.require 'kernel/bootstrap/Class.rb'
RUBY.require 'kernel/bootstrap/StClass.rb'
RUBY.require 'kernel/bootstrap/Object.rb'
RUBY.require 'kernel/bootstrap/BasicObject.rb'
RUBY.require 'kernel/bootstrap/Module2.rb'
RUBY.require 'kernel/bootstrap/Module3.rb'
RUBY.require 'kernel/bootstrap/Maglev1.rb'
RUBY.require 'kernel/bootstrap/System.rb'
RUBY.require 'kernel/bootstrap/Errno1.rb'
RUBY.require 'kernel/bootstrap/Exception.rb'
RUBY.require 'kernel/bootstrap/Fixnum.rb'
RUBY.require 'kernel/bootstrap/Enumerator.rb'
RUBY.require 'kernel/bootstrap/Enumerator2.rb'
#
RUBY.require 'kernel/bootstrap/Float.rb'
RUBY.require 'kernel/bootstrap/Integer.rb'
RUBY.require 'kernel/bootstrap/Numeric.rb'
RUBY.require 'kernel/bootstrap/Bignum.rb'
RUBY.require 'kernel/bootstrap/Precision.rb'
RUBY.require 'kernel/bootstrap/Boolean.rb'
RUBY.require 'kernel/bootstrap/NilClass.rb'
RUBY.require 'kernel/bootstrap/Proc.rb'
RUBY.require 'kernel/bootstrap/Array1.rb'
RUBY.require 'kernel/bootstrap/Array.rb'
RUBY.require 'kernel/bootstrap/Hash1.rb'
RUBY.require 'kernel/bootstrap/Hash.rb'
RUBY.require 'kernel/bootstrap/IdentityHash.rb'
RUBY.require 'kernel/bootstrap/String.rb'
RUBY.require 'kernel/bootstrap/Symbol.rb'
RUBY.require 'kernel/bootstrap/Regexp.rb'
RUBY.require 'kernel/bootstrap/MatchData.rb'
# Globals2 deleted
RUBY.require 'kernel/bootstrap/Env.rb'
RUBY.require 'kernel/bootstrap/Errno.rb'
RUBY.require 'kernel/bootstrap/Dir.rb'
RUBY.require 'kernel/bootstrap/Fcntl.rb'
RUBY.require 'kernel/bootstrap/IO.rb'
RUBY.require 'kernel/bootstrap/File.rb'
RUBY.require 'kernel/bootstrap/Range.rb'
RUBY.require 'kernel/bootstrap/FlipFlop.rb'
RUBY.require 'kernel/bootstrap/RecursionGuard.rb'
RUBY.require 'kernel/bootstrap/Comparable.rb'
RUBY.require 'kernel/bootstrap/Thread1.rb'
RUBY.require 'kernel/bootstrap/Thread.rb'
RUBY.require 'kernel/bootstrap/IdentitySet.rb'
RUBY.require 'kernel/bootstrap/Time.rb'
RUBY.require 'kernel/bootstrap/BasicSocket.rb'
RUBY.require 'kernel/bootstrap/Socket.rb'
RUBY.require 'kernel/bootstrap/IO2.rb'
RUBY.require 'kernel/bootstrap/UnboundMethod.rb'
RUBY.require 'kernel/bootstrap/Method.rb'
RUBY.require 'kernel/bootstrap/Process.rb'
RUBY.require 'kernel/bootstrap/GC.rb'
RUBY.require 'kernel/bootstrap/FileStat.rb'
RUBY.require 'kernel/bootstrap/Mutex.rb'
RUBY.require 'kernel/bootstrap/String2.rb'
RUBY.require 'kernel/bootstrap/String3.rb'
RUBY.require 'kernel/bootstrap/Queue.rb'
RUBY.require 'kernel/bootstrap/Math.rb'
RUBY.require 'kernel/bootstrap/Struct.rb'
RUBY.require 'kernel/bootstrap/ThreadGroup.rb'
RUBY.require 'kernel/bootstrap/ObjectSpace1.rb'
RUBY.require 'kernel/bootstrap/ObjectSpace.rb'
RUBY.require 'kernel/bootstrap/Binding1.rb'
RUBY.require 'kernel/bootstrap/Binding.rb'
RUBY.require 'kernel/bootstrap/Kernel2.rb'
RUBY.require 'kernel/bootstrap/File2.rb'
RUBY.require 'kernel/bootstrap/RubyContext.rb'
RUBY.require 'kernel/bootstrap/zlib_czstream1.rb'
RUBY.require 'kernel/bootstrap/zlib_czstream.rb'
RUBY.require 'kernel/bootstrap/Maglev.rb'
RUBY.require 'kernel/bootstrap/bigdecimal1.rb'
RUBY.require 'kernel/bootstrap/bigdecimal.rb'
RUBY.require 'kernel/bootstrap/Gprof.rb'
RUBY.require 'kernel/bootstrap/Signal.rb'
RUBY.require 'kernel/bootstrap/timeout1.rb'
RUBY.require 'kernel/bootstrap/timeout.rb'
RUBY.require 'kernel/bootstrap/Data.rb'
RUBY.require 'kernel/bootstrap/Random.rb'
RUBY.require 'kernel/bootstrap/Continuation.rb'

# Include the common code after the basic primitives.  This is code that
# should be identical to, or very close to, the Rubinius code.
# RUBY.require 'kernel/common/misc.rb'  # not used
RUBY.require 'kernel/common/ctype.rb'

RUBY.require 'kernel/common/integer.rb'
RUBY.require 'kernel/common/Enumerable.rb'
RUBY.require 'kernel/common/Enumerable2.rb'
RUBY.require 'kernel/common/Enumerable3.rb'
RUBY.require 'kernel/common/struct.rb'
RUBY.require 'kernel/common/kernel.rb'
RUBY.require 'kernel/common/string.rb'
RUBY.require 'kernel/common/symbol.rb'
RUBY.require 'kernel/common/dir.rb'
RUBY.require 'kernel/common/dir_glob.rb'
RUBY.require 'kernel/common/file.rb'
RUBY.require 'kernel/common/marshal0.rb'
RUBY.require 'kernel/common/marshal.rb'
RUBY.require 'kernel/common/marshal2.rb'
RUBY.require 'kernel/common/regexp.rb'
RUBY.require 'kernel/common/io.rb'
RUBY.require 'kernel/common/filetest.rb'
RUBY.require 'kernel/common/Encoding.rb'
RUBY.require 'kernel/common/fiber.rb'

# Include the delta code.  The delta code overwrites the rubinius
# common code with gemstone specific mods.
# RUBY.require 'kernel/delta/hash.rb'
RUBY.require 'kernel/delta/Module.rb'
RUBY.require 'kernel/delta/Object3.rb'
RUBY.require 'kernel/delta/Array.rb'
RUBY.require 'kernel/delta/Dir.rb'
RUBY.require 'kernel/delta/Range.rb'
RUBY.require 'kernel/delta/Process.rb'
RUBY.require 'kernel/delta/purerubystringio.rb'
RUBY.require 'kernel/delta/purerubystringio2.rb'
RUBY.require 'kernel/delta/String.rb'
RUBY.require 'kernel/delta/ruby_utils.rb'

# deleted test of Maglev::System.session_temp( :MAGLEV_primLoadFFI ) for env2
RUBY.require 'kernel/delta/ffi1.rb'
RUBY.require 'kernel/delta/ffi2.rb'
RUBY.require 'kernel/delta/ffi3.rb'
RUBY.require 'kernel/delta/ffi_struct.rb'
RUBY.require 'kernel/delta/ffi_enum.rb'
RUBY.require 'kernel/delta/pointer.rb'
RUBY.require 'kernel/delta/buffer.rb'
RUBY.require 'kernel/bootstrap/Argf.rb'
RUBY.require 'kernel/bootstrap/BasicObject.rb'
