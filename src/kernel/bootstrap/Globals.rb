# We define all of these globals up front so that we can open these classes later without accidentally creating new ones

String = ''.class
Regexp = //.class
Range = (1..2).class
Fixnum = 1.class
NilClass = nil.class
Float=1.0e100.class
Integer = 1152921504606846976.class.superclass
Numeric = 1.class.superclass.superclass
Class = self.class.class.superclass.superclass
RUBY.global('Hash', 'RubyHash')
Array = [].class
Object = self.class
Symbol = :primitive.class

RUBY.global('Kernel', 'Kernel')    # for module Kernel
RUBY.global('Boolean', 'Boolean')
RUBY.global('TrueClass', 'TrueClass')
RUBY.global('FalseClass', 'FalseClass')
RUBY.global('MatchData', 'MatchData')
RUBY.global('File', 'GsFile')
RUBY.global('File__Stat', 'GsFileStat')  # File::Stat -->  GsFileStat
RUBY.global('Socket', 'Socket')
RUBY.global('IPSocket', 'IPSocket')
RUBY.global('TCPSocket', 'TCPSocket')
RUBY.global('TCPServer', 'TCPServer')
RUBY.global('IO', 'IO')
RUBY.global('Method', 'RubyMethod')  # RubyMethod , RubyUnboundMethod defined in .mcz
RUBY.global('UnboundMethod', 'RubyUnboundMethod')
RUBY.global('Random')
RUBY.global('Set', 'IdentitySet')
RUBY.global('Time', 'RubyTime')

RUBY.global("Gemstone", "System")

# 
#  Define some of the Ruby exception classes to be identical to certain
#   Smalltalk exception classes   .
#  see GlobalErrors.rb for additional creation of Ruby exception classes
#
#            Ruby name           Smalltalk Name
#
RUBY.global('Exception', 	'UserException')
  RUBY.global('SystemExit', 	'RubySystemExit')
  RUBY.global('SystemStackExit', 	'StackOverflow')
  RUBY.global('NoMemoryError', 	'OutOfMemory')
  RUBY.global('ScriptError',      'RubyScriptError')
    RUBY.global('LoadError',        'RubyLoadError')
    RUBY.global('NotImplementedError', 	'RubyNotImplementedError')
    RUBY.global('SyntaxError', 	'RubyParseError')

  RUBY.global('SignalException', 	'ControlInterrupt')

  RUBY.global('StandardError',    'Error')
    #IntepreterError is here in Smalltalk hierarchy
      RUBY.global('ArgumentError', 	'ArgumentError')
      RUBY.global('IOError', 		'IOError')
        RUBY.global('EOFError', 	'EOFError')
        RUBY.global('SocketError', 	'SocketError')
          RUBY.global('EBADF' , 		'SocketErrorEBADF')
          RUBY.global('ENOTCONN' , 	'SocketErrorENOTCONN')
          RUBY.global('EPIPE' , 		'SocketErrorEPIPE')
          RUBY.global('ECONNRESET' , 	'SocketErrorECONNRESET')
      RUBY.global('IndexError', 	'OffsetError')
      RUBY.global('LocalJumpError', 	'CannotReturn')
      RUBY.global('RangeError', 	'RangeError')
        RUBY.global('FloatDomainError', 'FloatingPointError')
      RUBY.global('RegexpError', 	'RegexpError')
      RUBY.global('RuntimeError', 	'RuntimeError')
      RUBY.global('SecurityError', 	'SecurityError')
      RUBY.global('SystemCallError', 	'SystemCallError')
      RUBY.global('ThreadError', 	'ThreadError')
      RUBY.global('TypeError', 	'TypeError')

    RUBY.global('NameError', 	'NameError')
      RUBY.global('NoMethodError', 	'MessageNotUnderstood')
    RUBY.global('ZeroDivisionError', 'ZeroDivide')



# ----------------------------------------------------------

RUBY.global("Proc", "ExecBlock")
def proc(&b); b; end
# remainder of Proc support in Proc.rb

# ----------------------------------------------------------

ARGV = []
#  $; is auto-initialized to nil if referenced, by RubySexpParser .
#  $/ is auto-initialized to "\n" by parser at first ref .
#  $-0 is translated to $/ by parser .
#  $-F is translated to  $;  by parser .
#  $!  is auto-initialized by parser at first occurance of a RubyRescueBodyNode

# -------------------

# A quick hack to get specs further w/o build having to poke the right string in
#  this slot. See ticket #76
RUBY_PLATFORM = 'Unspecified.platform'
RUBY_VERSION = '1.8.6'
RUBY_RELEASE_DATE = '09/15/2008' #TODO: Date should be inserted by build framework
RUBY_PATCHLEVEL = '114'  # TODO: this is what MRI 1.8.6 shows...

# If DEBUG_SPEC is true an executing rubyspec pauses on exception for topaz debugging
# If false, the handling is unchanged: exception is reported along with the
#  stringified stack.
# Value is examined by mspec/runner/mspec.rb.
DEBUG_SPEC = false


