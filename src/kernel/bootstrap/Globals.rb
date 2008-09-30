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
Hash = {}.class
Array = [].class
Object = self.class
Symbol = :primitive.class

RUBY.global('Kernel', 'Kernel')    # for module Kernel
RUBY.global('Boolean', 'Boolean')
RUBY.global('TrueClass', 'TrueClass')
RUBY.global('FalseClass', 'FalseClass')
RUBY.global('MatchData', 'MatchData')
RUBY.global('File', 'GsFile')
RUBY.global('FileStat', 'GsFileStat')
RUBY.global('Socket', 'Socket')
RUBY.global('IPSocket', 'IPSocket')
RUBY.global('TCPSocket', 'TCPSocket')
RUBY.global('TCPServer', 'TCPServer')
RUBY.global('IO', 'IO')
RUBY.global("Method", "RubyMethod")
RUBY.global('Random')
RUBY.global('Set', 'IdentitySet')
RUBY.global('Time', 'RubyTime')

RUBY.global('Exception', 'UserException')
RUBY.global('StandardError', 'Error')
RUBY.global('LoadError', 'RubyLoadError')
RUBY.global('SystemExit', 'RubySystemExit')
RUBY.global('ZeroDivisionError', 'ZeroDivide')
RUBY.global( 'EBADF' , 'SocketErrorEBADF')
RUBY.global( 'ENOTCONN' , 'SocketErrorENOTCONN')
RUBY.global( 'EPIPE' , 'SocketErrorEPIPE')
RUBY.global( 'ECONNRESET' , 'SocketErrorECONNRESET')

RUBY.global("Gemstone", "System")

RUBY.global("Proc", "ExecBlock")
def proc(&b); b; end
# remainder of Proc support in Proc.rb

ARGV = []
#  $; is auto-initialized to nil if referenced, by RubySexpParser .
#  $/ is auto-initialized to "\n" by parser at first ref .
#  $-0 is translated to $/ by parser .
#  $-F is translated to  $;  by parser .
#  $!  is auto-auto-initialized by parser at first occurance of a RubyRescueBodyNode

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


