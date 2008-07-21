# We define all of these globals up front so that we can open these classes later without accidentally creating new ones

String = ''.class
Regexp = //.class
Range = (1..2).class
Fixnum = 1.class
NilClass = nil.class
TrueClass = true.class
Float=1.0e100.class
Integer = 1152921504606846976.class.superclass
Numeric = 1.class.superclass.superclass
Class = self.class.class.superclass.superclass
Hash = {}.class
Array = [].class
Object = self.class
Symbol = :primitive.class

RUBY.global('MatchData', 'MatchData')
RUBY.global('File', 'GsFile')
RUBY.global('Socket', 'Socket')
RUBY.global('IPSocket', 'IPSocket')
RUBY.global('TCPSocket', 'TCPSocket')
RUBY.global('TCPServer', 'TCPServer')
RUBY.global('IO', 'IO')
RUBY.global("Method", "RubyMethod")
RUBY.global('Random')
RUBY.global('Set', 'IdentitySet')
RUBY.global('Time', 'DateTime')

RUBY.global('Exception', 'Exception')
RUBY.global('StandardError', 'Error')
RUBY.global('LoadError', 'RubyLoadError')
RUBY.global( 'EBADF' , 'SocketErrorEBADF')
RUBY.global( 'ENOTCONN' , 'SocketErrorENOTCONN')
RUBY.global( 'EPIPE' , 'SocketErrorEPIPE')
RUBY.global( 'ECONNRESET' , 'SocketErrorECONNRESET')

class SystemCallError < Exception; end
class RuntimeError < Exception; end
class IOError < Exception; end

RUBY.global("Gemstone", "System")

Gemstone.class.primitive 'commitTransaction'
Gemstone.class.primitive 'abortTransaction'
Gemstone.class.primitive 'beginTransaction'

def proc(&b); b; end
Proc = proc{}.class

ARGV = []

