# This file is loaded after Globals.rb.
# Definitions in this file need to be in separate file from
#   Globals.rb, to force all global definitions in Globals.rb
#  to be created/resolved before we attempt to use some of them here.

# See the exception hierarchy Pickaxe Fig 27.1 page 462.  Some of the
# exceptions listed in Pickaxe are directly mapped to a Smalltalk exception
# in Globals.rb.

# Exception => Exception:  See Globals.rb
# fatal: ignored

class NoMemoryError       < Exception; end

class ScriptError         < Exception; end
# class LoadError         < ScriptError; end     # See Globals.rb
class NotImplementedError < ScriptError; end
class SyntaxError         < ScriptError; end

class SignalException     < Exception; end
class Interrupt           < SignalException; end

# class StandardError     < Exception; end       # See Globals.rb
class ArgumentError       < StandardError; end
class IOError             < StandardError; end
class EOFError            < IOError; end
class IndexError          < StandardError; end
class LocalJumpError      < StandardError; end
class NameError           < StandardError; end
class NoMethodError       < NameError; end
class RangeError          < StandardError; end
class FloatDomainError    < RangeError; end
class RegexpError         < StandardError; end
class RuntimeError        < StandardError; end
class SecurityError       < StandardError; end
class SystemCallError     < StandardError; end
class ThreadError         < StandardError; end
class TypeError           < StandardError; end
# class ZeroDivisionError < StandardError; end   # See Globals.rb

# SystemExit        # see Globals.rb
class SystemStackError    < Exception; end



