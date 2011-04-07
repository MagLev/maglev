#  The Smalltalk product, upon which MagLev is based, comes with a simple
#  code and statistics browser written in Smalltalk.  This ruby script
#  registers that code into the Ruby namespace, and then starts the
#  application.  Once it is running, it will print a URL to connect to.

# Register the Smalltalk WebTools Server class into the Ruby Namespace
SmalltalkWebTools = __resolve_smalltalk_global(:Server)

# Expose the class-side method needed to run the application
class SmalltalkWebTools
  class_primitive_nobridge 'run', 'runInForeground'
end

# Invoke.  This will print out something like: http://cairo:60166/
# Point your web browser to the given url and play.
SmalltalkWebTools.run
