	# definitions in this file need to be in separate file from
	#   Globals.rb, to force all global definitions in Globals.rb
	#  to be created/resolved before we attempt to use some of them here.

	# see exception hierarchy Pickaxe Fig 27.1 page 462
class ScriptError < Exception; end
class StandardError < Exception; end
class SystemCallError < StandardError; end
class RuntimeError < StandardError; end
class IOError < StandardError; end
class RangeError < StandardError; end
class LocalJumpError < StandardError; end

