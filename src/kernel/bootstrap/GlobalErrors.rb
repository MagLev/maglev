
	# see exception hierarchy Pickaxe Fig 27.1 page 462
class ScriptError < Exception; end
class StandardError < Exception; end
class SystemCallError < StandardError; end
class RuntimeError < StandardError; end
class IOError < StandardError; end
class RangeError < StandardError; end
class LocalJumpError < StandardError; end

