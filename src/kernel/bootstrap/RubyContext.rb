# = RubyContext
#
# Root of the Ruby environment in the MagLev VM.
#
# The VM maintains a singleton RubyContext object, referred to in ruby code
# as +RUBY+ (see <tt>RubyContext>>initialize</tt>).  The singleton,
# hereafter referred to as +RUBY+, provides the following services:
#
# * Entry points to load or require ruby files (available only from
#   Smalltalk code) (<tt>RubyContext(c)>>loadFileNamed:</tt>,
#   <tt>RubyContext(c)>>requireFileNamed:</tt>)
#
# * Manage the ruby method dictionaries by copying between session state
#   and persistent state.
#
# * Hold the root namespace object (which holds +$+ globals and top level
#   constants).
#
# +RUBY+ is a persistent object, and hence changes to its namespace will be
# commited to the repository during a <tt>Gemstone.commitTransaction</tt>.

# RubyContext is identically Smalltalk RubyContext.
class RubyContext
  # Set this instance as the saved version of the ruby context and make
  # session methods visible to the persistent store (does NOT commit).
  class_primitive 'save_context', 'save'

  # Return the current default context after initializing or loading it.
  #
  # If no saved context exists, then create a new one, load the primitives
  # code (<tt>src/kernel/kernel.rb</tt>), save the instance and commit the
  # transaction.  The global namespace will start empty, and be initialized
  # with effects of loading the primitives.
  #
  # If a saved contet exists, retrieve it and install its methods into
  # session temps (copy from persistent to session temps).
  class_primitive 'load_context', 'load'

  # These primitives are defined in kernel.rb for bootstrapping.  The
  # comment here is for documentation.
  #
  # RUBY.class.primitive 'require', 'requireFileNamed:'
  # RUBY.class.primitive 'load', 'loadFileNamed:'
  # RUBY.class.primitive 'global', 'installGlobal:name:'

  # access to persistentMode in transient instance of RubyCompilerState
  class_primitive_nobridge 'persistence_mode', 'persistenceMode'
  class_primitive_nobridge 'persistence_mode=', 'persistenceMode:'
  class_primitive_nobridge 'persistable_instances', 'persistableInstances'
  class_primitive_nobridge 'persistable_instances=', 'persistableInstances:'

  # Customize the top level object <tt>top_self</tt>.  This is called once
  # per VM to setup the singleton methods for the top self.  See
  # <tt>RubyCompiler>>compileFileNamed:loadName:</tt> for invocation.
  def self.customize_top_self(top_self)
    # Commented out since adding the eigneclass causes testGlobals1a.rb to
    # fail, but only if run via RubyContext _runVmUnit.
#     class << top_self
#       def to_s
#         "main"
#       end
#       def inspect
#         "main"
#       end
#       def include(*args)
#         Object.include(*args)
#       end
#       def private(*args)
#         Object.private(*args)
#       end
#       def public(*args)
#         Object.public(*args)
#       end
#     end
  end
end
