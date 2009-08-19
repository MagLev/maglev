# This tests that autoload associations can be persisted and will be active
# in a new VM.  Inspired by Rack

module M012
  # Create a RubyAutoloadAssociation object.  This will be done in
  # persistent mode.  The check will be that the reference can be resolved
  # in a fresh VM.
  autoload :Builder, "t012_builder"
end
