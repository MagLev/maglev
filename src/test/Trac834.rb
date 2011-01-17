# Bug found trying to run a webapp with bundler and rack.
#
#
# Bundler was reporting that the source wasn't checked out.
#
# The issue is with subclasses that define instance vars, but the accessors
# are monkey patched into a pre-existing class.

# This is a pre-existing class.  It does not have @source instvar
class GemDependency
  
end

# Now we monkey patch the class to add the accessor for @source
class GemDependency
  attr_accessor :source
end

# Finally, we derive a class and define a real, fixed instvar for @source
class BundlerDependency < GemDependency
  def initialize(source)
    @source = source
  end
end


bd = BundlerDependency.new(10)
p bd.source
bd.source = 20  # This assignment fails
p bd.source

raise "Fail" unless bd.source == 20
