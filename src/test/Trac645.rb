# From the ActiveSupport BlankSlate class.
p String.instance_method(:class)

# User classes should also work:
class BlankSlate
end
p BlankSlate.instance_method(:class)
