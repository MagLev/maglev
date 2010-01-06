# From the ActiveSupport BlankSlate class.
ma = String.instance_method(:class)

# User classes should also work:
class BlankSlate
end
mb = BlankSlate.instance_method(:class)
unless ma.class.equal?(UnboundMethod); raise 'error'; end
unless mb.class.equal?(UnboundMethod); raise 'error'; end
true
