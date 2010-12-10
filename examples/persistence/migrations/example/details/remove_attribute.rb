# If you want to rename "foo" to "bar"

class C
  VERSION "1.0.0"

  attr_accessor :foo

  def initialize(foo)
    @foo = foo
  end
end

# First, consider just adding a "renaming" accessor:
class C
  VERSION "1.0.1"

  # attr_accessor :foo

  def bar
    @foo
  end
  def bar=(value)
    @foo = value
  end

  def initialize(bar)
    @foo = bar
  end
end

# But if you insist
