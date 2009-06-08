# Distilled from optparse.rb

class Switch
  def initialize(block = Proc.new)
    puts "block_given?: #{block_given?}"
    @block = block
  end
  def doit
    @block.call
  end
end
x = Switch.new() { puts "Hi there" }
x.doit  # => Prints "Hi there"
