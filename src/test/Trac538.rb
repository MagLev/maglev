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
s = Switch
x = Switch.new() { $aa = "Hi there 538" }
x.doit()
unless (ax = $aa) == 'Hi there 538' ; raise 'fail' ; end 
true
