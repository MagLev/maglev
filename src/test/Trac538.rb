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
#################### Trac Info
# ID:         538
# Summary:    Block passed to new not found at param initialization
# Changetime: 2011-06-12 23:32:13+00:00
###

#  Distilled from optparse.rb
#  
#  
#  {{{
#  class Switch
#    def initialize(block = Proc.new)
#      puts "block_given?: #{block_given?}"
#      @block = block
#    end
#    def doit
#      @block.call
#    end
#  end
#  x = Switch.new() { puts "Hi there" }
#  x.doit  # => Prints "Hi there"
#  }}}
#  
#  Apparently, the Proc.new gets passed the block passed to Switch.new() in MRI, but not in maglev.
#  
#  
#  The error in MagLev:
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb 
#  -- RubyFile>>load  : loading /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  error , tried to create Proc object without a block,
#            during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  ERROR 2023, Error, 'tried to create Proc object without a block'
#  topaz 1> 
#  
#  }}}
#  
#  From the Proc.new description in the pickaxe book:
#  
#  
#  {{{
#  Proc.new may be called without a block only within a method 
#  with an attached block, in which case that block is 
#  converted to the Proc object.
#  }}}
#  
#  
#  