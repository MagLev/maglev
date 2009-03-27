#  file Trac376.rb

class C376
  R = []
  def bug(&block_arg)
    block = block_arg
    def block.each
      yield call   # To iterate over the block is to call it
    end
    block.each { |r| R << r }
  end
end

o = C376.new
o.bug { "helloA" }        # Can't add singleton, receiver is invariant
o.bug { "helloB" }   # should not be in ExecBlock>>_rubyCall

unless C376::R == [ 'helloA' , 'helloB' ] ; raise 'error'; end 
true
