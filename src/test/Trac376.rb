#  file Trac376.rb

class C376
  R = []
  def bug(&block_arg)
    block = block_arg
    def block.each
      yield call   # To iterate over the block is to call it
    end
    block.each { |r| R << r }
    def block.setiv(v)
      @iva = v
    end
    def block.getiv
      r = @iva 
      r
    end
    block.setiv(98)
    x = block.getiv
    unless x == 98; raise 'fail';end
    puts "bug done"
  end
end

o = C376.new
o.bug { "helloA" }   # Can't add singleton, receiver is invariant
o.bug { "helloB" }   # should not be in ExecBlock>>_rubyCall

unless C376::R == [ 'helloA' , 'helloB' ] ; raise 'error'; end 
puts "ok"
true
