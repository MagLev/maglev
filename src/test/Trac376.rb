#  file Trac376.rb
def bug(&block_arg)
  block = block_arg
  # block = Proc.new(&block_arg)   # workaround is to uncomment
  def block.each
    yield call   # To iterate over the block is to call it
  end
  block.each { |r| puts "Result: #{r}" }
end

bug { "hello" }        # Can't add singleton, receiver is invariant
bug { puts "hello" }   # should not be in ExecBlock>>_rubyCall
