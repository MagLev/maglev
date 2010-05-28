# From rake:
def sh(*cmd, &block)
  unless block_given?
    block = lambda { puts "Ok"; 10 }
  end
  block.call
end

sh("x")  # Maglev prints 'no block was passed'

