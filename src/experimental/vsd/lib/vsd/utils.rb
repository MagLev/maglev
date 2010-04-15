module VSD
  module Utils

    # Given a block, a zero element and several enums, call the block with
    # the first element from all the enums, then call it with the second
    # elements from all enums etc.  E.g., to do the element-wise sum of
    # several arrays:
    #
    #    mapcar(0, [0,0,0], [1,2,3], [2,4,6]) {|x,y| x + y} # => [3,6,9]
    def self.mapcar(*args)
      zargs = args[0].zip(*args[1..-1])
      zargs.map {|x| x.inject {|s,i| yield(s,i) } }
    end
  end
end
