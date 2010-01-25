# Heap, BestK
module Collections
  # A simple heap based on arrays.  A heap is a tree with the property that
  # all the children of a node N have a value greater then or equal to the
  # value of node N (a max heap).  This implies that the minimum value in
  # the tree is the root of the tree.  A heap with the comparison reveresed
  # (a min heap) will have a minimal value at the root.
  #
  # TODO:
  # 1. percolate_down and bubble_up are recursive.  If performance is
  #    suspect, they ould be implemented as a loop.
  # 2. No thought to thread safety.
  class Heap
    def self.left_idx(i);  2*i + 1 end
    def self.right_idx(i)  2*i + 2 end
    def self.parent_idx(i) (i-1)/2 end

    # Initialize a new heap.  If +size+ is given, space for that many
    # elements will be allocated up front.  The heap is not limited to
    # +size+ elements.  Values passed into the heap are compared with the
    # given block (or <code>proc {|a,b| a < b}</code>, if no block is
    # given).  if block.call(a,b) is true, then a will be placed closer to
    # the root than b.  The default implements a max heap.
    def initialize(size=nil, &block)
      @cmp = block_given? ? block : proc {|a,b| a < b}
      @data = size.nil? ? Array.new : Array.new(size)
      @bottom = 0 # index of bottom of heap
    end

    # Return the number of values in the heap.
    def size
      @bottom
    end

    # Add an element to the heap.  Rebalances the heap. Returns self.
    def add(el)
      @data[@bottom] = el
      bubble_up(@bottom)
      @bottom += 1
      self
    end

    # returns, but does not remove, the top of the heap.
    def top
      raise "Empty heap" if @bottom == 0
      @data[0]
    end

    # removes and returns the top of the heap
    def delete_top
      raise "Empty heap" if @bottom == 0
      ret_value = @data[0]
      @bottom -= 1

      if @bottom > 0
        @data[0] = @data[@bottom]
        percolate_down(0)
      end

      ret_value
    end

    # Test the node at index +i+ for heap violation.  If it violates the
    # heap property, push it up until it doesn't.
    def bubble_up(i)
      return if i <= 0

      parent = Heap.parent_idx i
      if @cmp.call(@data[i], @data[parent])
        @data[i], @data[parent] = @data[parent], @data[i]
        bubble_up(parent)
      end
    end

    # Test the node at index +i+ for heap violation.  If it violates the
    # heap property, push it down until it doesn't.
    def percolate_down(i)
      return if i >= @bottom

      li = Heap.left_idx  i
      return if li >= @bottom # we fill left to right, so => right is empty too

      ri = Heap.right_idx i
      child = if ri >= @bottom
                li
              else
                @cmp.call(@data[li], @data[ri]) ? li : ri
              end
      if @cmp.call(@data[child], @data[i])
        @data[child], @data[i] = @data[i], @data[child]
        percolate_down(child)
      end
    end

    # Return an unsorted array of all the values.  The max (min) element is
    # at index 0, but no other gurantee is made.
    def values
      if @bottom > 0
        # TODO: why was the reverse in there???
        #        @data[0..(@bottom-1)].reverse
        @data[0..(@bottom-1)]
      else
        []
      end
    end

    # Remove all elements from the heap and return them in sorted order.
    def remove_all
      Array.new(@bottom) { |i| self.delete_top }
    end
  end

  # Maintains a collection of at most K elements that best meet some
  # criteria.  Elements are added to the collection, but only if they are one
  # of the K best seen so far.  The notion of best is supplied by the client
  # code as a block to the constructor.
  #
  # == Example:
  #
  # To collect the best ten random numbers, where "best" is defined as
  # "bigger" (i.e., '>'):
  #
  #     best = BestK.new(10)
  #     1_000.times {|i| best.add rand(100) }
  #     best.values  # => an array of the ten biggest numbers seen
  #
  class BestK < Heap
    # Initializes a new BestK element to manage at most +k+ elements.  If a
    # block is given, then it is used to determine which of two objects is
    # better.  The block should take two arguments and return true if the
    # first object is better than the second.  The default comparison is
    # "bigger is better":  { |a,b| a > b }
    #
    def initialize(k, &block)
      raise ArgumentError, "k must be greater than 0: #{k}" unless k > 0
      # Because we really use a heap, we need to invert the logic of the
      # comparison, since we throw away the top element of the heap.
      cmp = block_given? ?
        Proc.new {|a,b| not block.call(a,b) } :
        Proc.new {|a,b| a <= b }

      super(k, &cmp)
      @limit = k
    end

    # Returns true if we have N elements.
    def full?
      @limit == size
    end

    # Attempt to add +el+ to receiver.  +el+ will be added if there are fewer
    # than k elements already in receiver, or if el is better than the worst
    # element already in receiver.  "better" is measured by the block passed
    # to receiver's initializer.
    # Returns receiver.
    def add(el)
      if size >= @limit
        # Only bother to insert if it is not worse than
        # the top element.
        if @cmp.call(top, el)
          super(el)
          delete_top
        end
      else
        super(el)
      end
      self
    end

    # Return the worst element seen so far.
    def worst
      top
    end
  end
end
