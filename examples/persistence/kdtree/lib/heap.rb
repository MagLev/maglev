# A simple binary heap, that is limited to N elements

module Heap
  # A simple heap based on arrays.
  class Heap
    def self.left_idx(i);  2*i + 1 end
    def self.right_idx(i)  2*i + 2 end
    def self.parent_idx(i) (i-1)/2 end

    def initialize(size=nil, &block)
      @cmp = block_given? ? block : proc {|a,b| a < b}
      @data = size.nil? ? Array.new : Array.new(size)
      @bottom = 0 # index of bottom of heap
    end

    def size
      @bottom
    end

    # Add an element to the heap.  Returns self.
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

    def bubble_up(i)
      return if i <= 0

      pi = Heap.parent_idx i
      if @cmp.call(@data[i], @data[pi])
        @data[i], @data[pi] = @data[pi], @data[i]
        bubble_up(pi)
      end
    end

    def percolate_down(i)
      return if i >= @bottom

      li = Heap.left_idx  i
      return if li >= @bottom # we fill left to right, so => right is empty too

      ri = Heap.right_idx i
      swap_child = if ri >= @bottom
                     li
                   else
                     @cmp.call(@data[li], @data[ri]) ? li : ri
                   end
      if @cmp.call(@data[swap_child], @data[i])
        @data[swap_child], @data[i] = @data[i], @data[swap_child]
        percolate_down(swap_child)
      end
    end

    def values
      if @bottom > 0
        @data[0..(@bottom-1)].reverse
      else
        []
      end
    end

    # returns an array of all values in sorted order
    def remove_all
      Array.new(@bottom) { |i| self.delete_top }
    end
  end


  class NHeap < Heap
    def initialize(size, &block)
      super
      @limit = size
    end

    # Adds el to the heap.  If the heap already contains N elements, then
    # it removes the top element before inserting.  Returns nil or the
    # removed element.
    def add(el)
      ret_value = nil
      if @bottom >= @limit
        # Only bother to insert if it is not worse than
        # the top element.
        if @cmp.call(@data[0], el)
          ret_value = @data[0]
          @data[0] = el
          percolate_down(0)
        end
      else
        super(el)
      end
      ret_value
    end
  end

#   # TODO: NHeap

#   class Node
#     attr_accessor :left, :right
#     attr_reader   :value

#     def initialize(value, left=nil, right=nil)
#       @value = value
#       @left = left
#       @right = right
#     end
#   end
end
