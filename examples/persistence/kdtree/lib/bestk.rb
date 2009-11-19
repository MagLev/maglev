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
require 'heap'

class BestK
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
    @heap = if block_given?
              Heap::NHeap.new(k) {|a,b| not block.call(a,b) }
            else
              Heap::NHeap.new(k) {|a,b| a <= b }
            end
  end

  # Attempt to add +el+ to receiver.  +el+ will be added if there are fewer
  # than k elements already in receiver, or if el is better than the worst
  # element already in receiver.  "better" is measured by the block passed
  # to receiver's initializer.
  # Returns receiver.
  def add(el)
    @heap.add(el)
    self
  end

  # Returns 0 <= n <= k.  The number of elements currently in the
  # collection.
  def size
    @heap.size
  end

  def full?
    @heap.full?
  end

  # Return the worst element seen so far.
  def worst
    @heap.top
  end

  # Return an array of the current best elements seen so far.
  def values
    @heap.values
  end
end
