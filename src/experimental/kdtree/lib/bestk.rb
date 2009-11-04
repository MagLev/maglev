# Maintains a collection of elements of size K that best meet some
# criteria.  Elements are added to the collection, but only if they are one
# of the K best seen.
#
# == Example:

# To collect the best ten random numbers, where "best" is defined as
# "bigger" (i.e., '>'):
#
#     best = BestK.new(10)
#     1_000.times {|i| best.add rand(100) }
#     best.values  # => an array of the ten biggest numbers seen
#
#
class BestK
  # Initializes a new BestK element to manage at most +k+ elements.  If a
  # block is given, then it is used to determine which of two objects is
  # better.  The block should take two arguments and return true if the
  # first object is better than the second.  The default comparison is
  # "bigger is better":  { |a,b| a > b }
  #
  def initialize(k, &block)
    raise ArgumentError, "k must be greater than 0: #{k}" unless k > 0
    @limit = k
    @index = 0  # index of the first unused slot in @elements
    @elements = Array.new
    @is_better = block_given? ? block : proc {|a,b| a > b }
    @worst_idx = 0
  end

  # Attempt to add +el+ to receiver.  +el+ will be added if there are fewer
  # than k elements already in receiver, or if el is better than the worst
  # element already in receiver.  "better" is measured by the block passed
  # to receiver's initializer.
  # Returns receiver.
  def add(el)
    if @index < @limit
      @elements[@index] = el
      @worst_idx = @index if @is_better.call(@elements[@worst_idx], el)
      @index += 1
    elsif @is_better.call(el, @elements[@worst_idx])
      @elements[@worst_idx] = el
      @worst_idx = index_of_worst
    end
    self
  end

  def index_of_worst
    raise "empty" if @index == 0
    idx = 0
    @elements.each_with_index do |el,i|
      idx = i if @is_better.call(@elements[idx], @elements[i])
    end
    idx
  end

  # Return the worst element seen so far.
  def worst
    @elements[@worst_idx]
  end

  # Return an array of the current best elements seen so far.
  def values
    @elements[0..@index]
  end
end
