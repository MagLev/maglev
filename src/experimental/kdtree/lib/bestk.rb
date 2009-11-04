# Do not derive from Array, since there are too many methods to override...
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

  def add(el)
    if @index < @limit
      @elements[@index] = el
      @worst_idx = @index if @is_better.call(@elements[@worst_idx], el)
      @index += 1
    elsif @is_better.call(el, @elements[@worst_idx])
      @elements[@worst_idx] = el
      @worst_idx = index_of_worst
    end
  end

  def index_of_worst
    raise "empty" if @index == 0
    idx = 0
    @elements.each_with_index do |el,i|
      idx = i if @is_better.call(@elements[idx], @elements[i])
    end
    idx
  end

  def worst
    @elements[@worst_idx]
  end

  def values
    @elements[0..@index]
  end
end
