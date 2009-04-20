class Range
  include Enumerable

  # Overrides of Enumerable for performance reasons
  def collect(&block)
    res = []
    current = @from
    lim = @to
    if @from._isNumeric
      unless @excludeEnd
        lim = lim + 1
      end
      begin
        res << block.call(current)
        current += 1
      end while current < lim
    else
      unless @excludeEnd
        lim = lim.succ
      end
      begin
        res << block.call(current)
        current = current.succ 
      end while current < lim
    end
    res
  end

  def member?(val)
    self.step(1) { |i| 
      if i == val 
        return true
      end
    }
    false
  end

  alias include? ===
end
