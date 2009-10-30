class Range
  include Enumerable

  def member?(val)
    # implementation to conform to rubyspecs, Pickaxe docs incorrect
    fr = @from
    if fr._isNumeric && val._isNumeric
      if @excludeEnd
        val >= fr && val < @to
      else
        val >= fr && val <= @to
      end
    else
      self.step(1) { |i| 
        if i == val 
          return true
        end
      }
      false
    end
  end

  alias include? ===
end
