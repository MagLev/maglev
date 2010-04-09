class Range
  include Enumerable

  def member?(val)
    # implementation to conform to rubyspecs, Pickaxe docs incorrect
    fr = @_st_from
    if fr._isNumeric && val._isNumeric
      if @_st_excludeEnd
        val >= fr && val < @_st_to
      else
        val >= fr && val <= @_st_to
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
