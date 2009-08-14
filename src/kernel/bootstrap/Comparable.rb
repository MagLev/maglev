module Comparable
  def ==(other)
    begin
      status = (self <=> other) 
      if status._isNumeric
        status == 0
      else
        nil
      end
     rescue StandardError
       nil
     end
  end
  
  def >(other)
    status =  self <=> other
    if status._isNumeric
      status > 0
    else
      raise ArgumentError, 'in Compariable#> , <=> returned a non-Numeric'
    end
  end
  
  def <(other)
    status =  self <=> other
    if status._isNumeric
      status < 0
    else
      raise ArgumentError, 'in Compariable#< , <=> returned a non-Numeric'
    end
  end
  
  def >=(other)
    status =  self <=> other
    if status._isNumeric
      status >= 0
    else
      raise ArgumentError, 'in Compariable#>= , <=> returned a non-Numeric'
    end  
  end
  
  def <=(other)
    status =  self <=> other
    if status._isNumeric
      status <= 0
    else
      raise ArgumentError, 'in Compariable#<= , <=> returned a non-Numeric'
    end
  end
  
  def between?(min, max)
    (min <= self) && (self <= max)
  end
end
