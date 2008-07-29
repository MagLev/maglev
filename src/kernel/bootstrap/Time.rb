class Time
  self.class.primitive 'now'
  
  primitive '==', '='
  primitive 'hash'
  primitive 'to_f', 'asFloat'  

  primitive 'httpdate'

  primitive 'year'
  primitive 'month'
  primitive 'day'
  primitive 'hour'
  primitive 'minute'
  primitive 'second'
  primitive 'monthName'
  
  def strftime(str)
    str.gsub(/%Y/, year.to_s).gsub(/%m/, month.to_s).gsub(/%d/, day.to_s).gsub(/%H/, hour.to_s).gsub(/%M/, minute.to_s).gsub(/%S/, second.to_s).gsub(/%b/, monthName).gsub(/%Z/, "")
  end
  
  def inspect
    httpdate
  end
  
  def -(other)
    to_f - other.to_f
  end
  
end
