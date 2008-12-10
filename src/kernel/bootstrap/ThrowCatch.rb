class ThrownValue < Exception
  primitive 'pass', 'pass'
  primitive 'signal', 'signal'

  def initialize(symbol, value)
    @name = symbol
    @returnValue = value
  end

  def name
    @name
  end

  def returnValue
    @returnValue
  end

end

class Object
  def throw(symbol, value=nil)
    ThrownValue.new(symbol, value).signal
  end

  def catch(symbol, obj=nil, &block)
    begin
      block.call
    rescue ThrownValue => err
      if err.name == symbol
        err.returnValue
      else
        err.pass
      end
    end
  end
end
