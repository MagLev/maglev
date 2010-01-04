class Hash
  include Enumerable

  # Override some of the methods in Enumerable with better implementations

  alias include? has_key?

  alias member? has_key?

  def to_a
    ary = Array.new(@numElements)
    n = 0
    self.each_pair { | k, v | 
      ary[n] = [ k , v ] 
      n += 1
    }
    ary 
  end

  def sort(&block)
    self.to_a.sort(&block)
  end
  
end
