class EncodingError < StandardError
end

class Encoding
  class UndefinedConversionError < EncodingError
  end

  class ConverterNotFoundError < EncodingError
  end

  class CompatibilityError < EncodingError
  end
  
  def initialize
    
  end

  def self.list
    []
  end
  def self.find(name)
  end

  def self.aliases
  end

  def compatible?(obj1, obj2)
  end

  def self.default_external
  end

  def self.default_external=(enc)
  end

  def self.default_internal
  end

  def self.default_internal=(enc)
  end

  def self.locale_charmap
    
  end

  def self.name_list
    
  end

  def ascii_compatible?
    
  end

  def dummy?
    
  end

  def inspect
  end

  def name
  end

  def names
    
  end


  def replicate(name)
    
  end

  def to_s
    
  end

end

Encoding::UTF_8 = Encoding.new
Encoding::ASCII_8BIT = Encoding.new
Encoding::BINARY = Encoding::ASCII_8BIT
