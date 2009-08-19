module PSpec
  def self.describe(msg, &block)
    @descriptions[msg] = block
  end

  def self.list_descriptions
    @descriptions.each_key { |k| puts k }
  end
end

class Object
  def describe(msg, &block)
    PSpec.describe(msg, &block)
  end
end

if $0 == __FILE__
end
