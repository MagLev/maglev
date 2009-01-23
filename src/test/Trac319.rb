# Distilled from src/lib/set.rb
class Set
  def initialize(enum = nil, &block)
  end
end

class SortedSet < Set
  def initialize(*args, &block)
    super
  end
end

SortedSet.new([1,2,3])
