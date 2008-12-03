module Gem
  def self.source_index
    puts "Gem.source_index"
  end
  class << self
    alias cache source_index
  end
end
true
