class Range
  include Enumerable

  # Overrides of Enumerable for performance reasons
  primitive 'collect&', 'collect:'

  def member?(val)
    self.detect { |i| i == val }
  end

  alias include? ===
end
