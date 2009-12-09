class Data
  attr_reader :x, :y, :migrated
  def initialize(x)
    @x = x
    @y = 2 * x
    @migrated = false
  end

  def migrate_from(old_instance)
    @x = old_instance.x
    @y = @x * 3
    @migrated = true
  end
end
