class DataV2
  attr_reader :x, :y, :migrated
  def initialize(x)
    @x = x
    @y = 2 * x
    @migrated = false
  end

  # In the lazy migration, most methods are undefined, so we have to jump
  # through a couple of hoops here.
  def migrate_from(old_instance)
    @x = old_instance.instance_variable_get :@x
    @y = @x * 3
    @migrated = true
  end
end
