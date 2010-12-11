
class Migration
  def initialize(klass, file)
    @klass = klass
    @file = file
  end

  def migrate
    # 1. Define compatibility methods on old class
    # 2. Remove old class from the namespace
    # 3. Define new class
    # 4. Migrate instances from old to new
    # 5. Add new behavior on new class.
    # 6. Profit.
  end
end
