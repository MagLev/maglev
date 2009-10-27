class DefaultV1
  #CONST_ONE = 1  # Remove a constant
  CONST_TWO = 4   # Change a constant
  CONST_THREE = 3 # Add a constant
  attr_accessor :a, :b, :c
  def m1; 1 end
  # def m2; 2 end  # Remove a Method

  # Use default migrate_from code
  # def migrate_from
  # end
end

