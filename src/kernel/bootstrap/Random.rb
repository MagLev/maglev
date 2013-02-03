# Random is the Smalltalk class RubyRandom
#

class Random
  class_primitive_nobridge 'new'
  primitive_nobridge '__float', 'float'
  primitive_nobridge '__integer', 'integer'
  primitive_nobridge '__next_int', 'nextInt:'
  primitive_nobridge '__seed', 'seed:'

  def seed
    @_st_seed
  end
  def seed(v)
    if v._not_equal?(nil)
      v = Maglev::Type.coerce_to(v, Fixnum, :to_int)
      @_st_seed = v
      self.__seed(v)
    else
      @_st_seed = nil
    end
    v
  end
     
end

class RandomNp < Random
  # Smalltalk class RubyRandomNp
  # instances may not be committed
end

# transient constant RandomInstance , instance of RandomNp,
# defined in delta/Object3.rb 

