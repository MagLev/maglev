# Random is the Smalltalk class RubyRandom
#
class Random
  class_primitive_nobridge 'new'
  primitive_nobridge 'next', 'nextInt:'
  primitive_nobridge 'next'
  primitive_nobridge '_seed', 'seed:'

  def seed
    @_st_seed
  end
  def seed(v)
    v = Type.coerce_to(v, Fixnum, :to_int)
    @_st_seed = v
    self._seed(v)
  end
     
end

class RandomNp < Random
  # Smalltalk class RubyRandomNp
  # instances may not be committed
end

# transient constant RandomInstance , instance of RandomNp,
# defined in delta/Object3.rb 

