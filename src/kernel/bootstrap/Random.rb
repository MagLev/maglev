# Random is the Smalltalk class Random

class Random
  class_primitive_nobridge 'new'
  primitive_nobridge 'next', 'nextInt:'
  primitive_nobridge 'next'
end
RandomInstance = Random.new

