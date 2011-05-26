# Random is the Smalltalk class TransientRandom
#
# We use TransientRandom so that the Ruby persistent instance,
# RandomInstance, is still usable in the face of abort_transaction.
class Random
  class_primitive_nobridge 'new'
  primitive_nobridge 'next', 'nextInt:'
  primitive_nobridge 'next'
  primitive_nobridge 'seed', 'seed:'
  primitive_nobridge 'seed', 'seed'
end
RandomInstance = Random.new

