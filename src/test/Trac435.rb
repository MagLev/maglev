
class String
  @civ_435 = 'trac435'

  def self.get435
    @civ_435
  end
end

unless String.get435 == 'trac435' ; raise 'err' ; end
true
#################### Trac Info
# ID:         435
# Summary:    Problem with aliasing from Smalltalk classes
# Changetime: 2009-04-21 16:55:33+00:00
###

#  There is a fundamental problem with aliasing Smalltalk classes to use as Ruby classes. There are things that are assumed to be possible with Ruby classes that the Smalltalk classes prevent.
#  
#  The current issue is that adding a class instvar to the String class fails during Ruby boot because it ends up trying to modify the class inst var dictionary of String which is access controlled.
#  
#  See active_support/core_ext/class/inheritable_attributes.rb