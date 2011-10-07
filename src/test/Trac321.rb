# Distilled from src/lib/rational.rb
class Bignum

  alias power! **
end

v = 1 << 64 
vcl = v.class
vn = vcl.name
unless vn == 'Bignum' ; raise 'error'; end
vp = v.power!(3)
e = 1 << 192 
unless vp == e ; raise 'error'; end
true
#################### Trac Info
# ID:         321
# Summary:    Can't alias Bignum ** ; need Smalltalk class for Bignum
# Changetime: 2009-02-02 22:42:57+00:00
###

#  rational.rb (which is required by date, which is required for several things) tries to alias power! ** in Bignum, which fails:
#  
#  
#  {{{
#  # Distilled from src/lib/rational.rb
#  class Bignum
#  
#    def power!(other)
#    end
#  
#    alias power! **
#  end
#  
#  }}}
#  
#  The error:
#  
#  {{{
#  $ mruby src/test/TracXXX.rb
#  topaz 1> error , alias_method:  no method found for **,
#            during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  Error, 'alias_method:  no method found for **'
#  Error Category: [GemStone] Number: 2023 Arg Count: 1
#  Arg 1: alias_method:  no method found for **
#  topaz 1> 
#  
#  }}}
#  