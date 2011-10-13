# This test passes by not blowing up...
require File.expand_path('simple', File.dirname(__FILE__))

module ModuleSpec
  module ::ModuleSpec
  end
end

# test edited to conform to fix of Trac 672
x = nil
begin
  mm = ModuleSpec::ModuleSpec.class
  raise 'error, ModuleSpec::ModuleSpec should not be defined'
rescue NameError
  # ok
  x = 260
end
unless x == 260 ; raise 'error'; end
true
#################### Trac Info
# ID:         260
# Summary:    Opening module within itself broken
# Changetime: 2008-11-25 17:25:39+00:00
###

#  The following works in MRI, but broken in MagLev.  Distilled from rubyspec/1.8/core/module/fixtures/classes.rb:
#  
#  
#  {{{
#  module ModuleSpec
#    module ::ModuleSpec
#    end
#  end
#  }}}
#  
#  Error:
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb 
#  topaz 1> error during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  No method was found for the selector #'asClassNameNode' when sent
#  to aRubyColon3Node with arguments contained in anArray( ).
#  Error Category: [GemStone] Number: 2010 Arg Count: 4
#  Arg 1: aRubyColon3Node
#  Arg 2: asClassNameNode
#  Arg 3: anArray
#  Arg 4: 0
#  
#  }}}
#  
#  