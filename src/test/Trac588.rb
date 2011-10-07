# This test passes if it doesn't raise an exception
X ||= 10 if defined? X
#################### Trac Info
# ID:         588
# Summary:    new parser problem
# Changetime: 2009-08-18 18:22:05+00:00
###

#  The following blows up in the new parser:
#  
#  {{{
#  # Assume X is not defined
#  X ||= 10 if defined? X
#  }}}
#  
#  The error:
#  
#  {{{
#  $ mruby src/test/TracXXX.rb
#  -- RubyFile>>load  : loading /Users/pmclain/GemStone/snapshots/MagLev-2009-08-17/src/test/TracXXX.rb
#  error , Undefined method ,
#            during /Users/pmclain/GemStone/snapshots/MagLev-2009-08-17/src/test/TracXXX.rb
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  Undefined method 
#  Error Category: 231169 [GemStone] Number: 2010 Arg Count: 4 Context : 104957697
#  Arg 1: [20 sz:0 cls: 76289 UndefinedObject] nil
#  Arg 2: [80728833 sz:6 cls: 110849 Symbol] c2node
#  Arg 3: [104957441 sz:0 cls: 66817 Array] anArray
#  Arg 4: [10 sz:0 cls: 74241 SmallInteger] 1 == 0x1
#  topaz 1> 
#  }}}
#  