class C
  TM = true
  FM = false  # next line must end with ?
  xx = TM ?
       5 : 6
  yy = FM ?  # white space after ?
       8 : 9
  unless xx == 5 ; raise 'error';end
  unless yy == 9 ; raise 'error';end
  puts "done 584"
end
true
#################### Trac Info
# ID:         584
# Summary:    Parse Error
# Changetime: 2009-08-11 16:56:06+00:00
###

#  The new ruby parser has a problem with this file:
#  
#  
#  {{{
#  $ maglev-ruby lib/ruby/1.8/rdoc/parsers/parse_f95.rb 
#  parse error on value (RpNameToken pre_comment @26190) nil 
#  SyntaxError: missing or unexpected (RpNameToken pre_comment @26190) , near line 729 
#  error , SyntaxError,
#            during /Users/pmclain/GemStone/snapshots/MagLev-2009-08-10/lib/ruby/1.8/rdoc/parsers/parse_f95.rb
#  -----------------------------------------------------
#  GemStone: Error         Nonfatal
#  Error, 'SyntaxError'
#  Error Category: 231169 [GemStone] Number: 2023 Arg Count: 1 Context : 150051329
#  Arg 1: [150052865 sz:11 cls: 74753 String] SyntaxError
#  topaz 1> 
#  
#  }}}
#  