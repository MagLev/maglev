# Distilled from psych: the new YAML lib for Ruby
class Cx
  def to_yaml_properties
    true
  end
  def mx
    55
  end
  def my
    66
  end
end
# Trac 653: lexer has problem if the : in the ? expression is followed
#  immediately by EOL 
o = Cx.new
z = o.respond_to?(:to_yaml_properties) ?
  o.mx :
  o.my
unless z == 55 ;  'raise error'; end
true
#################### Trac Info
# ID:         653
# Summary:    Parse error on multiline ? :
# Changetime: 2010-01-22 20:18:44+00:00
###

#  MRI is fine with this; MagLev gets an unexpected ":":
#  
#  
#  {{{
#  o = Object.new
#  ivars = o.respond_to?(:to_yaml_properties) ?
#    o.object_id :
#    o.instance_variables
#  p ivars
#  }}}
#  
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb
#  parse error on value :":"  tSYMBEG 
#  SyntaxError: missing or unexpected :":" , near line 4 
#  error , SyntaxError,
#            during /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb
#  ERROR 2023, Error, 'SyntaxError' (SyntaxError)
#  [@bach yaml (dev)]$ 
#  
#  }}}
#  
#  