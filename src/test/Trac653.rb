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
