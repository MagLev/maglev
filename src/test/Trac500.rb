
# sub was not doing \1 replacement...
r = "hello".sub /h([aeiou])llo/, '\1'
raise "Expected 'e' but got #{r}" unless r == 'e'
#################### Trac Info
# ID:         500
# Summary:    String#sub not processing special substitution characters
# Changetime: 2009-04-27 17:31:36+00:00
###

#  This is used heavily in Rails file name processing.
#  
#  MBP:feature_tests lattam$ irb
#  >> "hello".sub /h([aeiou])llo/, '\1'
#  => "e"
#  >> quit
#  MBP:feature_tests lattam$ maglev-irb
#  irb(main):001:0> "hello".sub /h([aeiou])llo/, '\1'
#  => "\\1"
#  
#  