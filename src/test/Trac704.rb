
require 'yaml'

def verify_objs(orig, nobj )
  unless orig.class.equal?( nobj.class) ; raise "bad class: #{nobj.class}  expecting: #{orig.class}";end
  unless orig == nobj ; raise "orig (#{orig.inspect}) not equal to nobj (#{nobj.inspect})"; end
end

set = IdentitySet.new
(1010..1014).each { | o |
  set << o
}
nset = YAML::load( YAML::dump(set) )
verify_objs(set, nset)

ih = IdentityHash.from_hash( { 33 =>'three' , 44=>'four', 55=>'five' } )
nih = YAML::load( YAML::dump( ih ) )
verify_objs(ih, nih)

h = { 'abc'=>55 , 'def'=>66 } 
nh = YAML::load( YAML::dump( h ))
verify_objs(h, nh)

a = [101, 102 , 103, 104 , 105] 
na = YAML::load( YAML::dump( a ))
verify_objs(a, na)

puts "dumped ok"
true
