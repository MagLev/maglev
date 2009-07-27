# From YAML: Looks like doing a for loop over the key/value pairs in a Hash
# does not setup the correct handler for RubyBreakException.
#
# This test passes if there are no exceptions.

regexes =  {
  'foo' => /foo/,
  'tag' => Regexp.new('^!ruby/object:'),
  'bar' => /bar/
}


# Each of the following loop forms is broken in MagLev:

regexes.each_value do |x|
  break
end

regexes.each_pair do |x,y|
  break
end

regexes.each_key do |x|
  break
end

regexes.each do |x|
  break
end

for key,value in regexes do
  break
end
