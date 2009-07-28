# From YAML: Looks like doing a for loop over the key/value pairs in a Hash
# does not setup the correct handler for RubyBreakException.
#
# This test passes if there are no exceptions.

regexes =  {
  'foo' => /foo/,
  'tag' => Regexp.new('^!ruby/object:'),
  'bar' => /bar/
}


a = [ 9 ]
x = a.each do |x|  
  break 20
end
unless x == 20 ; raise 'error'; end

x = regexes.each do |x|
  break 21
end
unless x == 21 ; raise 'error'; end

x = regexes.each_value do |x|
  break 22
end
unless x == 22 ; raise 'error'; end

x = regexes.each_pair do |x,y|
  break 23
end
unless x == 23 ; raise 'error'; end

x = regexes.each_key do |x|
  break 24
end
unless x == 24 ; raise 'error'; end

x = for key,value in regexes do
  break 25
end
unless x == 25 ; raise 'error'; end
true
