# From YAML: Looks like doing a for loop over the key/value pairs in a Hash
# does not setup the correct handler for RubyBreakException.
regexes =  {
  'foo' => /foo/,
  'tag' => Regexp.new('^!ruby/object:'),
  'bar' => /bar/
}

for tag,reg in regexes
  if reg =~ '!ruby/object:'
    break
  end
end
