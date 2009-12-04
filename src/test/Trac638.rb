require 'uri'

sources = ['http://gems.rubyforge.org/',
           'http://gems.rubyforge.org' ]

cache = Hash.new

sources.each do |source_uri|
  uri = URI.parse source_uri
  cache[uri] = uri  # Blows up on second access
end
