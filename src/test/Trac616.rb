# From the pure ruby json gem
module JSON
  module TrueClass
    # Returns a JSON string for true: 'true'.
    def to_json(*); 'true' end
  end
end

TrueClass.class_eval do
  include JSON::TrueClass
end

json_true = true.to_json
raise "Expecting 'true' but got #{json_true.inspect}" unless json_true == "true"

puts "Done"
true

# ##########################################################################
# This code also fails, but I believe it to be due to the underlying cause
# from above (String#to_json isn't defined, so it defaults up to Object).
#
# require 'rubygems'
# require 'iconv'
# require 'json'

# json = [:foo].to_json
# raise "Expected \"[\"foo\"]\" but got #{json.inspect}" unless json == "[\"foo\"]"


#################### Trac Info
# ID:         616
# Summary:    JSON can't deal with symbols in maglev ;   TrueClass,FalseClass  problems
# Changetime: 2009-09-30 16:14:53+00:00
###

#  {{{
#  require 'rubygems'
#  require 'iconv'
#  require 'json'
#  
#  json = [:foo].to_json
#  raise "Expected \"[\"foo\"]\" but got #{json.inspect}" unless json == "[\"foo\"]"
#  
#  }}}
#  
#  The error:
#  
#  
#  {{{
#  error , Illegal creation of a Symbol,
#            during /Users/pmclain/GemStone/dev/pbm.rb
#  
#  }}}
#  