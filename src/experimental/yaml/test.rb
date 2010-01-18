require 'libtest'

c_parser = LibTest.test_version_data
parser   = LibTest::Parser.new(c_parser)
puts "parser:      #{parser.inspect} (#{parser.class})"
puts "parser.type: #{parser.type} (#{parser.type.class})"

data = parser[:data]
puts "data:        #{data.inspect} (#{data.class})"

version = data[:version_directive]
puts "version:     #{version.inspect} (#{version.class})"
puts "version[:major]: #{version[:major]}  #{version.major}"
puts "version[:minor]: #{version[:minor]}  #{version.minor}"
puts "version.version: #{version.version}"
puts "sequence_start_event: major: #{parser.version.inspect}"

# p parser.type

# case parser.type
# when LibTest::ParserEventEnum[:scalar_event]
#   puts "scalar_event:  value: #{parser.scalar_value}"
# when LibTest::ParserEventEnum[:sequence_start_event]
#   puts "sequence_start_event: major: #{parser.data.version_directive.major}"
#   puts "sequence_start_event: minor: #{parser.data.version_directive.minor}"
# else
#   puts "test.rb: Unknown event type: #{parser[:type]}"
# end


