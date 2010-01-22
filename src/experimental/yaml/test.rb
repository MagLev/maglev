require 'libtest'

puts "-- Creating data"
data = [ LibTest::Parser.new( LibTest.test_version_data ),
         LibTest::Parser.new( LibTest.test_scalar_data )
       ]

puts "-- Swith on data"
data.each do |d|
  puts "---- TYPE: #{d.type}"
  case d.type
  when :stream_start_event
    puts "    stream start event"
    puts "    version: #{d.version.inspect}"
  when :scalar_event
    puts "    scalar event"
    s = d[:data][:scalar][:value]
    puts "    value: #{s}"
    puts "    length: #{d[:data][:scalar][:length]}"
    puts "    s.length: #{s.length}"
  else
    puts "    uknown type: #{d.type}"
  end
end

