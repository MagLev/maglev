
$:.unshift(File.dirname(__FILE__))

begin
  require 'Trac757a'
rescue Foo::NOT_FOUND
  raise "Fail: In rescue clause"
end
puts "OK"
