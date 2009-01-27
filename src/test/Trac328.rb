# From the mspec framework:

def os?(*oses)
  puts "os?(#{oses.inspect})"
  raise "Expecting an array" unless oses.kind_of? Array
  oses.any? do |os|
    puts "Looking at #{os}"
  end
end

options = { :os => :mswin }
options.each do |key, value|
  os?(*value)
end
