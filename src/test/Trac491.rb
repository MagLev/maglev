require 'yaml'
file_name = File.dirname(__FILE__) + '/lib/zero_length_file'
# Underlying bug:
f = File.open(file_name)
contents = f.read
raise "Fail" unless contents == ""

# Original bug:
file = File.read(file_name)
x = YAML.load(file)

raise "Fail 2" unless x == false

