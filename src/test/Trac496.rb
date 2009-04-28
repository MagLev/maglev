require 'pathname'

# File.open was having problems with non-string objects being passed in.
p = Pathname(__FILE__)

lines = File.open(p).readlines
raise 'Fail' unless lines.size > 0

