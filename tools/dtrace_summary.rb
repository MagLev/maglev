#!/usr/bin/env ruby
#
# Summarize the dtrace output from rubymethods.d

$:.unshift File.dirname(__FILE__)
require 'mri_info'
require 'methods'

class GsDtrace
  attr_reader :methods

  # Create and return a new Methods object initialized from the dtrace data
  # in +file+.
  def self.methods_from_file(file)
    saw_data = false
    methods = Methods.new
    File.open(file) do |f|
      # The lines with real data start with a '|' character and include the
      # module or class name, method name and count
      f.grep(/^\|/).each do |line|
        saw_data = true
        cname, mname, count = line.split[1,3]
        methods.add(cname, mname, count) if MRIInfo.mri?(cname, mname)
      end
    end
    puts "\n==========\nWarning: No data lines in #{file}\n==========" unless saw_data
    methods
  end
end

if $0 == __FILE__
  trace = GsDtrace.methods_from_file(ARGV[0])
  trace.pprint
end
