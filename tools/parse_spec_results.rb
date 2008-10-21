#!/usr/bin/env ruby
#
# This script attempts to show the methods used by a program, e.g., rails,
# whose specs are not currently passing.  It require two files (A) the
# foo.dtrace file (the output of a previous run of dtrace_summary.rb) and
# (B) the output of a recent spec run.
#
# Usage:  parse_spec_results rails.dtrace.sum passed-Darwin.txt

$:.unshift File.dirname(__FILE__)

require 'dtrace_summary'
require 'mri_info'

app_counts   = ARGV[0]  # The summary of dtrace output
spec_results = ARGV[1]

if ARGV.length != 2
  puts "Usage: #{File.basename(__FILE__)}: dtrace_file spec_result"
  exit 1
end

passing = Methods.new

File.open(spec_results) do |f|
  f.each do |line|
    if line =~ /\s*(.+)\/(.+)\/(.+)_spec.rb/
      klass = $2.capitalize
      method = $3
      passing.add(klass, method) if MRIInfo.mri?(klass, method)
    end
  end
end

app_methods = GsDtrace.methods_from_file(app_counts)
not_passing = app_methods - passing
puts "==================== Failing Specs ===================="
puts "=== App File:  #{app_counts}"
puts "=== Spec File: #{spec_results}"
puts "=== Date run:  #{Time.now}"
puts "======================================================="
#passing.pprint
#app_methods.pprint
(app_methods - passing).pprint
