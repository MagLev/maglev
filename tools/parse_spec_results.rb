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

include MRIInfo

app_counts   = ARGV[0]  # The summary of dtrace output
spec_results = ARGV[1]

if ARGV.length != 2
  puts "Usage: #{File.basename(__FILE__)}: dtrace_file spec_result"
  exit 1
end

failing_spec_methods = Hash.new { |h,k| h[k] = Hash.new }

File.open(spec_results) do |f|
  f.each do |line|
    if line =~ /\s*(.+)\/(.+)\/(.+)_spec.rb/
      klass = $2.capitalize
      method = $3
      if mri?(klass, method)
        #puts "#{$2} #{$3}: OK"
      else
        failing_spec_methods[klass][method] = 1
        #puts "#{$2} #{$3}: BAD"
      end
    end
  end
end

puts "==================== Failing Specs ===================="
puts "=== App File:  #{app_counts}"
puts "=== Spec File: #{spec_results}"
puts "=== Date run:  #{Time.now}"
puts "======================================================="
GsDtrace.pprint_hash failing_spec_methods
