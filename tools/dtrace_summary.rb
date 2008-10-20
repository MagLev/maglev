#!/usr/bin/env ruby
#
# Summarize the dtrace output from rubymethods.d

$:.unshift File.dirname(__FILE__)
require 'mri_info'


class GsDtrace
  include MRIInfo

  attr_reader :counts

  def initialize(file)
    @counts = hash_for_file(file)
  end

  # Returns a hash of hashes for all the counts in +dtrace_file+, a file
  # generated via the dtrace script +rubymethods.d+ in this directory.
  # To access the counts:
  #
  #    h = hash_for_file('rails.dtrace')
  #    class_name = :String
  #    method_name = "==="
  #    count = h[class_name][method_name]
  #
  def hash_for_file(file)
    counts = Hash.new { |h,k| h[k] = Hash.new }
    File.open(file) do |f|
      # The lines with real data start with a '|' character and include the
      # module or class name, method name and count
      f.grep(/^\|/).each do |line|
        cname, mname, count = line.split[1,3]
        counts[cname][mname] = count if mri?(cname, mname)
      end
    end
    counts
  end

  # Print a hash of hashes in a human readable format.
  # Wrap lines at 80 columns.
  def pprint
    GsDtrace.pprint_hash(@counts)
  end

  # Pretty print a hash of hashes.  For each key in the hash, prints the
  # sorted list of sub-keys and wraps at 80 characters.
  def self.pprint_hash(h)
    start_col = 17
    width = 80 - start_col

    h.sort.each do |k,v|
      printf "%-15s", k
      cur_col = start_col

      v.keys.sort.each do |k|
        needed = k.length + 1
        if cur_col + needed > width
          printf "\n%s", " " * 15
          cur_col = start_col
        end
        printf "%s ", k
        cur_col += needed
      end
      printf "\n\n"
    end
  end
end

if $0 == __FILE__
  trace = GsDtrace.new(ARGV[0])
  trace.pprint
end



