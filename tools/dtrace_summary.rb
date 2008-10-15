#!/usr/bin/env ruby
#
# Summarize the dtrace output from rubymethods.d
#


# Assume mri_info.rb is in the same dir as this file...
require File.join(File.dirname(__FILE__), 'mri_info.rb')
include MRIInfo


# Print a hash of hashes in a human readable format.
# Wrap lines at 80 columns.
def pprint(h)
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

counts = Hash.new { |h,k| h[k] = Hash.new }
# The lines with real data start with a '|' character and include the
# module or class name, method name and count
ARGF.grep(/^\|/).each do |line|
  cname, mname, count = line.split[1,3]
  counts[cname][mname] = count if mri?(cname, mname)
end

pprint counts




