# Test that doing a puts of a recursive array, prints out correctly.
#
# This version of the test uses STDOUT Trac592a.rb uses StringIO.  This
# version ensures that topaz doesn't insert extraneous newlines.
# Trac592a.rb ensures the puts method works correctly.
require File.expand_path('simple', File.dirname(__FILE__))

# Unset MAGLEV_OPTS so that -d doesn't mess up the output
result = `MAGLEV_OPTS= maglev-ruby -e 'a = [:a, :b, :c]; a<<a; puts a'`
test(result, "a\nb\nc\n[...]\n", "recursive case")

report

# The original test case is below, but the recursive traverse of the FS is
# fine, just the final puts was broken, which the above test captures.


############################################################
# recursion and the Pathname class.
# Submitted by Lukas Domagala

# require 'pathname'

# def recursive_dir(path)
#   current_path = Pathname.new(path)
#   current_files = []
#   current_dirs = []
#   current_path.children.each do |file|
#     if file.directory?
#       current_dirs << file
#     else
#       current_files << file
#     end
#   end

#   current_dirs.each do |file|
#     recursive_dir(file)
#     @dirs << file.realpath
#   end

#   current_files.each do |file|
#     @files << file.realpath
#   end
# end

#     @dirs = []
#     @files = []
#     recursive_dir(".")
# @dirs << @dirs
# puts @dirs
# #    puts "Directories", "---", @dirs
# #    puts "Files", "---", @files

