# Distilled from optparse.rb
#
# The problem is that in the option_list.each loop, each
# same underlying block, so that all of the option handlers get the last block
# rather than their own Proc
class Parser
  def initialize
    @blocks = Array.new
  end

  def on(&block)
    @blocks << block
  end

  def parse!
    @blocks.each do |v|
      v.call(:foo)
    end
  end
end

option_list = [
  Proc.new { |a| puts "--help #{a}"},
  Proc.new { |a| puts "--verbose #{a}"},
  Proc.new { |a| puts "--rdoc #{a}"},
  Proc.new { |a| puts "--ri #{a}"},
]

# each handler works correctly:
# option_list.each { |handler| handler.call("xxx") }

parser = Parser.new

option_list.each do |handler|
  parser.on { |value| handler.call(value) }
end

parser.parse!


######################################################################
#
# The following commented out code reproduces the same problem in
# the original context of optparse
#
######################################################################

# require 'optparse'

# options = { }
# parser = OptionParser.new
# parser.separator("")

# option_list = [
#   [["-h", "--help", "Get help on this command"],                Proc.new { |a| puts "--help #{a}"}],
#   [["-V", "--[no-]verbose", "Set the verbose level of output"], Proc.new { |a| puts "--verbose #{a}"}],
#   [["--[no-]rdoc", "Set the rdoc flag"],                        Proc.new { |a| puts "--rdoc #{a}"}],
#   [["--[no-]ri", "Set the ri flag"],                            Proc.new { |a| puts "--ri #{a}"}],
# ]

# option_list.each do |args, handler|
#   puts "Iteration for #{args.inspect}  handler: #{handler}"
#   parser.on(*args) do |value|
#     puts "In Block  handler: #{handler.__id__}"
#     handler.call(value)
#   end
# end

# parser.parse!(["--no-rdoc", "--no-ri"])




