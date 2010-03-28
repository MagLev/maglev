require 'optparse'

# MagLev bug: doesn't recognize shortened forms of options e.g., rubygems
# has the "--prerelease" option, which most people shorten to "--pre".
# MagLev chokes on it.

ARGV << "--pre"
options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: example.rb [options]"

  opts.on("--prerelease", "Install prerelease version") do |value|
    options[:prerelease] = true
  end
end.parse!

raise "Fail" unless options[:prerelease]
