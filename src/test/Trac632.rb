# From rdoc 1.8/rdoc/markup/simple_markup/inline.rb
#
# $1 is initially "", but MATCHING_WORD_PAIRS[$2] seems to set it to nil.

str = "_stuff_ "

tags = "_*+"
re = "(^|\\W)([#{tags}])([A-Za-z_]+?)\\2(\\W|\$)"

MATCHING_WORD_PAIRS = {}
1 while str.gsub!(Regexp.new(re)) {
  x = $1
  MATCHING_WORD_PAIRS[$2]
  y = $1
  raise "x: #{x.inspect}  y: #{y.inspect}" if y.nil?
}
