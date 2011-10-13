require File.expand_path('simple', File.dirname(__FILE__))

# This is a complicated example taken from rubygems-1.3.1:
OPS = {
  "="  =>  lambda { |v, r| v == r },
  "!=" =>  lambda { |v, r| v != r },
  ">"  =>  lambda { |v, r| v > r },
  "<"  =>  lambda { |v, r| v < r },
  ">=" =>  lambda { |v, r| v >= r },
  "<=" =>  lambda { |v, r| v <= r },
  "~>" =>  lambda { |v, r| v >= r && v < r.bump }
}
OP_RE = /#{OPS.keys.map{ |k| Regexp.quote k }.join '|'}/o
#puts "OP_RE:  #{OP_RE.inspect}"

def match_it(obj)
  case obj
  when /^\s*(#{OP_RE})\s*([0-9.]+)\s*$/o then
    "A"
  when /^\s*([0-9.]+)\s*$/ then
    "B"
  when /^\s*(#{OP_RE})\s*$/o then
    "C"
  else
    "F"
  end
end

test(match_it(">= 0"), "A", "complicated regexp")

report
#################### Trac Info
# ID:         268
# Summary:    Regexp failure in rubygems-1.3.1
# Changetime: 2009-11-19 19:59:02+00:00
###

#  See the file src/test/TracXXX.rb (where XXX will be the trac number for this ticket):
#  {{{
#  require File.expand_path('simple', File.dirname(__FILE__))
#  
#  # This is a complicated example taken from rubygems-1.3.1:
#  OPS = {
#    "="  =>  lambda { |v, r| v == r },
#    "!=" =>  lambda { |v, r| v != r },
#    ">"  =>  lambda { |v, r| v > r },
#    "<"  =>  lambda { |v, r| v < r },
#    ">=" =>  lambda { |v, r| v >= r },
#    "<=" =>  lambda { |v, r| v <= r },
#    "~>" =>  lambda { |v, r| v >= r && v < r.bump }
#  }
#  OP_RE = /#{OPS.keys.map{ |k| Regexp.quote k }.join '|'}/o
#  #puts "OP_RE:  #{OP_RE.inspect}"
#  
#  def match_it(obj)
#    case obj
#    when /^\s*(#{OP_RE})\s*([0-9.]+)\s*$/o then
#      "A"
#    when /^\s*([0-9.]+)\s*$/ then
#      "B"
#    when /^\s*(#{OP_RE})\s*$/o then
#      "C"
#    else
#      "F"
#    end
#  end
#  
#  test(match_it(">= 0"), "A", "complicated regexp")
#  
#  report
#  
#  }}}
#  
#  
#  MRI matches the first case, "A", maglev goes to the else clause.