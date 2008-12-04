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
