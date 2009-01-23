#  [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' |
#                  CombiningChar | Extender
#  [5] Name     ::= (Letter | '_' | ':') (NameChar)*
# [84] Letter   ::= BaseChar | Ideographic


require 'set'

def make_char_classes(support = STDOUT)

  support.puts "  class CharClass"
  support.puts "    def CharClass.load_char_classes"
  support.puts "      @@points = {}"
  support.puts "      @@ranges = {}"

  digit = XMLChars.new('Digit')
  combining = XMLChars.new('CombiningChar')
  extender = XMLChars.new('Extender')
  base = XMLChars.new('BaseChar')
  ideographic = XMLChars.new('Ideographic')

  letter = XMLChars.new
  letter.merge! base
  letter.merge! ideographic

  # NameC
  name_char = XMLChars.new
  # no ':' for namespace reasons
  name_char.points = [ '.'[0], '-'[0], '_'[0] ]
  name_char.merge! letter
  name_char.merge! digit
  name_char.merge! combining
  name_char.merge! extender
  name_char.print('NameChar', support)

  # NameStart
  name_start = XMLChars.new
  name_start.points = [ '_'[0] ]
  name_start.merge! letter
  name_start.print('NameStart', support)

  support.print "    end\n\n"
  support.print "  end\n\n"
end

class XMLChars
  attr_accessor :points, :ranges

  def initialize(name = nil)
    @points = []
    @ranges = []

    return unless name
    
    x = File.read("cc/#{name}")
    x = x.strip.split(/\s*\|\s*/)
    x.each do |cr|
      if cr =~ /^\[.x(....)-.x(....)\]$/
        @ranges << "(0x#{$1}..0x#{$2})"
      elsif cr =~ /^.x(....)$/
        @points << "0x#{$1}"
      else
        print "Bogus datum #{cr}"
      end
    end
  end

  def merge! other
    @points = (Set.new(@points) | Set.new(other.points)).to_a
    @ranges = (Set.new(@ranges) | Set.new(other.ranges)).to_a
  end

  def print(name, out)
    on_line = 0
    out.print "      @@points['#{name}'] = [\n        "
    @points.each do |p|
      out.print "#{p}, "
      on_line += 1
      if on_line == 8
        out.print "\n        "
        on_line = 0
      end
    end
    out.print "\n      ]\n"
    out.print "      @@ranges['#{name}'] = [\n        "
    on_line = 0
    @ranges.each do |r|
      out.print "#{r}, "
      on_line += 1
      if on_line == 4
        out.print "\n        "
        on_line = 0
      end
    end
    out.print "\n      ]\n\n"
  end
end
