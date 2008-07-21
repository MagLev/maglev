require 'rx/reader'
require 'rx/rexml'

class L

  attr_reader :pi_count, :element_count, :the_count, :para_count, :jpg_count
  
  def initialize
    @pi_count = 0
    @element_count = 0
    @the_count = 0
    @para_count = 0
    @jpg_count = 0
  end

  def doctype(name, pub_sys, long_name, uri)
  end

  def cdata(x)
  end
  def comment(x)
  end

  def instruction(name, instruction)
    @pi_count += 1
  end

  def tag_end(name)
  end

  def tag_start(name, attrs)
    @element_count += 1
    if name == 'p'
      @para_count += 1
    end
    if name == 'img' && attrs['src'] =~ /\.jpg$/
      @jpg_count += 1
    end
  end

  def text(text)
    @the_count += text.scan(/\Wthe\W/).size
  end

  def report
    puts "PIs        #{@pi_count}"
    puts "Elements   #{@element_count}"
    puts "Paragraphs #{@para_count}"
    puts "Jpegs      #{@jpg_count}"
    puts "'the'      #{@the_count}"
  end

end

def test_rx(utf8)
l = L.new
f = "/opt/gemstone/maglev/src/rx/utf#{utf8 ? 8 : 16}.xml"
r = RX::Reader.new(File.open(f, "r"), RX::RXToStreamListener.new(l))
$s = Time.now.to_f
r.go
puts ((Time.now.to_f - $s) * 1000)
l.report
end
