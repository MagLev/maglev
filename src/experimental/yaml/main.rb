
require 'psych'

class StubHandler
  def start_stream encoding
    puts "start of stream"
    puts "  -- encoding: #{encoding.inspect}"
  end

  def start_document version, tag_directives, implicit
    puts "document start event"
    puts "  -- version:        #{version.inspect}"
    puts "  -- tag_directives: #{tag_directives.inspect}"
    puts "  -- implicit:       #{implicit.inspect}"
  end

  def end_document implicit
    puts "document end event"
    puts "  -- implicit: #{implicit.inspect}"
  end

  def alias anchor
    puts "alias event"
    puts "  -- anchor: #{anchor.inspect}"
  end

  def scalar value, anchor, tag, plain, quoted, style
    puts "scalar event"
    puts "  -- value:  #{value.inspect}"
    puts "  -- anchor: #{anchor.inspect}"
    puts "  -- tag:    #{tag.inspect}"
    puts "  -- plain:  #{plain.inspect}"
    puts "  -- quoted: #{quoted.inspect}"
    puts "  -- style:  #{style.inspect}"
  end

  def start_sequence anchor, tag, implicit, style
    puts "sequence start event"
    puts "  -- anchor:   #{anchor.inspect}"
    puts "  -- tag:      #{tag.inspect}"
    puts "  -- implicit: #{implicit.inspect}"
    puts "  -- style:    #{style.inspect}"
  end

  def end_sequence
    puts "sequence end event"
  end

  def start_mapping anchor, tag, implicit, style
    puts "mapping start event"
    puts "  -- anchor:   #{anchor.inspect}"
    puts "  -- tag:      #{tag.inspect}"
    puts "  -- implicit: #{implicit.inspect}"
    puts "  -- style:    #{style.inspect}"
  end

  def end_mapping
    puts "mapping end event"
  end

  def empty
    puts "empty"
  end

  def end_stream
    puts "end of stream"
  end
end

h = StubHandler.new
yaml = [
#        "%YAML 1.1\n%TAG ! tag:gemstone.com,2009:\n--- !squee\n",
        "- Mark McGwire\n- Sammy Sosa\n- Ken Griffey\n\n",
        "american:\n - Boston Red Sox\n - Detroit Tigers\nnational:\n - New York Mets\n - Chicago Cubs\n\n",
        "---\ntime: 20:30:20\nplayer: Sammy Sosa\naction: strike (miss)...\n",
       ]

# puts "PARSING YAML: #{yaml.inspect}"
# yaml.each do |y|
#   parser = Psych::Parser.new
#   puts
#   puts
#   puts "------ YAML -----------------"
#   puts y
#   puts "-" * 50
#   parser.parse(y, h)
# end

# Test an error case
puts "------ YAML  error case-----------------"
parser = Psych::Parser.new(h)
parser.parse("invoice: foo\nbar:baz\n")
