require 'libpsych'
class StubHandler
  def start_stream encoding
    puts "Start of stream"
  end

  def start_document version, tag_directives, implicit
    puts "document start event"
  end

  def end_document implicit
    puts "document end event"
  end

  def alias anchor
    puts "alias event"
  end

  def scalar value, anchor, tag, plain, quoted, style
    puts "scalar event"
  end

  def start_sequence anchor, tag, implicit, style
    puts "sequence start event"
  end

  def end_sequence
    puts "sequence end event"
  end

  def start_mapping anchor, tag, implicit, style
    puts "mapping start event"
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
yaml = "%YAML 1.1\n%TAG ! tag:gemstone.com,2009:\n--- !squee\n"

parser = Psych::Parser.new
parser.parse(yaml, h)
