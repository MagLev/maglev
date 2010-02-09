# A Psych Handler, used for testing
class TestHandler
  def initialize(verbose = false)
    @verbose = verbose
  end

  def start_stream encoding
    if @verbose
      puts "start of stream"
      puts "  -- encoding: #{encoding.inspect}"
    end
  end

  def start_document version, tag_directives, implicit
    if @verbose
      puts "document start event"
      puts "  -- version:        #{version.inspect}"
      puts "  -- tag_directives: #{tag_directives.inspect}"
      puts "  -- implicit:       #{implicit.inspect}"
    end
  end

  def end_document implicit
    if @verbose
      puts "document end event"
      puts "  -- implicit: #{implicit.inspect}"
    end
  end

  def alias anchor
    if @verbose
      puts "alias event"
      puts "  -- anchor: #{anchor.inspect}"
    end
  end

  def scalar value, anchor, tag, plain, quoted, style
    if @verbose
      puts "scalar event"
      puts "  -- value:  #{value.inspect}"
      puts "  -- anchor: #{anchor.inspect}"
      puts "  -- tag:    #{tag.inspect}"
      puts "  -- plain:  #{plain.inspect}"
      puts "  -- quoted: #{quoted.inspect}"
      puts "  -- style:  #{style.inspect}"
    end
  end

  def start_sequence anchor, tag, implicit, style
    if @verbose
      puts "sequence start event"
      puts "  -- anchor:   #{anchor.inspect}"
      puts "  -- tag:      #{tag.inspect}"
      puts "  -- implicit: #{implicit.inspect}"
      puts "  -- style:    #{style.inspect}"
    end
  end

  def end_sequence
    if @verbose
      puts "sequence end event"
    end
  end

  def start_mapping anchor, tag, implicit, style
    if @verbose
      puts "mapping start event"
      puts "  -- anchor:   #{anchor.inspect}"
      puts "  -- tag:      #{tag.inspect}"
      puts "  -- implicit: #{implicit.inspect}"
      puts "  -- style:    #{style.inspect}"
    end
  end

  def end_mapping
    if @verbose
      puts "mapping end event"
    end
  end

  def empty
    if @verbose
      puts "empty"
    end
  end

  def end_stream
    if @verbose
      puts "end of stream"
    end
  end
end
