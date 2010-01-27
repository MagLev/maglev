module Psych
  class Handler
  end

  # This class represents psych's emitter.c
  class Emitter < Handler
    def initialize(io)
      @io = io
      puts "--  initialize"
    end

    def start_stream(encoding)
      puts "--  start_stream(#{encoding})"
      Psych::LibPsych.start_stream(encoding)
      self
    end

    def end_stream
      puts "--  end_stream"
      # TODO
    end

    def start_document(version, tag_directives, implicit)
      puts "--  start_document"
      # TODO
    end

    def end_document(implicit)
      puts "--  end_document"
      # TODO
    end

    def scalar(value, anchor, tag, plain, quoted, style)
      puts "--  scalar"
      # TODO
    end

    def start_sequence(anchor, tag, implicit, style)
      puts "--  start_sequence"
      # TODO
    end

    def end_sequence
      puts "--  end_sequence"
      # TODO
    end

    def start_mapping(anchor, tag, implicit, style)
      puts "--  start_mapping"
      # TODO
    end

    def end_mapping
      puts "--  end_mapping"
      # TODO
    end

    def alias(anchor)
      puts "--  alias"
      # TODO
    end
  end
end
