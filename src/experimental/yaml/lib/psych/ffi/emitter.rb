module Psych
  class Handler
  end

  # This class represents psych's emitter.c
  #
  # RxINC: need to clean up the @emitter: Waiting on ephemeron support.
  class Emitter < Handler
    def initialize(io)
      puts "--  initialize"
      @io = io
      @emitter = Psych::LibPsych.create_emitter
    end

    def start_stream(encoding)
      puts "--  start_stream(#{encoding})"
      Psych::LibPsych.emit_start_stream(@emitter, encoding)
      self
    end

    def end_stream
      puts "--  end_stream"
      # TODO
    end

    #   version         # => [1, 1]
    #   tag_directives  # => [["!", "tag:tenderlovemaking.com,2009:"]]
    #   implicit        # => false
    def start_document(version, tag_directives, implicit)
      puts "--  start_document(#{version.inspect}, #{tag_directives.inspect}, #{implicit.inspect})"

      wrap_tag_directives(tag_directives)
      Psych::LibPsych.emit_start_document(@emitter,
                                          wrap_version(version),
                                          wrap_tag_directives(tag_directives),
                                          tag_directives.length,
                                          implicit ? 1 : 0)
      self
    end

    def wrap_version(version)
      version = [1,1] if version.nil? or version.empty?
      version_ptr = FFI::MemoryPointer.new(:int, version.length)
      version_ptr.write_array_of_int(version);
      version_ptr
    end

    def wrap_tag_directives(tag_directives)
      directives = tag_directives.flatten
      raise ArgumentError "tag tuple must be of length 2" unless directives.length % 2 == 0
      if directives.size > 0
        strptrs = []
        directives.each {|s| strptrs << FFI::MemoryPointer.from_string(s) }
        strptrs.each {|p| p p}
        directives_ptr = FFI::MemoryPointer.new(:pointer, directives.length)
        directives_ptr.write_array_of_pointer(strptrs)

        directives_ptr_ptr = FFI::MemoryPointer.new(:pointer)
        directives_ptr_ptr.write_pointer(directives_ptr)
      else
        directives_ptr_ptr = nil
      end
      puts "== directives_ptr:     #{directives_ptr.inspect}  [0]: #{directives_ptr[0].to_s(16)}"
      puts "== directives_ptr_ptr: #{directives_ptr_ptr.inspect} [0]: #{directives_ptr_ptr[0].to_s(16)}"
      directives_ptr_ptr
    end

    def wrap_anchor(anchor)

    end

    def end_document(implicit)
      puts "--  end_document"
      # TODO
    end

    def scalar(value, anchor, tag, plain, quoted, style)
      puts "--  scalar"
      Psych::LibPsych.emit_scalar(@emitter,
                                  wrap_value(value),
                                  wrap_anchor(anchor),
                                  wrap_tag(tag),
                                  wrap_plain(plain),
                                  wrap_quoted(quoted),
                                  wrap_style(style))
      self
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
