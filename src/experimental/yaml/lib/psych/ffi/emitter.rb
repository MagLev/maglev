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
      @emitter_context =
        Psych::LibPsych::EmitterContext.new Psych::LibPsych.create_emitter_context
    end

    def start_stream(encoding)
      puts "--  start_stream(#{encoding})"
      do_emit(Psych::LibPsych.emit_start_stream(@emitter_context.emitter, encoding))
      self
    end

    def end_stream
      puts "--  end_stream"
      do_emit(Psych::LibPsych.emit_end_stream(@emitter_context.emitter))
      self
    ensure
      Psych::LibPsych.free_emitter_context(@emitter_context)
      @emitter_context = nil
    end

    #   version         # => [1, 1]
    #   tag_directives  # => [["!", "tag:tenderlovemaking.com,2009:"]]
    #   implicit        # => false
    def start_document(version, tag_directives, implicit)
      puts "--  start_document(#{version.inspect}, #{tag_directives.inspect}, #{implicit.inspect})"

      wrap_tag_directives(tag_directives)
      do_emit(Psych::LibPsych.emit_start_document(@emitter_context.emitter,
                                                       wrap_version(version),
                                                       wrap_tag_directives(tag_directives),
                                                       tag_directives.length,
                                                       implicit ? 1 : 0))
      self
    end

    def end_document(implicit)
      puts "--  end_document"
      do_emit(Psych::LibPsych.emit_end_document(@emitter_context.emitter, implicit ? 1 : 0))
      self
    end

    def scalar(value, anchor, tag, plain, quoted, style)
      puts "--  scalar"
      do_emit(Psych::LibPsych.emit_scalar(@emitter_context.emitter,
                                               value, value.length,
                                               wrap_string_or_nil(anchor),
                                               wrap_string_or_nil(tag),
                                               plain  ? 1 : 0, quoted ? 1 : 0, style))
      self
    end

    def start_sequence(anchor, tag, implicit, style)
      puts "--  start_sequence"
      do_emit(Psych::LibPsych.emit_start_sequence(@emitter_context.emitter,
                                                  wrap_string_or_nil(anchor),
                                                  wrap_string_or_nil(tag),
                                                  implicit ? 1 : 0,
                                                  style))
      self
    end

    def end_sequence
      puts "--  end_sequence"
      do_emit(Psych::LibPsych.emit_end_sequence(@emitter_context.emitter))
      self
    end

    def start_mapping(anchor, tag, implicit, style)
      puts "--  start_mapping"
      do_emit(Psych::LibPsych.emit_start_mapping(@emitter_context.emitter,
                                                 wrap_string_or_nil(anchor),
                                                 wrap_string_or_nil(tag),
                                                 implicit ? 1 : 0,
                                                 style))
      self
    end

    def end_mapping
      puts "--  end_mapping"
      do_emit(Psych::LibPsych.emit_end_mapping(@emitter_context.emitter))
      self
    end

    def alias(anchor)
      puts "--  alias"
      do_emit(Psych::LibPsych.emit_alias(@emitter_context.emitter,
                                         wrap_string_or_nil(anchor)))
      self
    end

    # Check the error status of one of the libyaml emitter functions.
    # If there is accummulated output, then push it into our IO object
    def do_emit(status)
      if status == 0
        raise Psych::LibPsych.get_error_string(@emitter_context.emitter)
      end
      @emitter_context.flush(@io)
    end

    def wrap_string_or_nil(string)
      string.nil? ? FFI::MemoryPointer::NULL :
                    FFI::MemoryPointer.from_string(string)
    end

    def wrap_version(version)
      version = [1,1] if version.nil? or version.empty?
      version_ptr = FFI::MemoryPointer.new(:int, version.length)
      version_ptr.write_array_of_int(version);
      puts "--- version_ptr: #{version_ptr.inspect}"
      version_ptr
    end

    # Given tag_pointers: [ ["!", ":gemstone:"], ... ], Return a
    # MemoryPointer representing the char ** of the flattened tag_pointers.
    def wrap_tag_directives(tag_directives)
      directives = tag_directives.flatten
      raise ArgumentError "tag tuple must be of length 2" unless directives.length % 2 == 0
      if directives.size > 0
        strptrs = []
        directives.each {|s| strptrs << FFI::MemoryPointer.from_string(s) }
        strptrs.each {|p| p p}
        directives_ptr = FFI::MemoryPointer.new(:pointer, directives.length)
        directives_ptr.write_array_of_pointer(strptrs)
      else
        directives_ptr = FFI::MemoryPointer::NULL
      end
      directives_ptr
    end

#     def wrap_anchor(anchor)

#     end
  end
end
