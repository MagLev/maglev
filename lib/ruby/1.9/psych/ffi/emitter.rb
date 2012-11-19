module Psych

  class Handler
  end

  # This class represents psych's emitter.c
  #
  # TODO: need to clean up the @emitter: Waiting on ephemeron support.
  class Emitter < Handler
    # Creates a new emitter.
    # cannonical defaults to false
    # indentation defaults to 0
    def initialize(io)
      @io = io
      @emitter_callback = Proc.new do |context, buffer, count|
        str = buffer.stringfrom_to(0, count-1)
        bytes_written = @io.write(str)
        unless bytes_written == count
          raise "Only flushed #{bytes_written} of #{count} bytes to #{@io}"
        end
        1  # yaml_write_handler_t should return 1 for success, 0 for failure
      end

      c_emitter_context = Psych::LibPsych.create_emitter_context(@emitter_callback)
      @emitter_context  = Psych::LibPsych::EmitterContext.new(c_emitter_context)
    end

    def start_stream(encoding)
      do_emit(Psych::LibPsych.emit_start_stream(@emitter_context.emitter, encoding))
      self
    end

    def end_stream
      do_emit(Psych::LibPsych.emit_end_stream(@emitter_context.emitter))
      self
    ensure
      Psych::LibPsych.free_emitter_context(@emitter_context)
      @emitter_context = nil
    end

    # If +version+ is nil or empty, then no YAML version is used.  If
    # +version+ is present, then the emitter will output the appropriate
    # version directive, e.g., "%YAML 1.1".
    #
    # version        # => [1, 1]
    # tag_directives # => [["!", "tag:tenderlovemaking.com,2009:"]]
    # implicit       # => false
    def start_document(version, tag_directives, implicit)
      wrap_tag_directives(tag_directives)
      do_emit(Psych::LibPsych.emit_start_document(@emitter_context.emitter,
                                                  wrap_version(version),
                                                  wrap_tag_directives(tag_directives),
                                                  tag_directives.length,
                                                  implicit ? 1 : 0))
      self
    end

    def end_document(implicit)
      do_emit(Psych::LibPsych.emit_end_document(@emitter_context.emitter, implicit ? 1 : 0))
      self
    end

    def scalar(value, anchor, tag, plain, quoted, style)
      v = value.to_s
      do_emit(Psych::LibPsych.emit_scalar(@emitter_context.emitter,
                                          v, v.length,
                                          wrap_string_or_nil(anchor),
                                          wrap_string_or_nil(tag),
                                          plain  ? 1 : 0, quoted ? 1 : 0, style))
      self
    end

    def start_sequence(anchor, tag, implicit, style)
      do_emit(Psych::LibPsych.emit_start_sequence(@emitter_context.emitter,
                                                  wrap_string_or_nil(anchor),
                                                  wrap_string_or_nil(tag),
                                                  implicit ? 1 : 0,
                                                  style))
      self
    end

    def end_sequence
      do_emit(Psych::LibPsych.emit_end_sequence(@emitter_context.emitter))
      self
    end

    def start_mapping(anchor, tag, implicit, style)
      do_emit(Psych::LibPsych.emit_start_mapping(@emitter_context.emitter,
                                                 wrap_string_or_nil(anchor),
                                                 wrap_string_or_nil(tag),
                                                 implicit ? 1 : 0,
                                                 style))
      self
    end

    def end_mapping
      do_emit(Psych::LibPsych.emit_end_mapping(@emitter_context.emitter))
      self
    end

    def alias(anchor)
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
    end

    def wrap_string_or_nil(string)
      string.nil? ? FFI::MemoryPointer::NULL :
                    FFI::MemoryPointer.from_string(string)
    end

    def wrap_version(version)
      if version.nil? or version.empty?
        FFI::MemoryPointer::NULL
      else
        version_ptr = FFI::MemoryPointer.new(:int, version.length)
        version_ptr.write_array_of_int(version);
        version_ptr
      end
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

    # Get the output style, canonical or not.
    def canonical
      flag = Psych::LibPsych.get_canonical(@emitter_context.emitter)
      flag == 0 ? false : true
    end

    # Set the output style, canonical or not.
    def canonical=(flag)
      Psych::LibPsych.set_canonical(@emitter_context.emitter, flag ? 1 : 0)
      flag
    end

    # Set the indentation level to +level+.
    def set_indentation(level)
      Psych::LibPsych.set_indentation_level(@emitter_context.emitter, level)
      level
    end

    # Get the indentation level.
    def indentation
      Psych::LibPsych.get_indentation_level(@emitter_context.emitter)
    end
  end
end
