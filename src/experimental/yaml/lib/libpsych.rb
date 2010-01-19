# TODO:
#   need to ensure all data structures are released/free()d

module Psych
  # LibPsych wraps libparser.dylib.  libparser is a layer of support to
  # hide the complexity of libyaml with its deeply nested structs and
  # unions.  libparser is designed to pass into ruby just the items
  # required by Psych.
  class LibPsych
    extend FFI::Library
    ffi_lib "#{ENV['HOME']}/GemStone/checkouts/git/src/experimental/yaml/c/libpsych"

    # Creates a new parser context given a string.
    # Returns a pointer to the context
    #   parser_context_t *create_parser_context(unsigned char *input);
    attach_function :create_parser_context, [ :pointer ], :pointer

    # Returns
    # parser_event_t *next_event(parser_context_t *parser_context);
    attach_function :next_event, [:pointer], :pointer

    ParserEventEnum = FFI::Enum.new([:no_event,
                                     :stream_start_event,
                                     :stream_end_event,
                                     :document_start_event,
                                     :document_end_event,
                                     :alias_event,
                                     :scalar_event,
                                     :sequence_start_event,
                                     :sequence_end_event,
                                     :mapping_start_event,
                                     :mapping_end_event,
                                     :parse_error_event],
                                    :parser_event_type_e)

    ParserEncodingEnum = FFI::Enum.new([:any, :utf8, :utf_16le, :utf_16be],
                                       :parser_character_encoding_e)

    class ParserEvent < FFI::Struct
      layout :type,           :int,
             :encoding,       :int,
             :version_major,  :int,
             :version_minor,  :int,
             :num_tags,       :int,
             :tag_directives, :pointer,
             :yaml_line,      :size_t,
             :yaml_column,    :size_t,
             :scalar,         :pointer,
             :scalar_length,  :long,
             :style,          :long,
             :anchor,         :string,
             :tag,            :string,
             :flag,           :uchar

      VERSION_FLAG  = 0x01;
      IMPLICIT_FLAG = 0x02;
      QUOTE_FLAG    = 0x04;

      def has_version?
        (self[:flag] & VERSION_FLAG) != 0
      end

      def implicit?
        (self[:flag] & IMPLICIT_FLAG) != 0
      end

      def quoted?
        (self[:flag] & QUOTE_FLAG) != 0
      end

      # TODO: Should we raise an exception if the event type isn't correct?
      # Or just return nil?
      def version
        self.has_version? ? nil : [self[:version_major,], self[:version_minor]]
      end

      def event_type
        Psych::LibPsych::ParserEventEnum[self[:type]]
      end

      def character_encoding
        case event_type
        when :stream_start_event
          Psych::LibPsych::ParserEncodingEnum[self[:encoding]]
        else
          nil
        end
      end

      def value
        :stub_value
      end

      def tag
        :stub_tag
      end

      def plain
        :stub_plain
      end

      def style
        :stub_style
      end

      def tag_directives
        num_tags = self[:num_tags]
        puts "-- #{self}.tag_directives:  num_tags: #{num_tags}"
        if num_tags < 0
          raise ArgumentError.new "num_tags (#{num_tags}) should be positive"
        end
        if num_tags == 0
          []
        else
          [:stub]  # RxINC
          # The parser library returns an array of strings: two strings
          # (prefix and handle) for each tag.  E.g., here is an array that
          # represents two tags:
          #
          #    [ "!",     "tag:gemstone.com,2009",
          #      "!foo!", "tag:foo.com,1832" ]
          #
#           debug = :one
#           puts "-- DEBUG path #{debug}"
#           case debug
#           when :one
#             tag_dirs = self[:tag_directives]
#             puts "-- :one:  tag_dirs #{tag_dirs.inspect}"
#             strings = tag_dirs.get_array_of_string(0, 2*num_tags)
#           when :two
#             tag_dirs = self[:tag_directives].read_pointer
#             tag_ptr = FFI::MemoryPointer.new
#             puts "-- Attempt read_pointer from #{tag_dirs.inspect}"
#             tag_ptr.write_pointer(tag_dirs.read_pointer)
#             strings = tag_ptr.get_array_of_string(0, 2*num_tags)
# #          tag_ptr = tag_dirs.read_pointer
# #          tag_ptr = FFI::MemoryPointer.new(tag_dirs.read_pointer)

#           end
#           puts "-- path: #{debug}: strings: #{strings.inspect}"

#           result = []
#           tag = idx = 0
#           while (tag < num_tags)
#             res << [strings[idx], strings[idx + 1]]
#             idx += 2
#           end
#           result
        end
      end

      def anchor
        case event_type
        when :alias_event, :scalar_event, :sequence_start_event, :mapping_start_event
          :stub_anchor
        else
          :stub_not_implemented
        end
      end
    end
  end


  class Parser
    def parse(string, handler)
      # We need to make a stable copy of the ruby string.  If we just pass
      # the string directly as "create_parser_context(string)", then the gc
      # is free to collect the c-data struct when create_parser_context
      # returns (the auto-copied data is only good for the duration of that
      # particular c-call).  BUT, we then want to pass the c_parser_context (which
      # references the string) into the next c-call, so we need a more
      # permanent solution.  By creating input_buf, we have copied the
      # content of string into c-memory that won't be gc'd until input_buf
      # is gc'd, i.e., it will survive for the duration of this call to
      # parse (which is long enough).
      input_buf = FFI::Pointer.from_string(string)
      c_parser_context = Psych::LibPsych.create_parser_context(input_buf)
      done = false
      while not done
        event_ptr = Psych::LibPsych.next_event(c_parser_context)
        event = Psych::LibPsych::ParserEvent.new(event_ptr)

        case event.event_type
        when :no_event
          puts "#{self}: No Event"

        when :stream_start_event
          handler.start_stream( event.character_encoding )

        when :stream_end_event
          handler.end_stream
          done = true

        when :document_start_event
          handler.start_document( event.version,
                                  event.tag_directives,
                                  event.implicit? )

        when :document_end_event
          handler.end_document( event.implicit? )

        when :alias_event
          handler.alias( event.anchor )

        when :scalar_event
          # TODO: Need to associate the current encoding with
          #       the scalar string...
          #           rb_enc_associate_index(val, encoding);
          handler.scalar( event.value,
                          event.anchor,
                          event.tag,
                          event.plain,
                          event.quoted?,
                          event.style )

        when :sequence_start_event
          handler.start_sequence( event.anchor,
                                  event.tag,
                                  event.implicit?,
                                  event.style )

        when :sequence_end_event
          handler.end_sequence

        when :mapping_start_event
          handler.start_mapping( event.anchor,
                                 event.tag,
                                 event.implicit?,
                                 event.style )

        when :mapping_end_event
          handler.end_mapping

        when :parse_error_event
          puts "#{self}: ERROR"
          done = true
        else
          puts "#{self}: UNKNOWN EVENT"
          done = true
        end
      end
    end
  end
end
