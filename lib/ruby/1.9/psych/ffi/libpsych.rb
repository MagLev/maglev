# FFI support for psych.
module Psych

  # LibPsych wraps the libpsych shared library.  libpsych is a layer of
  # support to hide the complexity of libyaml with its deeply nested
  # structs and unions.  libparser is designed to pass into ruby just the
  # items required by Psych.

  class LibPsych
    extend FFI::Library
    ffi_lib Maglev::System.gs_lib_name(:YAML)

    ##################################################
    # Parser support
    ##################################################

    # Gets an array of the major, minor, patch level for the loaded
    # libyaml.
    #  void libyaml_version(int version_info[]);
    attach_function( :libyaml_version, [:pointer], :void )


    # typedef int yaml_read_handler_t(void *data, unsigned char *buffer, size_t size,
    #                                 size_t *size_read);
    #
    # void            *data   Application specific data (NULL in our case)
    # unsigned char *buffer   Buffer for the read handler to write data into
    # size_t           size   Size of buffer
    # size_t     *size_read   Num bytes read handler wrote into buffer
    #
    # Returns 1 for success; 0 for failure
    callback :parser_read_handler, [:pointer, :pointer, :size_t, :pointer ], :int

    # Creates a new parser context given a callback or input string.
    #   parser_context_t *create_parser_context(yaml_read_handler_t *callback,
    #                                           unsigned char *input_string));
    attach_function( :create_parser_context, [ :parser_read_handler, :pointer ], :pointer)

    # Returns a pointer to a ParserEvent that describes the next event in
    # the YAML event stream.
    #   parser_event_t *next_event(parser_context_t *parser_context);
    attach_function( :next_event, [:pointer], :pointer)

    # Frees a parser context created by create_parser_context
    #   void free_parser_context(parser_context_t *context);
    attach_function( :free_parser_context, [:pointer], :void)

    # An Enum that describes the YAML parser events.  ParserEvent[:type]
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

    # An Enum that describes the character encodings returned as part of a
    # :stream_start_event
    ParserEncodingEnum = FFI::Enum.new([:any, :utf8, :utf_16le, :utf_16be],
                                       :parser_character_encoding_e)

    # Encapsulates a YAML parser event.  Wraps parser_context_t.  Parsing a
    # YAML file will generate a sequence of ParserEvents.
    class ParserEvent < FFI::Struct
      layout :type,           :int,
             :encoding,       :int,
             :version_major,  :int,
             :version_minor,  :int,
             :num_tags,       :int,
             :tag_directives, :pointer,   # unsigned char **
             :yaml_line,      :size_t,
             :yaml_column,    :size_t,
             :scalar,         :pointer,   # unsigned char *; May have NULLs
             :scalar_length,  :long,
             :style,          :long,
             :anchor,         :string,    # unsigned char *
             :tag,            :string,    # unsigned char *
             :flag,           :uchar

      VERSION_FLAG        = 0x01;
      IMPLICIT_FLAG       = 0x02;
      QUOTE_FLAG          = 0x04;
      PLAIN_IMPLICIT_FLAG = 0x08;
      QUOTED_IMPLICIT_FLAG = 0x10;

      # Return a string describing the current line and column in the YAML
      # being parsed.
      def position
        "Line: #{self[:yaml_line]} Column: #{self[:yaml_column]}"
      end

      # If the event is an error, then return the emedded error message
      # from libyaml.  If the event is not an error, return the empty
      # string.
      def error_message
        case event_type
        when :parse_error_event
          self[:scalar].get_string(0)
        else
          ""
        end
      end

      def has_version?
        (self[:flag] & VERSION_FLAG) != 0
      end

      def implicit?
        (self[:flag] & IMPLICIT_FLAG) != 0
      end

      def quoted?
        (self[:flag] & QUOTE_FLAG) != 0
      end

      def plain_implicit?
        (self[:flag] & PLAIN_IMPLICIT_FLAG) != 0
      end

      def quoted_implicit?
        (self[:flag] & QUOTED_IMPLICIT_FLAG) != 0
      end

      # TODO: Should we raise an exception if the event type isn't correct?
      # Or just return nil?
      def version
        self.has_version? ?
          nil : [self[:version_major,], self[:version_minor]]
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

      # Returns a string ecnoding the scalar value of the event.  The
      # string may contain embedded nulls.
      def value
        case event_type
        when :scalar_event
          x = self[:scalar]
          length = self[:scalar_length]
          length == 0 ? "" : x.read_string(length)
        else
          nil
        end
      end

      def tag
        case event_type
        when :scalar_event, :sequence_start_event, :mapping_start_event
          self[:tag]
        else
          nil
        end
      end

      def style
        case event_type
        when :scalar_event, :sequence_start_event, :mapping_start_event
          self[:style]
        else
          nil
        end
      end

      def tag_directives
        num_tags = self[:num_tags]
        if num_tags < 0
          raise ArgumentError.new "num_tags (#{num_tags}) should be positive"
        end
        # The parser library returns an array of strings: two strings
        # (prefix and handle) for each tag.  E.g., here is an array that
        # represents two tags:
        #
        #    [ "!",     nil,
        #      "!foo!", "tag:foo.com,1832" ]
        #
        result = []
        if num_tags > 0
          num_strings = 2 * num_tags
          tag_dirs = self[:tag_directives]
          strings = tag_dirs.get_array_of_string(0, num_strings)
          idx = 0
          while (idx < num_strings)
            result << [strings[idx], strings[idx + 1]]
            idx += 2
          end
        end
        result
      end

      def anchor
        case event_type
        when :alias_event, :scalar_event,
             :sequence_start_event, :mapping_start_event
          self[:anchor]
        else
          :stub_not_implemented
        end
      end
    end


    ##################################################
    # Emitter support
    ##################################################
    class EmitterContext < FFI::Struct
      layout :emitter,        :pointer

      def emitter
        self[:emitter]
      end

      # # Flush any accummulated bytes of output to the IO object
      # def flush(io)
      #   count = self[:char_count]
      #   if count > 0
      #     str = self[:buffer].read_string(count)
      #     bytes_written = io.write(str)
      #     unless bytes_written == count
      #       raise "Only flushed #{bytes_written} of #{count} bytes to #{io}"
      #     end
      #     self[:char_count] = 0
      #   end
      # end
    end

    # Called by the libyaml emitter when it has accummulated output that
    # needs to be consumed.
    callback :emitter_output_writer, [:pointer, :pointer, :size_t], :int

    # Emit a stream start event
    #   int emit_start_stream(yaml_emitter_t *emitter,
    #                         yaml_encoding_t encoding);
    attach_function :emit_start_stream, [:pointer, :int], :int

    # int emit_start_document(yaml_emitter_t *emitter,
    #                         int *version,
    #                         unsigned char **tag_directives,
    #                         int num_tags,
    #                         int implicit);
    #
    attach_function :emit_start_document, [:pointer, :pointer,
                                           :pointer, :int, :int], :int

    # int emit_scalar(yaml_emitter_t *emitter,
    #                 yaml_char_t *value,
    #                 size_t value_len,
    #                 yaml_char_t *anchor,    /* May be NULL */
    #                 yaml_char_t *tag,       /* May be NULL */
    #                 int plain,
    #                 int quoted,
    #                 yaml_scalar_style_t style);
    attach_function :emit_scalar, [:pointer, :const_string, :size_t, :pointer,
                                   :pointer, :int, :int, :int], :int

    # int emit_end_document(yaml_emitter_t *emitter, int implicit);
    attach_function :emit_end_document, [:pointer, :int], :int

    # int emit_end_stream(yaml_emitter_t *emitter);
    attach_function :emit_end_stream, [:pointer], :int

    # Create the emitter context object
    #   emitter_context_t *create_emitter(callback);
    attach_function :create_emitter_context, [:emitter_output_writer], :pointer

    # Free the emitter context object
    #   void free_emitter(emitter_context_t *emitter);
    attach_function :free_emitter_context, [:pointer], :void

    # int emit_start_sequence(yaml_emitter_t *emitter
    #                         yaml_char_t *anchor,
    #                         yaml_char_t *tag,
    #                        int implicit,
    #                        yaml_scalar_style_t style);
    attach_function :emit_start_sequence, [:pointer, :pointer, :pointer,
                                           :int, :int], :int

    # int emit_end_sequence(yaml_emitter_t *emitter);
    attach_function :emit_end_sequence, [:pointer], :int

    # Use :pointer rather than :string, since passing NULL is ok and
    # expected.
    #  emitter:   The emitter
    #  anchor:    NULL or a string
    #  tag:       NULL or a string
    #  implicit:  0 or 1
    #  style:     the style
    #
    # int emit_start_mapping(yaml_emitter_t *emitter
    #                        yaml_char_t *anchor,
    #                        yaml_char_t *tag,
    #                        int implicit,
    #                        yaml_scalar_style_t style) {
    attach_function :emit_start_mapping, [:pointer, :pointer, :pointer,
                                          :int, :int], :int

    # int emit_end_mapping(yaml_emitter_t *emitter);
    attach_function :emit_end_mapping, [:pointer], :int

    # int emit_alias(yaml_emitter_t *emitter, yaml_char_t *anchor);
    attach_function :emit_alias, [:pointer, :pointer], :int

    # If there is an error, get the libyaml error description
    attach_function :get_error_string, [:pointer], :string

    # Get the current indentation level for this emitter
    attach_function :get_indentation_level, [:pointer], :int

    # Set the current indentation level for this emitter
    attach_function :set_indentation_level, [:pointer, :int], :void

    # Get the current canonical style
    attach_function :get_canonical, [:pointer], :int

    # Set the current canonical style
    attach_function :set_canonical, [:pointer, :int], :void
  end

  def self.libyaml_version
    version_info = FFI::MemoryPointer.new(:int, 3);
    LibPsych.libyaml_version(version_info)
    [version_info[0], version_info[1], version_info[2]]
  end
end
