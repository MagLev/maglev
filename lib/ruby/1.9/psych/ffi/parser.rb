# This file is the ruby version of the psych ext/psych/parser.c
# It uses the FFI lib, libpsych.rb, as a wrapper for libyaml
module Psych

  class PsychSyntaxError < SyntaxError;  end

  class Parser

    ANY     = LibPsych::ParserEncodingEnum[:any]
    UTF8    = LibPsych::ParserEncodingEnum[:utf8]
    UTF16LE = LibPsych::ParserEncodingEnum[:utf_16le]
    UTF16BE = LibPsych::ParserEncodingEnum[:utf_16be]

    def parse(input)

      c_parser_context =
        case input
        when String
          # We need to make a stable copy of the ruby string.  If we just
          # pass the string directly as "create_parser_context(string)",
          # then the gc is free to collect the c-data struct when
          # create_parser_context returns (the auto-copied data is only
          # good for the duration of that particular c-call).  BUT, we then
          # want to pass the c_parser_context (which references the string)
          # into the next c-call, so we need a more permanent solution.  By
          # creating input_buf, we have copied the content of string into
          # c-memory that won't be gc'd until input_buf is gc'd, i.e., it
          # will survive for the duration of this call to parse (which is
          # long enough).
          input_buf = FFI::Pointer.from_string(input)
          Psych::LibPsych.create_parser_context(nil, input_buf)

        when IO
          callback = Proc.new do |ignored, buffer, bufsize, num_bytes|
            str = input.read(bufsize)
            len = str.nil? ? 0 : str.length
            buffer.to_ptr.write_string(str, len) if len > 0
            num_bytes.int64_put(0, len)
            1
          end
          Psych::LibPsych.create_parser_context(callback, nil)

        else
          raise ArgumentError,
          "YAML.parse: Unsupported input: (#{input.class})"
        end

      done = false
      while not done
        event_ptr = Psych::LibPsych.next_event(c_parser_context)
        event = Psych::LibPsych::ParserEvent.new(event_ptr)

        case event.event_type
        when :no_event
          # Nothing

        when :stream_start_event
          @handler.start_stream(event.character_encoding)

        when :stream_end_event
          @handler.end_stream
          done = true

        when :document_start_event
          @handler.start_document(event.version,
                                  event.tag_directives,
                                  event.implicit?)

        when :document_end_event
          @handler.end_document(event.implicit?)

        when :alias_event
          @handler.alias(event.anchor)

        when :scalar_event
          # TODO: Need to associate the current encoding with
          #       the scalar string...
          #           rb_enc_associate_index(val, encoding);
          @handler.scalar(event.value,
                          event.anchor,
                          event.tag,
                          event.plain_implicit?,
                          event.quoted_implicit?,
                          event.style)

        when :sequence_start_event
          @handler.start_sequence(event.anchor,
                                  event.tag,
                                  event.implicit?,
                                  event.style)

        when :sequence_end_event
          @handler.end_sequence

        when :mapping_start_event
          @handler.start_mapping(event.anchor,
                                 event.tag,
                                 event.implicit?,
                                 event.style)

        when :mapping_end_event
          @handler.end_mapping

        when :parse_error_event
          e = event
          msg_ptr = event[:scalar]
          msg = msg_ptr.get_string(0)
          raise PsychSyntaxError.new "Syntax error at #{event.position}: #{event.error_message}"
        else
          raise "#{self}: UNKNOWN EVENT: #{event[:type]}"
        end
      end
    end
  end
end
