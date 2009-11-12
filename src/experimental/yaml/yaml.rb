require 'ffi'

class MAML
  extend FFI::Library
  ffi_lib '/Users/pmclain/external/yaml-0.1.3-64/src/.libs/libyaml-0.2.dylib'

  attach_function :yaml_get_version, [:pointer, :pointer, :pointer], :void
  attach_function :yaml_parser_initialize, [:pointer], :int
  attach_function :yaml_parser_set_input_string, [:pointer, :string, :int], :void
  attach_function :yaml_parser_parse, [:pointer, :pointer], :int
  attach_function :yaml_event_delete, [:pointer], :void
  attach_function :yaml_parser_delete, [:pointer], :void

  # return the version of libyaml we are using, e.g., "0.1.3"
  def self.version
    result = [FFI::MemoryPointer.new(:pointer),
              FFI::MemoryPointer.new(:pointer),
              FFI::MemoryPointer.new(:pointer)]
    yaml_get_version(*result)
    result.map {|el| el.read_int}.join(".")
  end

  class Parser
    ParserEventEnum = FFI::Enum.new([:yaml_no_event,
                                     :yaml_stream_start_event,
                                     :yaml_stream_end_event,
                                     :yaml_document_start_event,
                                     :yaml_document_end_event,
                                     :yaml_alias_event,
                                     :yaml_scalar_event,
                                     :yaml_sequence_start_event,
                                     :yaml_sequence_end_event,
                                     :yaml_mapping_start_event,
                                     :yaml_mapping_end_event ],
                                    :yaml_event_type_e)

    def initialize(string_to_parse)
      # TODO: 248 should be sufficient, it is what sizeof(yaml_parser_t)
      # returns.  The c code is doing:
      #    memset(parser, 0, sizeof(yaml_parser_t));
      #
      # @parser = FFI::Buffer.alloc_inout(248)
      @yaml = string_to_parse
      @parser = FFI::Buffer.alloc_inout(512)
      @documents = []
      MAML.yaml_parser_initialize(@parser)

      x = ParserEventEnum
      puts "-- #{x[:yaml_scalar_event]} "
      puts "-- #{x[3]} "
    end

    def parse
      MAML.yaml_parser_set_input_string(@parser, @yaml, @yaml.size)
      event = FFI::Buffer.alloc_inout(512)
      document = nil
      sequence = nil
      done = false
      while not done
        res = MAML.yaml_parser_parse(@parser, event)
        raise "MAML parse error" if res == 0

        event_name = ParserEventEnum[event.read_int]
        case event_name
        when :yaml_stream_start_event
          # no-op
        when :yaml_stream_end_event
          done = true

        when :yaml_document_start_event
          raise "Unfinished document" if document
          document = Document.new
        when :yaml_document_end_event
          raise "no document for yaml_document_end_event" unless document
          @documents << document
          document = nil

        when :yaml_sequence_start_event
          raise "Unfinished sequence" if sequence
          sequence = Sequence.new
        when :yaml_sequence_end_event
          raise "no sequence for yaml_sequence_end_event" unless sequence
          document << sequence
          sequence = nil

        when :yaml_scalar_event
          puts "scalar"

        else
          raise "Unhandled event: #{event_name}"
        end
      end

      return true
    ensure
      MAML.yaml_event_delete(event) if event
      MAML.yaml_parser_delete(@parser) if @parser
    end
  end

  class Document
    def initialize
      puts "New document created"
      @elements = []
    end
    def <<(obj)
      @elements << obj
    end
  end

  class Sequence
    def initialize
      puts "New sequence created"
    end
  end
end

p MAML.version
yaml = "- Mark McGwire" # a small yaml file
parser = MAML::Parser.new(yaml)
p parser.parse
