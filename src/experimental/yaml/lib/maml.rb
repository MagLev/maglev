module Maml

  class Parser
    def initialize(string_to_parse)
      @yaml = string_to_parse
      @parser = FFI::Buffer.alloc_inout LibYaml::YAML_PARSER_T_SIZE
      @documents = []
      LibYaml.yaml_parser_initialize(@parser)
    end

    def parse
      LibYaml.yaml_parser_set_input_string(@parser, @yaml, @yaml.size)
      event = FFI::Buffer.alloc_inout LibYaml::YAML_EVENT_T_SIZE

      done = false
      while not done
        res = LibYaml.yaml_parser_parse(@parser, event)
        raise_lib_yaml_exception if res == 0

        event_name = LibYaml::ParserEventEnum[event.read_int]
        case event_name
        when :yaml_stream_start_event
          raise "non-empty stack at stream start" unless @stack.nil?
          @stack = []
        when :yaml_stream_end_event
          done = true

        when :yaml_document_start_event
          @stack.push Document.new
        when :yaml_document_end_event
          puts event_name
          # TODO: Should we mark that we are in a state that expects a new
          # container?
        when :yaml_sequence_start_event
          @stack.push Sequence.new
        when :yaml_sequence_end_event
          seq = @stack.pop
          raise "no sequence for yaml_sequence_end_event" unless Sequence === seq
          @stack[0] << seq

        when :yaml_mapping_start_event
          @stack.push Mapping.new
        when :yaml_mapping_end_event
          mapping = @stack.pop
          raise "no mapping for yaml_mapping_end_event" unless Mapping === mapping
          @stack[0] << mapping

        when :yaml_scalar_event
          puts "scalar"

        else
          raise "Unhandled event: #{event_name}"
        end
      end
      documents = @stack
      @stack = nil
      return documents
    ensure
      LibYaml.yaml_event_delete(event) if event
      LibYaml.yaml_parser_delete(@parser) if @parser
    end

    def raise_lib_yaml_exception
      error = @parser.read_int
      raise "libYAML error: #{error} #{ErrorCodes[error]}"
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

  class Sequence < Array
    def initialize
      super()
      puts "New sequence created"
    end
  end

end
