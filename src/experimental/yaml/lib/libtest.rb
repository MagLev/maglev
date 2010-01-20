class LibTest
  extend FFI::Library
  ffi_lib "#{ENV['HOME']}/GemStone/checkouts/git/src/experimental/yaml/c/libtest"

  attach_function :test_version_data, [], :pointer
  attach_function :test_scalar_data,  [], :pointer

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
  class ScalarStruct < FFI::Struct
    layout( :value, :string,
            :length, :size_t )

    def string_value
      self[:value]
    end
  end

  class VersionStruct < FFI::Struct
    layout( :major, :int,
            :minor, :int )

    def major
      self[:major]
    end

    def minor
      self[:minor]
    end

    def version
      [self[:major], self[:minor]]
    end
  end

  class DataUnion < FFI::Union
    layout :scalar,            ScalarStruct,
           :version_directive, VersionStruct
  end

  class Parser < FFI::Struct
    layout :type, :int,
           :data, DataUnion

    def data
      self[:data]
    end

    def type
      ParserEventEnum[self[:type]]
    end

    def version
      case self.type
      when :stream_start_event
        self[:data][:version_directive].version
      else
        puts "-- Can't pull a version out of #{self.type}"
        nil
      end
    end
  end

end
