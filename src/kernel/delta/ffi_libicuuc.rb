# def reload_icu
#   String.send(:remove_const, :Libicu) if String.const_defined?(:Libicu)
#   load "ffi_libicuuc.rb"
# end
class String
  def _encoding=(encoding)
    @encoding = encoding
  end

  def encoding
    @encoding ||= Encoding.default_internal
  end

  def encode(src_encoding=nil, dst_encoding=nil, options={})
    return self unless src_encoding
    return self if src_encoding._isHash

    if dst_encoding._isHash || dst_encoding.nil?
      options = dst_encoding || {}
      dst_encoding = src_encoding
      src_encoding = encoding
    end

    if src_encoding._isString
      src_encoding = Encoding.find(src_encoding)
    end
    if dst_encoding._isString
      dst_encoding = Encoding.find(dst_encoding)
    end

    # options ignored
    Libicu.convert(self, src_encoding.name, dst_encoding.name).tap do |s|
      s._encoding = dst_encoding
    end
  end

  def length
    Libicu::UChars.from_string(encoding.converter, self).length
  end
  alias size length

  class Libicu
    class UChars < Struct.new(:pointer, :converter, :length)
      def self.from_string(converter, str)
        err = UErrorCodePtr.new
        target_len = Libicu.ucnv_toUChars_48(converter.pointer, nil, 0, str, str.__size, err)
        uchars = FFI::MemoryPointer.new(:UChar, target_len + 1)
        err = UErrorCodePtr.new
        Libicu.ucnv_toUChars_48(converter.pointer, uchars, target_len + 1, str, str.__size, err)
        err.raise_if_error
        self.new(uchars, converter, target_len)
      end

      def to_string(conv = converter)
        err = UErrorCodePtr.new
        target_len = Libicu.ucnv_fromUChars_48(conv.pointer, nil, 0, pointer, length, err)
        chars = FFI::MemoryPointer.new(:uchar, target_len + 1)
        err = UErrorCodePtr.new
        Libicu.ucnv_fromUChars_48(conv.pointer, chars, target_len + 1, pointer, length, err)
        err.raise_if_error
        chars.read_string(target_len)
      end

      def length
        Libicu.u_strlen_48(pointer)
      end
    end

    class UConverter < Struct.new(:pointer, :name)
      def self.find(name = nil)
        name ||= Libicu.ucnv_getDefaultName_48
        err = UErrorCodePtr.new
        converter = Libicu.ucnv_open_48(name, err)
        err.raise_if_error
        proper_name = Libicu.ucnv_getName_48(converter, err)
        self.new(converter, proper_name)
      end

      def reset
        Libicu.ucnv_reset_48(pointer)
      end
    end

    class ICUError < StandardError; end

    class UErrorCodePtr < FFI::MemoryPointer
      def self.new
        super(:int, 1)
      end

      def raise_if_error
        if self.read_int != UErrorCode[:U_ZERO_ERROR]
          raise ICUError, UErrorCode[self.read_int].to_s
        end
      end
    end

    extend FFI::Library
    ffi_lib Maglev::System.gs_lib_name(:ICUUC)

    UErrorCode = enum :U_USING_FALLBACK_WARNING, -128,
                      :U_ERROR_WARNING_START, -128,
                      :U_USING_DEFAULT_WARNING,
                      :U_SAFECLONE_ALLOCATED_WARNING,
                      :U_STATE_OLD_WARNING,
                      :U_STRING_NOT_TERMINATED_WARNING,
                      :U_SORT_KEY_TOO_SHORT_WARNING,
                      :U_AMBIGUOUS_ALIAS_WARNING,
                      :U_DIFFERENT_UCA_VERSION,
                      :U_PLUGIN_CHANGED_LEVEL_WARNING,
                      :U_ERROR_WARNING_LIMIT,
                      :U_ZERO_ERROR, 0,
                      :U_ILLEGAL_ARGUMENT_ERROR,
                      :U_MISSING_RESOURCE_ERROR,
                      :U_INVALID_FORMAT_ERROR,
                      :U_FILE_ACCESS_ERROR,
                      :U_INTERNAL_PROGRAM_ERROR,
                      :U_MESSAGE_PARSE_ERROR,
                      :U_MEMORY_ALLOCATION_ERROR,
                      :U_INDEX_OUTOFBOUNDS_ERROR,
                      :U_PARSE_ERROR,
                      :U_INVALID_CHAR_FOUND,
                      :U_TRUNCATED_CHAR_FOUND,
                      :U_ILLEGAL_CHAR_FOUND,
                      :U_INVALID_TABLE_FORMAT,
                      :U_INVALID_TABLE_FILE,
                      :U_BUFFER_OVERFLOW_ERROR,
                      :U_UNSUPPORTED_ERROR,
                      :U_RESOURCE_TYPE_MISMATCH,
                      :U_ILLEGAL_ESCAPE_SEQUENCE,
                      :U_UNSUPPORTED_ESCAPE_SEQUENCE,
                      :U_NO_SPACE_AVAILABLE,
                      :U_CE_NOT_FOUND_ERROR,
                      :U_PRIMARY_TOO_LONG_ERROR,
                      :U_STATE_TOO_OLD_ERROR,
                      :U_TOO_MANY_ALIASES_ERROR,
                      :U_ENUM_OUT_OF_SYNC_ERROR,
                      :U_INVARIANT_CONVERSION_ERROR,
                      :U_INVALID_STATE_ERROR,
                      :U_COLLATOR_VERSION_MISMATCH,
                      :U_USELESS_COLLATOR_ERROR,
                      :U_NO_WRITE_PERMISSION,
                      :U_STANDARD_ERROR_LIMIT,
                      :U_BAD_VARIABLE_DEFINITION, 0x10000,
                      :U_PARSE_ERROR_START, 0x10000,
                      :U_MALFORMED_RULE,
                      :U_MALFORMED_SET,
                      :U_MALFORMED_SYMBOL_REFERENCE,
                      :U_MALFORMED_UNICODE_ESCAPE,
                      :U_MALFORMED_VARIABLE_DEFINITION,
                      :U_MALFORMED_VARIABLE_REFERENCE,
                      :U_MISMATCHED_SEGMENT_DELIMITERS,
                      :U_MISPLACED_ANCHOR_START,
                      :U_MISPLACED_CURSOR_OFFSET,
                      :U_MISPLACED_QUANTIFIER,
                      :U_MISSING_OPERATOR,
                      :U_MISSING_SEGMENT_CLOSE,
                      :U_MULTIPLE_ANTE_CONTEXTS,
                      :U_MULTIPLE_CURSORS,
                      :U_MULTIPLE_POST_CONTEXTS,
                      :U_TRAILING_BACKSLASH,
                      :U_UNDEFINED_SEGMENT_REFERENCE,
                      :U_UNDEFINED_VARIABLE,
                      :U_UNQUOTED_SPECIAL,
                      :U_UNTERMINATED_QUOTE,
                      :U_RULE_MASK_ERROR,
                      :U_MISPLACED_COMPOUND_FILTER,
                      :U_MULTIPLE_COMPOUND_FILTERS,
                      :U_INVALID_RBT_SYNTAX,
                      :U_INVALID_PROPERTY_PATTERN,
                      :U_MALFORMED_PRAGMA,
                      :U_UNCLOSED_SEGMENT,
                      :U_ILLEGAL_CHAR_IN_SEGMENT,
                      :U_VARIABLE_RANGE_EXHAUSTED,
                      :U_VARIABLE_RANGE_OVERLAP,
                      :U_ILLEGAL_CHARACTER,
                      :U_INTERNAL_TRANSLITERATOR_ERROR,
                      :U_INVALID_ID,
                      :U_INVALID_FUNCTION,
                      :U_PARSE_ERROR_LIMIT,
                      :U_UNEXPECTED_TOKEN, 0x10100,
                      :U_FMT_PARSE_ERROR_START, 0x10100,
                      :U_MULTIPLE_DECIMAL_SEPARATORS,
                      :U_MULTIPLE_DECIMAL_SEPERATORS,
                      :U_MULTIPLE_EXPONENTIAL_SYMBOLS,
                      :U_MALFORMED_EXPONENTIAL_PATTERN,
                      :U_MULTIPLE_PERCENT_SYMBOLS,
                      :U_MULTIPLE_PERMILL_SYMBOLS,
                      :U_MULTIPLE_PAD_SPECIFIERS,
                      :U_PATTERN_SYNTAX_ERROR,
                      :U_ILLEGAL_PAD_POSITION,
                      :U_UNMATCHED_BRACES,
                      :U_UNSUPPORTED_PROPERTY,
                      :U_UNSUPPORTED_ATTRIBUTE,
                      :U_ARGUMENT_TYPE_MISMATCH,
                      :U_DUPLICATE_KEYWORD,
                      :U_UNDEFINED_KEYWORD,
                      :U_DEFAULT_KEYWORD_MISSING,
                      :U_DECIMAL_NUMBER_SYNTAX_ERROR,
                      :U_FORMAT_INEXACT_ERROR,
                      :U_FMT_PARSE_ERROR_LIMIT,
                      :U_BRK_INTERNAL_ERROR, 0x10200,
                      :U_BRK_ERROR_START, 0x10200,
                      :U_BRK_HEX_DIGITS_EXPECTED,
                      :U_BRK_SEMICOLON_EXPECTED,
                      :U_BRK_RULE_SYNTAX,
                      :U_BRK_UNCLOSED_SET,
                      :U_BRK_ASSIGN_ERROR,
                      :U_BRK_VARIABLE_REDFINITION,
                      :U_BRK_MISMATCHED_PAREN,
                      :U_BRK_NEW_LINE_IN_QUOTED_STRING,
                      :U_BRK_UNDEFINED_VARIABLE,
                      :U_BRK_INIT_ERROR,
                      :U_BRK_RULE_EMPTY_SET,
                      :U_BRK_UNRECOGNIZED_OPTION,
                      :U_BRK_MALFORMED_RULE_TAG,
                      :U_BRK_ERROR_LIMIT,
                      :U_REGEX_INTERNAL_ERROR, 0x10300,
                      :U_REGEX_ERROR_START, 0x10300,
                      :U_REGEX_RULE_SYNTAX,
                      :U_REGEX_INVALID_STATE,
                      :U_REGEX_BAD_ESCAPE_SEQUENCE,
                      :U_REGEX_PROPERTY_SYNTAX,
                      :U_REGEX_UNIMPLEMENTED,
                      :U_REGEX_MISMATCHED_PAREN,
                      :U_REGEX_NUMBER_TOO_BIG,
                      :U_REGEX_BAD_INTERVAL,
                      :U_REGEX_MAX_LT_MIN,
                      :U_REGEX_INVALID_BACK_REF,
                      :U_REGEX_INVALID_FLAG,
                      :U_REGEX_LOOK_BEHIND_LIMIT,
                      :U_REGEX_SET_CONTAINS_STRING,
                      :U_REGEX_OCTAL_TOO_BIG,
                      :U_REGEX_MISSING_CLOSE_BRACKET,
                      :U_REGEX_INVALID_RANGE,
                      :U_REGEX_STACK_OVERFLOW,
                      :U_REGEX_TIME_OUT,
                      :U_REGEX_STOPPED_BY_CALLER,
                      :U_REGEX_ERROR_LIMIT,
                      :U_IDNA_PROHIBITED_ERROR, 0x10400,
                      :U_IDNA_ERROR_START, 0x10400,
                      :U_IDNA_UNASSIGNED_ERROR,
                      :U_IDNA_CHECK_BIDI_ERROR,
                      :U_IDNA_STD3_ASCII_RULES_ERROR,
                      :U_IDNA_ACE_PREFIX_ERROR,
                      :U_IDNA_VERIFICATION_ERROR,
                      :U_IDNA_LABEL_TOO_LONG_ERROR,
                      :U_IDNA_ZERO_LENGTH_LABEL_ERROR,
                      :U_IDNA_DOMAIN_NAME_TOO_LONG_ERROR,
                      :U_IDNA_ERROR_LIMIT,
                      :U_STRINGPREP_PROHIBITED_ERROR, 0x10400,
                      :U_STRINGPREP_UNASSIGNED_ERROR,
                      :U_STRINGPREP_CHECK_BIDI_ERROR,
                      :U_PLUGIN_ERROR_START, 0x10500,
                      :U_PLUGIN_TOO_HIGH, 0x10500,
                      :U_PLUGIN_DIDNT_SET_LEVEL,
                      :U_PLUGIN_ERROR_LIMIT,
                      :U_ERROR_LIMIT

    typedef :uint16, :UChar

    # Default system encoding
    attach_function(:ucnv_getDefaultName_48, [], :string)
    # Creates a UConverter object with the name of a coded character
    # set specified as a C string.
    attach_function(:ucnv_open_48, [:string, :pointer], :pointer)
    # Reset converter state after an error
    attach_function(:ucnv_reset_48, [:pointer], :void)
    # Get standard name
    attach_function(:ucnv_getName_48, [:pointer, :pointer], :string)
    # Convert codepage string into Unicode string
    # UConverter*, UChar *dest, destCapacity, src, srcLen, errorCode
    attach_function(:ucnv_toUChars_48,
                    [:pointer, :pointer, :int, :string, :int, :pointer],
                    :int)
    # Convert into output string using converter
    # UConverter*, char*, destCapacity, UChar*, srclen, errorCode
    attach_function(:ucnv_fromUChars_48,
                    [:pointer, :pointer, :int, :pointer, :int, :pointer],
                    :int)
    # convert from external charset into another
    # toConvName, fromConvName, char* target, targetCapacity, src, srclen, errorcode
    attach_function(:ucnv_convert_48,
                    [:string, :string, :pointer, :int, :string, :int, :pointer],
                    :int)

    def self.convert(str, src_encoding, dst_encoding)
      err = UErrorCodePtr.new
      len = ucnv_convert_48(dst_encoding, src_encoding, nil, 0, str, str.__size, err)
      err = UErrorCodePtr.new
      chars = FFI::MemoryPointer.new(:char, len + 1)
      ucnv_convert_48(dst_encoding, src_encoding, chars, len +  1, str, str.__size, err)
      err.raise_if_error
      chars.read_string(len)
    end

    attach_function(:u_strlen_48, [:pointer], :int)
  end
end

class Encoding
  attr_writer :name

  def _converter
    String::Libicu::UConverter.find @name
  end

  def self.new(name)
    super.tap do |i|
      i.name = name
    end
  end
  private :new

  def self.default_external
    @default_external ||= new(String::Libicu::UConverter.find.name)
  end

  def self.default_internal
    default_external
  end

  def self.find(name)
    name = name.to_s.gsub("-", "_").gsub(" ", "_")
    if %w[external internal locale filesystem].include? name
      return default_external
    elsif const_defined?(name)
      return const_get(name) if const_get(name).class == self
    end
    raise ArgumentError, "unknown encoding name - #{name}"
  end

  def self.list
    self.constants.map do |c|
      const_get(c)
    end.select do |o|
      o.class == self
    end.uniq
  end

  def self.name_list
    self.list.map(&:name)
  end

  def name
    converter.name
  end
  alias to_s name

  def dummy?
    false
  end

  def ascii_compatible?
    ["UTF-8", "ISO-8859-1", "US-ASCII"].include? name
  end

  def names
    self.class.list.select do |e|
      e == self
    end.map(&:name)
  end

  def inspect
    "#<Encoding:#{name}#{' (dummy)' if dummy?}>"
  end

  def replicate
    raise NotImplementedError
  end

  UTF8 = UTF_8 = new("UTF-8")
  LATIN1 = ISO8859_1 = ISO_8859_1 = new("LATIN1")
  BINARY = ASCII = ASCII_8BIT = US_ASCII = new("ASCII")
  UTF16 = UTF_16 = new("UTF-16")
  UTF16LE = UTF_16LE = new("UTF-16LE")
  UTF16BE = UTF_16BE = new("UTF-16BE")
  UTF32 = UTF_32 = new("UTF-32")
  UTF32LE = UTF_32LE = new("UTF-32LE")
  UTF32BE = UTF_32BE = new("UTF-32BE")
end
