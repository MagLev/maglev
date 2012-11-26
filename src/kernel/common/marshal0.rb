# file marshal0.rb
# first opening of Marshal, so bootstrap can use non-dynamic constants

module Marshal

  MAJOR_VERSION = 4.chr
  MINOR_VERSION = 8.chr

  #--
  # NOTE: All string constants are frozen.  Protects against a bug where
  # the constant was returned from a serialize_* method, and then used to
  # build the serialized data for an object, thus turning "I" into
  # "I%:blahblah..." for the next time...
  #++

  VERSION_STRING = "\x04\x08".freeze

  TYPE_ARRAY = '['.freeze
  TYPE_ARRAY_ch = 91 #?[

  TYPE_BIGNUM = 'l'.freeze
  TYPE_BIGNUM_ch = 108 #?l

  TYPE_CLASS = 'c'.freeze
  TYPE_CLASS_ch = 99 #?c

  TYPE_DATA = 'd'.freeze  # no specs
  TYPE_DATA_ch = 100 #?d  # no specs

  TYPE_EXTENDED = 'e'.freeze
  TYPE_EXTENDED_ch = 101 #?e

  TYPE_FALSE = 'F'.freeze
  TYPE_FALSE_ch = 70 #?F

  TYPE_FIXNUM = 'i'.freeze
  TYPE_FIXNUM_ch = 105 #?i

  TYPE_FLOAT = 'f'.freeze
  TYPE_FLOAT_ch = 102 #?f

  TYPE_HASH = '{'.freeze
  TYPE_HASH_DEF = '}'.freeze

  TYPE_HASH_DEF_ch = 125 #?}
  TYPE_HASH_ch = 123 #?{

  TYPE_IVAR = 'I'.freeze
  TYPE_IVAR_ch = 73 #?I

  TYPE_LINK = '@'.freeze
  TYPE_LINK_ch = 64 #?@

  TYPE_MODULE = 'm'.freeze
  TYPE_MODULE_ch = 109 #?m

  TYPE_MODULE_OLD = 'M'.freeze  # no specs
  TYPE_MODULE_OLD_ch = 77 #?M   # no specs

  TYPE_NIL = '0'.freeze
  TYPE_NIL_ch = 48 #?0

  TYPE_OBJECT = 'o'.freeze
  TYPE_OBJECT_ch = 111 #?o

  TYPE_REGEXP = '/'.freeze
  TYPE_REGEXP_ch = 47 #?/

  TYPE_STRING = '"'.freeze
  TYPE_STRING_ch = 34 #?"

  TYPE_STRUCT = 'S'.freeze
  TYPE_STRUCT_ch = 83 #?S

  TYPE_SYMBOL = ':'.freeze
  TYPE_SYMBOL_ch = 58 #?:

  TYPE_SYMLINK = ';'.freeze
  TYPE_SYMLINK_ch = 59 #?;

  TYPE_TRUE = 'T'.freeze
  TYPE_TRUE_ch = 84 #?T

  TYPE_UCLASS = 'C'.freeze
  TYPE_UCLASS_ch = 67 #?C

  TYPE_USERDEF = 'u'.freeze
  TYPE_USERDEF_ch = 117 #?u

  TYPE_USRMARSHAL = 'U'.freeze
  TYPE_USRMARSHAL_ch = 85 #?U
end
Marshal.__freeze_constants

