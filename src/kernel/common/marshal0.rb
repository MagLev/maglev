# file marshal0.rb
# first opening of Marshal, so bootstrap can use non-dynamic constants

module Marshal

  MAJOR_VERSION = 4
  MINOR_VERSION = 8

  #--
  # NOTE: All string constants are frozen.  Protects against a bug where
  # the constant was returned from a serialize_* method, and then used to
  # build the serialized data for an object, thus turning "I" into
  # "I%:blahblah..." for the next time...
  #++

  VERSION_STRING = "\x04\x08".freeze

  TYPE_ARRAY = '['.freeze
  TYPE_ARRAY_ch = ?[

  TYPE_BIGNUM = 'l'.freeze
  TYPE_BIGNUM_ch = ?l

  TYPE_CLASS = 'c'.freeze
  TYPE_CLASS_ch = ?c

  TYPE_DATA = 'd'.freeze  # no specs
  TYPE_DATA_ch = ?d  # no specs

  TYPE_EXTENDED = 'e'.freeze
  TYPE_EXTENDED_ch = ?e

  TYPE_FALSE = 'F'.freeze
  TYPE_FALSE_ch = ?F

  TYPE_FIXNUM = 'i'.freeze
  TYPE_FIXNUM_ch = ?i

  TYPE_FLOAT = 'f'.freeze
  TYPE_FLOAT_ch = ?f

  TYPE_HASH = '{'.freeze
  TYPE_HASH_DEF = '}'.freeze

  TYPE_HASH_DEF_ch = ?}
  TYPE_HASH_ch = ?{

  TYPE_IVAR = 'I'.freeze
  TYPE_IVAR_ch = ?I

  TYPE_LINK = '@'.freeze
  TYPE_LINK_ch = ?@

  TYPE_MODULE = 'm'.freeze
  TYPE_MODULE_ch = ?m

  TYPE_MODULE_OLD = 'M'.freeze  # no specs
  TYPE_MODULE_OLD_ch = ?M   # no specs

  TYPE_NIL = '0'.freeze
  TYPE_NIL_ch = ?0

  TYPE_OBJECT = 'o'.freeze
  TYPE_OBJECT_ch = ?o

  TYPE_REGEXP = '/'.freeze
  TYPE_REGEXP_ch = ?/

  TYPE_STRING = '"'.freeze
  TYPE_STRING_ch = ?"

  TYPE_STRUCT = 'S'.freeze
  TYPE_STRUCT_ch = ?S

  TYPE_SYMBOL = ':'.freeze
  TYPE_SYMBOL_ch = ?:

  TYPE_SYMLINK = ';'.freeze
  TYPE_SYMLINK_ch = ?;

  TYPE_TRUE = 'T'.freeze
  TYPE_TRUE_ch = ?T

  TYPE_UCLASS = 'C'.freeze
  TYPE_UCLASS_ch = ?C

  TYPE_USERDEF = 'u'.freeze
  TYPE_USERDEF_ch = ?u

  TYPE_USRMARSHAL = 'U'.freeze
  TYPE_USRMARSHAL_ch = ?U
end
Marshal.__freeze_constants

