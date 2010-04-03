# file marshal0.rb 
# first opening of Marshal, so bootstrap can use non-dynamic constants

module Marshal

  MAJOR_VERSION = 4
  MINOR_VERSION = 8

  VERSION_STRING = "\x04\x08"

  TYPE_ARRAY = '['
  TYPE_ARRAY_ch = ?[
  TYPE_BIGNUM = 'l'
  TYPE_BIGNUM_ch = ?l
  TYPE_CLASS = 'c'
  TYPE_CLASS_ch = ?c
  TYPE_DATA = 'd'  # no specs
  TYPE_DATA_ch = ?d  # no specs
  TYPE_EXTENDED = 'e'
  TYPE_EXTENDED_ch = ?e
  TYPE_FALSE = 'F'
  TYPE_FALSE_ch = ?F
  TYPE_FIXNUM = 'i'
  TYPE_FIXNUM_ch = ?i
  TYPE_FLOAT = 'f'
  TYPE_FLOAT_ch = ?f
  TYPE_HASH = '{'
  TYPE_HASH_DEF = '}'
  TYPE_HASH_DEF_ch = ?}
  TYPE_HASH_ch = ?{
  TYPE_IVAR = 'I'
  TYPE_IVAR_ch = ?I
  TYPE_LINK = '@'
  TYPE_LINK_ch = ?@
  TYPE_MODULE = 'm'
  TYPE_MODULE_OLD = 'M'  # no specs
  TYPE_MODULE_OLD_ch = ?M   # no specs
  TYPE_MODULE_ch = ?m
  TYPE_NIL = '0'
  TYPE_NIL_ch = ?0
  TYPE_OBJECT = 'o'
  TYPE_OBJECT_ch = ?o
  TYPE_REGEXP = '/'
  TYPE_REGEXP_ch = ?/
  TYPE_STRING = '"'
  TYPE_STRING_ch = ?"
  TYPE_STRUCT = 'S'
  TYPE_STRUCT_ch = ?S
  TYPE_SYMBOL = ':'
  TYPE_SYMBOL_ch = ?:
  TYPE_SYMLINK = ';'
  TYPE_SYMLINK_ch = ?; 
  TYPE_TRUE = 'T'
  TYPE_TRUE_ch = ?T
  TYPE_UCLASS = 'C'
  TYPE_UCLASS_ch = ?C
  TYPE_USERDEF = 'u'
  TYPE_USERDEF_ch = ?u
  TYPE_USRMARSHAL = 'U'
  TYPE_USRMARSHAL_ch = ?U

end
Marshal.__freeze_constants

