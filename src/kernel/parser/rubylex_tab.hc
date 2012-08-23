/* Generated file, do not edit.  Editing to be done in git/src/kernel/parser */
/* C code produced by gperf version 2.7.2 */
/* Command-line: gperf -p -j1 -i 1 -g -o -t -N rb_reserved_word -k'1,3,$' ./keywords  */
class kwtable {
 public:
  int id[2];
  LexStateKind state;
  char name[16];
  AstSymbolEType a_sym; // a_sym_INVALID if lexer result need not be
			     //   encapsulated in an RpNameToken
};

enum { TOTAL_KEYWORDS  = 40,
       MIN_WORD_LENGTH = 2,
       MAX_WORD_LENGTH = 8,
       MIN_HASH_VALUE = 6,
       MAX_HASH_VALUE = 55 };
/* maximum key range = 50, duplicates = 0 */

static inline unsigned int hash(const char *str, unsigned int len)
{
  static const unsigned char asso_values[] =
    {
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 11, 56, 56, 36, 56,  1, 37,
      31,  1, 56, 56, 56, 56, 29, 56,  1, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56,  1, 56, 32,  1,  2,
       1,  1,  4, 23, 56, 17, 56, 20,  9,  2,
       9, 26, 14, 56,  5,  1,  1, 16, 56, 21,
      20,  9, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
      56, 56, 56, 56, 56, 56
    };
  int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

inline const kwtable* mel_reserved_word(const char *str, unsigned int len)
{
  static const kwtable wordlist[] =
    {
      {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}},
      {{kEND, kEND}, EXPR_END, "end", a_sym_end},
      {{kELSE, kELSE}, EXPR_BEG, "else", a_sym_else},
      {{kCASE, kCASE}, EXPR_BEG, "case", a_sym_case},
      {{kENSURE, kENSURE}, EXPR_BEG, "ensure", a_sym_ensure},
      {{kMODULE, kMODULE}, EXPR_BEG, "module", a_sym_module},
      {{kELSIF, kELSIF}, EXPR_BEG, "elsif", a_sym_elsif},
      {{kDEF, kDEF}, EXPR_FNAME, "def", a_sym_def},
      {{kRESCUE, kRESCUE_MOD}, EXPR_MID, "rescue", a_sym_rescue},
      {{kNOT, kNOT}, EXPR_BEG, "not", a_sym_not},
      {{kTHEN, kTHEN}, EXPR_BEG, "then", a_sym_then},
      {{kYIELD, kYIELD}, EXPR_ARG, "yield", a_sym_yield},
      {{kFOR, kFOR}, EXPR_BEG, "for", a_sym_for},
      {{kSELF, kSELF}, EXPR_END, "self", a_sym_self},
      {{kFALSE, kFALSE}, EXPR_END, "false", a_sym_false},
      {{kRETRY, kRETRY}, EXPR_END, "retry", a_sym_retry},
      {{kRETURN, kRETURN}, EXPR_MID, "return", a_sym_return},
      {{kTRUE, kTRUE}, EXPR_END, "true", a_sym_true},
      {{kIF, kIF_MOD}, EXPR_BEG, "if", a_sym_if},
      {{kDEFINED, kDEFINED}, EXPR_ARG, "defined?", a_sym_definedQ},
      {{kSUPER, kSUPER}, EXPR_ARG, "super", a_sym_super},
      {{kUNDEF, kUNDEF}, EXPR_FNAME, "undef", a_sym_undef},
      {{kBREAK, kBREAK}, EXPR_MID, "break", a_sym_break},
      {{kIN, kIN}, EXPR_BEG, "in", a_sym_in},
      {{kDO, kDO}, EXPR_BEG, "do", a_sym_do},
      {{kNIL, kNIL}, EXPR_END, "nil", a_sym_nil},
      {{kUNTIL, kUNTIL_MOD}, EXPR_BEG, "until", a_sym_until},
      {{kUNLESS, kUNLESS_MOD}, EXPR_BEG, "unless", a_sym_unless},
      {{kOR, kOR}, EXPR_BEG, "or", a_sym_or},
      {{kNEXT, kNEXT}, EXPR_MID, "next", a_sym_next},
      {{kWHEN, kWHEN}, EXPR_BEG, "when", a_sym_next},
      {{kREDO, kREDO}, EXPR_END, "redo", a_sym_redo},
      {{kAND, kAND}, EXPR_BEG, "and", a_sym_and},
      {{kBEGIN, kBEGIN}, EXPR_BEG, "begin", a_sym_begin},
      {{k__LINE__, k__LINE__}, EXPR_END, "__LINE__", a_sym__LINE_},
      {{kCLASS, kCLASS}, EXPR_CLASS, "class", a_sym_class},
      {{k__FILE__, k__FILE__}, EXPR_END, "__FILE__", a_sym__FILE_},
      {{klEND, klEND}, EXPR_END, "END", a_sym_END},
      {{klBEGIN, klBEGIN}, EXPR_END, "BEGIN", a_sym_BEGIN},
      {{kWHILE, kWHILE_MOD}, EXPR_BEG, "while", a_sym_while},
      {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}},
      {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}}, {{0, 0}},
      {{kALIAS, kALIAS}, EXPR_FNAME, "alias", a_sym_alias}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
