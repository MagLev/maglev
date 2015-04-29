/* Generated file, do not edit.  Editing to be done in git/src/kernel/parser */
#if !defined(RUBYPARSER_H)
#define RUBYPARSER_H 1

// portions derived from the Rubinius melbourne parser

#include "rubyom.hf"
#include "rubyast.ht"

// typedef int64 quark;
typedef omObjSType NODE ;
typedef OopType QUID;

// QUID is a SmallInteger with value bits 0x0nnnnnnnnnnSTTT
//  S are the bits per ID_SCOPE_MASK
//  TTT are the bits holding token value 0.. tLAST_TOKEN+1
//  nnnnnnnnnn are the OopNumberType of a Symbol object
enum { 
       ID_TOK_MASK = 0xFFF ,

       ID_SCOPE_SHIFT = 12 ,
       ID_SCOPE_MASK  = 0xF ,
       ID_LOCAL    = 0x01 ,  // method(or block) temp or argument
       ID_INSTANCE = 0x02 ,  // instance variable
       ID_GLOBAL   = 0x03 ,  // Ruby global variable
       ID_ATTRSET  = 0x04 ,
       ID_CONST    = 0x05 ,  // Constant declaration or reference
       ID_CLASS    = 0x06 ,  // class instance variable
       ID_JUNK     = 0x07 ,
       ID_INTERNAL = ID_JUNK,

       ID_symOopNum_SHIFT = 16
};


typedef enum {
  EXPR_BEG =       1,  /* ignore newline, +/- is a sign. */
  EXPR_END =       2,  /* newline significant, +/- is a operator. */
  EXPR_ARG =       4,  /* newline significant, +/- is a operator. */
  EXPR_CMDARG =    8,  /* newline significant, +/- is a operator. */
  EXPR_ENDARG = 0x10,  /* newline significant, +/- is a operator, and unbound braces */
  EXPR_MID =    0x20,  /* newline significant, +/- is a operator. */
  EXPR_FNAME =  0x40,  /* ignore newline, no reserved words. */
  EXPR_DOT =    0x80,  /* right after `.' or `::', no reserved words. */
  EXPR_CLASS = 0x100,  /* immediate after `class', no here document. */
  EXPR_VALUE = 0x200,  /* like EXPR_BEG but label is disallowed */
  EXPR_ENDFN = 0x400   /* newline significant, +/- is a operator, and unbound braces */
} LexStateKind;

class rb_parse_state ;

// class YyStackElement {  // defined in rubyom.hf
// public:
//   NODE *obj;  
//   short state;
//};

// class YyStackData {  // defined in rubyom.hf
// public:
//   YyStackElement *mark;
//   YyStackElement *base;
//   YyStackElement *last
//   uint  stacksize ;
//};

class BitStack {
 private:
  int64 _word;

  enum { num_bits = 48 ,
         depth_mask = 0xFF ,
         lsb_shift = 8 };

 public:
  omObjSType *asSi() {
    return OOP_OF_SMALL_LONG_(_word);
  }
  int64 word() { return _word; }

  BoolType restoreFromSi(omObjSType *si) {
    if (OOP_IS_SMALL_INT(si)) {
      _word = OOP_TO_I64(si);
      return 1; 
    }
    return 0;
  }

  uint64 depth() {
    return _word & depth_mask ;
  }

  BoolType push(uint64 arg) {
    UTL_ASSERT(arg <= 1);
    int64 w = _word;
    int64 d = w & depth_mask ;
    if (d >= num_bits) {
      return 0;
    }
    w = ((w >> lsb_shift) << 1) | (arg & 1) ;
    _word = (w << lsb_shift) | ((d + 1) & depth_mask) ;
    return 1;
  }

  BoolType pop() {
    int64 w = _word;
    int64 d = w & depth_mask ;
    if (d > 0) {  
      w =  w >> (lsb_shift + 1) ;
      _word = (w << lsb_shift) | ((d - 1) & depth_mask) ;
      return 1;
    }
    return 0;
  }

  BoolType lexPop() {
    int64 w = _word;
    int64 d = w & depth_mask ;
    if (d > 0) { 
      w = w >> lsb_shift ;
      w = (w >> 1) | (w & 1) ;
      _word = (w << lsb_shift) | ((d - 1) & depth_mask) ;
      return 1;
    };
    return 0;
  }

  int64 topBit() {
    return (_word >> lsb_shift) & 1;
  }

  void initialize() {
    _word = 0;
  }
};

class VarTable
{
 public:
  VarTable *next;
  int allocatedSize;
  int size;
  QUID *list;

  void initialize(int aSize) {
    next = NULL;
    allocatedSize = aSize;
    size = 0;
  }

  void grow(rb_parse_state *ps);

  static VarTable* allocate(rb_parse_state *ps, int numElements);

  static VarTable* push(rb_parse_state *ps, VarTable *cur) {
    VarTable* res = VarTable::allocate(ps, 5);
    res->next = cur;
    return res;
  }

  VarTable* pop() {
    VarTable* res = this->next;
#if defined(FLG_DEBUG)
    this->next = NULL; 
    this->size = 0;
#endif
    return res;
  }

  int64 add(rb_parse_state *st, QUID id); // returns new size of receiver

  void removeLast();

  // omObjSType *asArrayOfSymbols(rb_parse_state *st);

  omObjSType *asArray(om *omPtr);
};

class LocalState 
{
 public:
  LocalState* prev;
  VarTable *variables;
  VarTable *block_vars;
 
  void initialize(rb_parse_state *ps) {
    prev = NULL;
    variables = VarTable::allocate(ps, 5);
    block_vars = NULL;
  } 

  static LocalState* allocate(rb_parse_state *ps);

  static LocalState* push(rb_parse_state *ps, LocalState* cur) {
    LocalState* res = LocalState::allocate(ps);
    res->prev = cur;
    return res;
  } 

  static LocalState* pop(LocalState* cur) {
    LocalState* tmp = cur->prev;
#if defined(FLG_DEBUG)
    cur->prev = NULL;
    cur->variables = NULL;
#endif
    return tmp;
  }
};

class bstring {
  // simplified version of bstring.h ,
  // derived from bstring.h , the bstring string library, which was 
  //  written by Paul Hsieh in 2002-2007, and is covered by the BSD open source license. 

 private:
  int memLen;
  int strLen;
  char *mem ;
 public:
  inline void initialize() {
    memLen = 0;
    strLen = 0;
    mem = NULL;
  }
  
  inline bstring() {
    initialize();
  }

  int64 len() { return strLen; }

  char* data() { return mem; }

  static void balloc(bstring *s, int64 len, rb_parse_state *ps);

  static bstring* new_(rb_parse_state *ps);

  static void btrunc(bstring *s, int64 len) {
    UTL_ASSERT(len >= 0);
    s->strLen = 0;
  }
  static void bcatcstr(bstring *s, const char* a, int64 a_len, rb_parse_state *ps) {
    balloc(s, a_len, ps);
    memcpy(s->data(), a, a_len);
    s->strLen = a_len;
  }

  void set_strLen(int64 v) { strLen = v; }

  static char* bdata(bstring *s) { return s->data(); }
  static int blength(bstring *s) { return s->len(); }
};

class StartPosition {
 public:
  const char* kind;
  int line;

  void initialize() {
    line = 0;
    kind = "unknown";
  }
};

class StartPositionList {
 private:
  StartPosition *list;
  int allocatedSize;
  int size;

  void allocate(rb_parse_state *ps, int newSize);

 public:
   void push_back(rb_parse_state *ps, int line, const char* which) {
     if (size >= allocatedSize) {
       int newSize = allocatedSize == 0 ? 20 : size * 2;
       allocate(ps, newSize);
     }
     list[size].line = line;
     list[size].kind = which;
     size += 1;
   }

   int pop_back() {
     int res = 0;
     if (size > 0) {
       res = list[size - 1].line;
       size -= 1;
     }
     return res;
   }

   StartPosition* back() {
     if (size > 0) {
       return &list[size-1];
     };
     return NULL;
   }
 
   void initialize() {
     allocatedSize = 0;
     size = 0;
     list = NULL;
   }
  
};

class ComStateType;

class rb_parse_state
{
 public:
  om *omPtr;

  // initalized for each compilation
  char* sourceFileName;  // was ruby_sourcefile in rubinius
  NODE **fileNameH;            
  StartPositionList start_lines; 
  LocalState* variables;
  ComStateType *cst;

  NODE **_nilH;  // always  contains OOP_NIL;
  inline NODE** nilH() { 
    UTL_ASSERT(*_nilH == ram_OOP_NIL);
    return _nilH;
  }
  NODE **lexvalH;  // the parser's yylval , holds value output of lexer 
  NODE **yyvalH;   // used to save yyvalO when growing the parser stack
  NODE **lex_strtermH;
  NODE **magicCommentsH;
  NODE **warningsH;
  NODE **evalScopeH; // NULL or handle to a RubyEvalScope

  enum { yystack_MAXDEPTH = 10000 ,
         yystack_START_DEPTH = 1000 };
  YyStackData yystack;  // initialized at session startup

  char *lex_pbeg;
  char *lex_p;
  char *lex_pend;
  bstring line_buffer;
  bstring *lex_lastline ;

  NODE **sourceStrH;
  char *sourceBytes; // body of a CByteArray , per compilation
  char *sourcePtr;
  char *sourceLimit;
  intptr_t  lineStartOffset; // zero based
  intptr_t  tokStartDelta;

  int64 tokenOffset() {
    // offset of start of token in bytes
    return lineStartOffset + lex_p - 1 - lex_pbeg ;
  }

  char *tokenbuf;
  intptr_t toksiz;
  intptr_t tokidx;

  BitStack cond_stack;
  BitStack cmdarg_stack;

  LexStateKind lex_state;
  int lex_str_used;

  int lineNumber ;  // a one-based line number
  inline int ruby_sourceline() { return lineNumber; }

  int ternary_colon; // set but not tested, for grammar debugging
  // int column; 

  //  int debug_lines;
  int heredoc_end; // a line number
  int class_nest;
  int in_single;
  int in_def;
  int errorCount;
  int firstErrorLine;

  BoolByteType end_seen;
  BoolByteType command_start;
  BoolByteType compile_for_eval;
  BoolByteType in_defined;
  BoolByteType verbose;
  BoolByteType inStrTerm;
  BoolByteType printWarnings;
  BoolByteType atEof;
  BoolByteType parserActive;

  omObjSType **astClassesH; // initialzed at session startup
  omObjSType **astSymbolsH; // initialzed at session startup

  const char* eofReason;
  ByteType charTypes[256];

  // large items at end of class
  char firstErrorReason[4096];
  OopType astSelectorIds[NUM_AST_SELECTORS]; // initialized at session startup

  omObjSType* clear_lex_strterm() {
    UTL_ASSERT(inStrTerm);
    inStrTerm = 0;
    omObjSType *res = *lex_strtermH;
    *lex_strtermH = ram_OOP_NIL;
    return res;
  }
  void set_lex_strterm(omObjSType *strTermO) {
    inStrTerm = 1;
    *lex_strtermH = strTermO;
  }
 
};

#endif  // PARSER_H
