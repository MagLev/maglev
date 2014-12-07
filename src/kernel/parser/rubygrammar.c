#ifndef lint
static const char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif

#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYPATCH 20100610

enum {  YYEMPTY  =  -1 }; 

#define YYPREFIX "yy"

#define YYPURE 1

/* # line 20 "grammar.y" */ 

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
/* include <stdbool.h>*/

/* include <assert.h>*/
/*   use Maglev VM assertion support*/
#define assert UTL_ASSERT

#include "om.hf"
#include "rubyparser.h"
#include "rubyast.hf"
#include "gemsup.hf"
#include "gemdo.hf"
#include "object.hf"
#include "comheap.hf"
#include "gcifloat.hf"
#include "floatprim.hf"
#include "doprimargs.hf"
#include "intloopsup.hf"
#include "om_inline.hf"
#include "unicode/ustring.h"
#include "unicode/umachine.h"
#include "unicode/utf.h"

#include "rubygrammar.h"

#ifndef isnumber
#define isnumber isdigit
#endif

#if !defined(TRUE)
#define TRUE  true
#endif
#if !defined(FALSE)
#define FALSE false
#endif

static inline int ismbchar(uint c) { return 0; }
static inline int mbclen(char c) { return 1; }

static int yyparse(rb_parse_state* parse_state);
static void yyStateError(int64 yystate, int yychar, rb_parse_state*ps);

/* ID_SCOPE_MASK , ID_LOCAL..ID_INTERNAL moved to parser.h*/


static int64 QUID_to_id(NODE* idO)
{
  UTL_ASSERT(OOP_IS_SMALL_INT(idO));
  return OOP_TO_I64(idO);
}

static NODE* quidToSymbolObj(NODE* q, rb_parse_state *ps)
{
  if (OOP_IS_SMALL_INT(q)) {
    int64 id = OOP_TO_I64(q);
    int64 oopNum = id >> ID_symOopNum_SHIFT ; 
    OopType symOid = BIT_TO_OOP(oopNum);
    om *omPtr = ps->omPtr;
    NODE *symO = om::LocatePomObj(omPtr, symOid);
    UTL_ASSERT( symO->classPtr()->isSymbolCls());
    return symO;
  } 
  return RpNameToken::symval(q, ps);
}

static BoolType is_notop_id(NODE* id);
static BoolType v_is_notop_id(int64 val);

static BoolType v_is_local_id(int64 val)
{
  return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)==ID_LOCAL && v_is_notop_id(val);
}

static BoolType is_local_id(NODE* id) { return v_is_local_id(QUID_to_id(id)); }

static BoolType v_is_global_id(int64 val) 
{
  return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_GLOBAL && v_is_notop_id(val);
}

static BoolType is_global_id(NODE *id) { return v_is_global_id(QUID_to_id(id)); }

static BoolType v_is_instance_id(int64 val)
{
  return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_INSTANCE && v_is_notop_id(val);
}

static BoolType is_instance_id(NODE *id) { return v_is_instance_id(QUID_to_id(id)); }

/* static BoolType is_attrset_id(QUID id)*/
/* {*/
/*   int64 val = QUID_to_id(id);*/
/*   return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_ATTRSET && v_is_notop_id(val);*/
/* }*/

static BoolType v_is_const_id(int64 val)
{
  return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_CONST && v_is_notop_id(val);
}

static BoolType is_const_id(NODE *id) { return v_is_const_id(QUID_to_id(id)); }

static BoolType v_is_class_id(int64 val)
{
  return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_CLASS && v_is_notop_id(val);
}

static BoolType is_class_id(NODE *id) { return v_is_class_id(QUID_to_id(id)); }

/* static BoolType is_junk_id(QUID id)*/
/* {*/
/*   int64 val = QUID_to_id(id);*/
/*   return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_JUNK && v_is_notop_id(val);*/
/* }*/

static BoolType is_asgn_or_id(NODE *id)
{
  int64 val = QUID_to_id(id);
  int64 scopeVal = (val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK ;
  return v_is_notop_id(val) && ( scopeVal == ID_GLOBAL ||
                                 scopeVal == ID_INSTANCE ||
				 scopeVal == ID_CLASS);
}


static YyStackElement* yygrowstack(rb_parse_state *ps, YyStackElement* markPtr)
{
  YyStackData *stk = &ps->yystack;
  UTL_ASSERT(markPtr == stk->mark);
  if (stk->stacksize >= rb_parse_state::yystack_MAXDEPTH) {
    return NULL;
  }
  int newSize = stk->stacksize == 0 ? rb_parse_state::yystack_START_DEPTH
				: stk->stacksize * 2;
  if (newSize > rb_parse_state::yystack_MAXDEPTH)
    newSize = rb_parse_state::yystack_MAXDEPTH;

  int64 numBytes = sizeof(YyStackElement) * newSize ;
  YyStackElement *base = (YyStackElement*)UtlMalloc(numBytes, "yygrowstack");
  int64 depth = -1;
  if (stk->stacksize > 0) {
    depth = stk->mark - stk->base;
    memcpy(base, stk->base, sizeof(YyStackElement)*stk->stacksize);
    UtlFree(stk->base);
  };
  stk->base = base;
  stk->mark = base + depth;
  stk->last = base + newSize - 1 ;
  stk->stacksize = newSize;
  for (YyStackElement *elem = stk->mark + 1; elem <= stk->last; elem++) {
    /* initialize newly allocated memory*/
    elem->state = 0;
    elem->obj = ram_OOP_NIL;
  }
  return stk->mark;
}

#define BITSTACK_PUSH(stack, n) (stack = (stack<<1)|((n)&1))
#define BITSTACK_POP(stack)     (stack >>= 1)
#define BITSTACK_LEXPOP(stack)  (stack = (stack >> 1) | (stack & 1))
#define BITSTACK_SET_P(stack)   (stack&1)

static int yyerror(const char *msg, rb_parse_state *ps)
{
  if (ps->firstErrorLine == -1) {
    ps->firstErrorLine = ps->lineNumber;
  }
  printf("%s:%d, %s\n", ps->sourceFileName, ps->lineNumber, msg);
  ps->errorCount += 1;

  return 1;
}

static void yytrap()
{
  return; /* place to set breakpoint*/
}

static void rb_warning(rb_parse_state* ps, const char* msg)
{
  char buf[1024];
  snprintf(buf, sizeof(buf), "WARNING, line %d: %s\n", ps->lineNumber, msg);
  if (ps->printWarnings) {
    printf("%s", buf);
  } else {
    if (*ps->warningsH == ram_OOP_NIL) {
      *ps->warningsH = om::NewString_(ps->omPtr, buf);
    } else {
      om::AppendToString(ps->omPtr, ps->warningsH, buf);
    }
  }
}

static void rb_compile_error(rb_parse_state* ps, const char* msg)
{
  if (ps->firstErrorReason[0] == '\0') {
    strlcpy(ps->firstErrorReason, msg, sizeof(ps->firstErrorReason));
    ps->firstErrorLine = ps->lineNumber;
  }
  yyerror(msg, ps);
}

static void rb_compile_error_override(rb_parse_state* ps, const char* msg)
{
  strlcpy(ps->firstErrorReason, msg, sizeof(ps->firstErrorReason));
  ps->firstErrorLine = ps->lineNumber;
  yyerror(msg, ps);
}

static void rb_compile_error_q(rb_parse_state* ps, const char* msg, omObjSType *quidO)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  char buf[256];
  buf[0] = '\0';
  if (OOP_IS_SMALL_INT(quidO)) {
    omObjSType *symO = quidToSymbolObj(quidO, ps);
    om::FetchCString_(symO, buf, sizeof(buf));
  } 
  strlcpy(ps->firstErrorReason, msg, sizeof(ps->firstErrorReason)) ; 
  strlcat(ps->firstErrorReason, ", ", sizeof(ps->firstErrorReason));
  strlcat(ps->firstErrorReason, buf, sizeof(ps->firstErrorReason)); 
  yyerror(ps->firstErrorReason, ps); 
}

static void rb_compile_error(const char* msg, rb_parse_state* ps)
{
  rb_compile_error(ps, msg);
}
 
static void COND_PUSH(rb_parse_state *ps, uint64 n)
{
  if (! ps->cond_stack.push(n)) {
    rb_compile_error("cond_stack_overflow", ps);
  }
}

static void COND_POP(rb_parse_state *ps)
{
  if (! ps->cond_stack.pop()) {
    rb_compile_error("cond_stack_underflow", ps);
  }
}

static void COND_LEXPOP(rb_parse_state *ps)
{
  /* no underflow check*/
  ps->cond_stack.lexPop();
}

static int64 COND_P(rb_parse_state *ps)        
{
  return ps->cond_stack.topBit();
}

#if defined(FLG_DEBUG)
static int trapD = 10;
static int debugCmdArg = 0;
#endif

static void CMDARG_PUSH(rb_parse_state *ps, uint64 n)
{
  if (! ps->cmdarg_stack.push(n)) {
    rb_compile_error("cmdarg_stack_overflow", ps);
  }
#if defined(FLG_DEBUG)
  int d = ps->cmdarg_stack.depth();
  if (d > trapD) {
    yytrap();
  }
  if (debugCmdArg) {
    printf("CMDARG_PUSH, cmdarg_stack 0x%lx\n", ps->cmdarg_stack.word());
  }
#endif
}

static void CMDARG_LEXPOP(rb_parse_state *ps)
{
  ps->cmdarg_stack.lexPop(); /* no underflow check*/
#if defined(FLG_DEBUG)
  if (debugCmdArg) {
    printf("CMDARG_LEXPOP, cmdarg_stack 0x%lx\n", ps->cmdarg_stack.word());
  }
#endif
}

static void rParenLexPop(rb_parse_state *ps)
{
  COND_LEXPOP(ps);
  CMDARG_LEXPOP(ps);
}

static int64 CMDARG_P(rb_parse_state *ps)        
{
  return ps->cmdarg_stack.topBit();
}


static void rb_backref_error(NODE* n, rb_parse_state* ps);

static void local_push(rb_parse_state*, int cnt);
static void local_pop(rb_parse_state*);

static int64 local_cnt(rb_parse_state *st, NODE *idO);

static int local_id(rb_parse_state* ps, NODE* quid);
static int eval_local_id(rb_parse_state *st, NODE* idO);

static void tokadd(char c, rb_parse_state *parse_state);
static int tokadd_utf8(rb_parse_state *parse_state, int string_literal, int symbol_literal, int regexp_literal);

static NODE* gettable(rb_parse_state *parse_state, NODE **idH);

static void push_start_line(rb_parse_state* ps, int line, const char* which) 
{
  ps->start_lines.push_back(ps, line, which);
}

static void PUSH_LINE(rb_parse_state* ps, const char* which)
{
  push_start_line(ps, ps->ruby_sourceline(), which);
}

static int POP_LINE(rb_parse_state* ps) 
{ 
  /* maglev had premature_eof at call sites*/
  return ps->start_lines.pop_back(); 
}

static NODE* rb_parser_sym(const char *name, rb_parse_state *ps);

rb_parse_state *alloc_parse_state();

static uint64 scan_oct(const char *start, int len, int *retlen);
static uint64 scan_hex(const char *start, int len, int *retlen);

static void reset_block(rb_parse_state *parse_state);
static NODE* get_block_vars(rb_parse_state *parse_state);
static void  pop_block_vars(rb_parse_state *parse_state);

static NODE* asQuid(NODE* idO, rb_parse_state *ps);

enum { RE_OPTION_IGNORECASE   = 1, 
       RE_OPTION_EXTENDED     = 2,
       RE_OPTION_MULTILINE    = 4,
       RE_OPTION_DONT_CAPTURE_GROUP = 0x80,
       RE_OPTION_CAPTURE_GROUP      = 0x100,
       RE_OPTION_ONCE               = 0x2000
};

enum { NODE_STRTERM = 1,
       NODE_HEREDOC = 2 };

#define NEW_BLOCK_VAR(b, v) NEW_NODE(NODE_BLOCK_PASS, 0, b, v)
/* -----------------------------------------*/

static NODE* int64ToSi(int64 v)
{
  return OOP_OF_SMALL_LONG_(v);
}

static int64 siToI64(NODE *o)
{
  UTL_ASSERT(OOP_IS_SMALL_INT(o));
  return OOP_TO_I64(o);
}

static NODE* NEW_STR( const char* str, int64 len, rb_parse_state *ps)
{
  return om::NewString__(ps->omPtr, (ByteType*)str, len);
}

static NODE* NEW_STR( bstring *str, rb_parse_state *ps)
{
  return om::NewString__(ps->omPtr, (ByteType*)str->data() , str->len());
}

int64 RubyLexStrTerm::incrementNest(NODE **objH, int delta, rb_parse_state *ps)
{
  int64 v = om::FetchSmallInt_(objH, nest_ofs);
  v += delta;
  om::StoreSmallInt_(ps->omPtr, objH, nest_ofs, v);
  return v;
}

static NODE* NEW_STRTERM(short func, int term, int paren, rb_parse_state *ps)
{
  return RubyLexStrTerm::newStrTerm(func, term, paren, ps);  /* nest set to zero*/
}

#if defined(FLG_DEBUG)
static YtokenEType tokenType(int yychar)
{
  return (YtokenEType)yychar;
}
static int yTraceLevel = 0;
static int yydebug = 0;

static void yTrace(rb_parse_state *ps, const char*str)
{
  if (yTraceLevel > 0) {
    printf("line %d:   %s\n", ps->lineNumber, str);
  }
}
#else
enum { yydebug = 0,
       yTraceLevel = 0 };
static inline void yTrace(rb_parse_state *ps, const char*str) { return; }
#endif

NODE* RubyLexStrTerm::newStrTerm(short func, int term, int paren, rb_parse_state *ps)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  NODE **clsH = scp.add( om::FetchOop(*ps->astClassesH, my_cls));
  NODE **resH = scp.add( om::NewObj(omPtr, clsH));
  om::StoreSmallInt_(omPtr, resH, kind_ofs, NODE_STRTERM );
  om::StoreSmallInt_(omPtr, resH, a_ofs, func );
  om::StoreSmallInt_(omPtr, resH, b_ofs, term );
  om::StoreSmallInt_(omPtr, resH, c_ofs, paren );
  int64 d = ps->ruby_sourceline();
  d = (d << 32) | (ps->lineStartOffset & 0x7FFFFFFF);
  om::StoreSmallInt_(omPtr, resH, d_ofs, d);
  om::StoreSmallInt_(omPtr, resH, nest_ofs, 0 );
  return *resH;
}

NODE* RubyLexStrTerm::newHereDoc( rb_parse_state *ps,
       const char* tokStr, int64 tokLen, int64 ndNth, bstring* saveLine)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  NODE **litH = scp.add(om::NewString__(omPtr, (ByteType*)tokStr, tokLen));
  NODE **nthH = scp.add( LrgInt64ToOop(omPtr, ndNth));
  NODE **origH = scp.add(om::NewString__(omPtr, (ByteType*)saveLine->data(),
                                                        saveLine->len()));
  NODE **clsH = scp.add( om::FetchOop(*ps->astClassesH, my_cls));
  NODE **resH = scp.add( om::NewObj(omPtr, clsH));
  om::StoreSmallInt_(omPtr, resH, kind_ofs, NODE_HEREDOC );
  om::StoreOop(omPtr, resH, a_ofs, litH);
  om::StoreOop(omPtr, resH, b_ofs, nthH);
  om::StoreOop(omPtr, resH, c_ofs, origH);
  int64 d = ps->lineNumber;
  d = (d << 32) | (ps->lineStartOffset & 0x7FFFFFFF);
  om::StoreSmallInt_(omPtr, resH, d_ofs, d);
  if (yTraceLevel > 0) {
    printf("newHereDoc line %d lineStartOffset %ld \n", 
	ps->lineNumber, ps->lineStartOffset);
  }
  return *resH;
}

UTL_DEBUG_DEF( static int heredoc_restore_count = 0; )

static void heredoc_restore(rb_parse_state *ps)
{
  UTL_DEBUG_DEF( heredoc_restore_count += 1; )
  NODE **hereH = ps->lex_strtermH ;
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  UTL_ASSERT( RubyLexStrTerm::kind(*hereH) == NODE_HEREDOC );
  UTL_ASSERT(ps->inStrTerm);
  NODE **ndOrigH =  scp.add( RubyLexStrTerm::ndOrig(*hereH));
  int64 nd_orig_size = om::FetchSize_(*ndOrigH);

  if (ps->lex_lastline == &ps->line_buffer) {
    ps->lex_lastline = bstring::new_(ps);
  }
  bstring* line = ps->lex_lastline;
  bstring::balloc(line, nd_orig_size + 1/*ensure non-zero allocation*/, ps);
  char* lData = line->data();
  UTL_DEBUG_DEF( int64 numRet = )
    om::FetchBytes_(*ndOrigH, 0, nd_orig_size, (ByteType*)lData);
  line->set_strLen(nd_orig_size);
  UTL_ASSERT( numRet == nd_orig_size );

  ps->lex_pbeg = lData ;
  ps->lex_pend = lData + nd_orig_size ;
  ps->lex_p =    lData + RubyLexStrTerm::ndNth(*hereH);
  ps->heredoc_end =  ps->ruby_sourceline();
  ps->lineNumber = RubyLexStrTerm::lineNum(*hereH);
  ps->lineStartOffset = RubyLexStrTerm::lineStartOffset(*hereH);

  if (yTraceLevel > 0) {
    printf("heredoc_restore restored to line %d lineStartOffset %ld tokenOffset %ld \n",
        ps->lineNumber, ps->lineStartOffset, ps->tokenOffset() );
  }
}

static NODE* assignable(NODE **idH, NODE* srcOffset, NODE **valH, rb_parse_state *ps);

/* # line 516 "rubygrammar.c" */ 
/* Parameters sent to lex. (prototype expected to be hand coded)*/
/*extern int YYPARSE_DECL();*/
/*extern int YYLEX_DECL();*/

enum {  YYERRCODE = 256 };
static const short yyUnifiedTable[] = 
{
/* start of table lhs */  -1, 
   97,    0,   19,   20,   21,   21,   21,   21,  100,   22,
   22,   22,   22,   22,   22,   22,   22,   22,   22,  101,
   22,   22,   22,   22,   22,   22,   22,   22,   22,   22,
   22,   22,   22,   22,   23,   23,   23,   23,   23,   23,
   29,   27,   27,   27,   27,   27,   56,   56,   56,  103,
  104,   74,   26,   26,   26,   26,   26,   26,   26,   26,
   79,   79,   82,   82,   81,   81,   81,   81,   81,   81,
   83,   83,   80,   80,  102,   84,   84,   84,   84,   84,
   84,   84,   84,   76,   76,   76,   76,   76,   76,   76,
   76,   91,   91,   18,   18,   18,   92,   92,   92,   92,
   92,   78,   78,   78,   66,  106,   66,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,   93,  105,  105,  105,  105,  105,  105,  105,
  105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
  105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
  105,  105,  105,  105,  105,  105,  105,  105,  105,  105,
  105,  105,  105,  105,   24,   24,   24,   24,   24,   24,
   24,   24,   24,   24,   24,   24,   24,   24,   24,   24,
   24,   24,   24,   24,   24,   24,   24,   24,   24,   24,
   24,   24,   24,   24,   24,   24,   24,   24,   24,   24,
   24,   24,   24,   24,   24,  108,   24,  109,   24,   24,
   30,   48,   48,   48,   48,   48,   48,   45,   45,   45,
   45,   46,   46,   42,   42,   42,   42,   42,   42,   42,
   42,   42,   43,   43,   43,   43,   43,   43,   43,   43,
   43,   43,   43,   43,  111,   47,   44,  112,   44,  113,
   44,   50,   49,   49,   40,   40,   53,   53,   53,   25,
   25,   25,   25,   25,   25,   25,   25,   25,  114,   25,
  115,   25,   25,   25,   25,   25,   25,   25,   25,   25,
   25,   25,  116,   25,   25,   25,   25,  117,   25,  119,
   25,  120,  122,   25,  123,  124,   25,  125,   25,  126,
   25,  127,   25,  128,  129,  130,   25,  131,   25,  133,
  134,   25,  135,   25,  136,   25,  138,  139,   25,   25,
   25,   25,   25,   31,  118,  118,  118,  118,  121,  121,
  121,   32,   32,   33,   33,   69,   69,   72,   72,   70,
   70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
   70,   71,   71,   71,   71,  140,  141,   75,   55,   55,
   55,   28,   28,   28,   28,   28,   28,   28,   28,  142,
  143,   73,  144,  145,   73,   34,   41,   41,   41,   35,
   35,   36,   36,   37,   37,   37,   38,   38,   39,   39,
   15,   15,   15,    2,    3,    3,    3,    4,    5,    6,
   10,   10,   12,   12,   14,   14,   11,   11,   13,   13,
    7,    7,    8,    8,    9,  146,    9,  147,    9,   68,
   68,   68,   68,   87,   86,   86,   86,   86,   17,   16,
   16,   16,   16,   85,   85,   85,   85,   85,   85,   85,
   85,   85,   85,   85,   51,   52,   67,   67,   54,  148,
   54,   54,   57,   57,   58,   58,   58,   58,   58,   58,
   58,   58,   58,   95,   95,   95,   95,   95,   96,   96,
   60,   59,   59,  149,  149,   94,   94,  150,  150,   61,
   62,   62,    1,  151,    1,   63,   63,   63,   64,   64,
   65,   65,   88,   88,   88,   89,   89,   89,   89,   90,
   90,   90,  137,  137,   98,   98,  107,  107,  110,  110,
  110,  132,  132,   99,   99,   77,
/* start of table len */  2, 
    0,    2,    4,    2,    1,    1,    3,    2,    0,    4,
    3,    3,    3,    2,    3,    3,    3,    3,    3,    0,
    5,    4,    3,    3,    3,    4,    5,    5,    5,    3,
    3,    3,    3,    1,    1,    3,    3,    2,    2,    1,
    1,    1,    1,    2,    2,    2,    1,    4,    4,    0,
    0,    6,    2,    3,    4,    5,    4,    5,    2,    2,
    1,    3,    1,    3,    1,    2,    3,    2,    2,    1,
    1,    3,    2,    3,    3,    1,    2,    3,    3,    3,
    3,    2,    1,    1,    2,    3,    3,    3,    3,    2,
    1,    1,    1,    2,    1,    3,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    0,    4,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    3,    5,    3,    4,    5,    5,
    5,    5,    4,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    4,    4,    2,    2,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    3,    3,    3,    2,
    2,    3,    3,    3,    3,    0,    4,    0,    6,    1,
    1,    1,    2,    2,    5,    2,    3,    3,    4,    4,
    6,    1,    1,    1,    2,    5,    2,    5,    4,    7,
    3,    1,    4,    3,    5,    7,    2,    5,    4,    6,
    7,    9,    3,    1,    0,    2,    1,    0,    3,    0,
    4,    2,    2,    1,    1,    3,    3,    4,    2,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    0,    4,
    0,    5,    3,    3,    2,    2,    3,    3,    1,    4,
    3,    1,    0,    6,    2,    1,    2,    0,    7,    0,
    7,    0,    0,    7,    0,    0,    7,    0,    6,    0,
    5,    0,    6,    0,    0,    0,   10,    0,    6,    0,
    0,    8,    0,    5,    0,    6,    0,    0,    9,    1,
    1,    1,    1,    1,    1,    1,    1,    2,    1,    1,
    1,    1,    5,    1,    2,    1,    1,    1,    3,    1,
    2,    4,    7,    6,    4,    3,    5,    4,    2,    1,
    2,    1,    2,    1,    3,    0,    0,    6,    2,    4,
    4,    2,    4,    4,    3,    3,    2,    2,    1,    0,
    0,    6,    0,    0,    6,    5,    1,    4,    2,    1,
    1,    6,    1,    1,    1,    1,    2,    1,    2,    1,
    1,    1,    1,    1,    1,    1,    2,    3,    3,    3,
    3,    3,    0,    3,    1,    2,    3,    3,    0,    3,
    0,    2,    0,    2,    1,    0,    3,    0,    4,    1,
    1,    1,    1,    2,    1,    1,    1,    1,    3,    1,
    1,    2,    2,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    0,
    4,    2,    4,    2,    6,    4,    4,    2,    4,    2,
    2,    1,    0,    1,    1,    1,    1,    1,    1,    3,
    3,    1,    3,    1,    1,    2,    1,    1,    1,    2,
    2,    1,    1,    0,    5,    1,    2,    2,    1,    3,
    3,    2,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    0,    1,    0,    1,    0,    1,
    1,    1,    1,    1,    2,    0,
/* start of table defred */  1, 
    0,    0,    0,    0,    0,    0,    0,  279,  298,  300,
    0,  302,  305,  314,    0,    0,  332,  333,    0,    0,
    0,  450,  449,  451,  452,    0,    0,    0,   20,    0,
  454,  453,    0,    0,  446,  445,    0,  448,  423,  440,
  441,  405,  457,  458,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  421,  423,    0,    0,    0,    0,
    0,  271,    0,  406,  272,  273,  274,  275,  270,  401,
  403,    2,    0,    0,    0,    0,    0,    0,   35,    0,
    0,  276,    0,    0,   43,    0,    0,    5,    0,    0,
   61,    0,   71,    0,  402,    0,    0,  330,  331,  289,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  334,    0,  277,  455,    0,   95,  323,  143,  154,  144,
  167,  140,  160,  150,  149,  165,  148,  147,  142,  168,
  152,  141,  155,  159,  161,  153,  146,  162,  169,  164,
    0,    0,    0,    0,  139,  158,  157,  170,  171,  172,
  173,  174,  138,  145,  136,  137,    0,    0,    0,   99,
    0,  129,  130,  127,  111,  112,  113,  116,  118,  114,
  131,  132,  119,  120,  124,  115,  117,  108,  109,  110,
  121,  122,  123,  125,  126,  128,  133,  494,    0,  493,
  325,  100,  101,  163,  156,  166,  151,  134,  135,   97,
   98,  104,    0,  105,  103,  102,    0,    0,    0,  523,
  522,    0,    0,    0,  524,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  234,    0,    0,    0,   45,  242,
    0,    0,  499,    0,    0,    0,   46,   44,    0,   60,
    0,    0,  378,   59,   38,    0,    9,  518,    0,    0,
    0,    0,  195,    0,    0,  506,  508,  507,  377,  509,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  222,    0,    0,    0,  496,    0,    0,    0,   69,
    0,  437,  436,  438,    0,  434,  435,    0,    0,    0,
    0,    0,    0,    0,    0,  210,   39,  211,  407,    4,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  218,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  366,  369,  383,  380,  297,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   73,  372,    0,  295,    0,    0,   92,
    0,   94,  442,  443,    0,  460,  318,  459,    0,    0,
  286,    0,    0,  514,  513,  327,    0,  106,    0,    0,
    0,    0,    0,    0,    0,  525,    0,    0,    0,    0,
    0,    0,    0,  346,  347,    0,  502,    0,    0,  262,
    0,    0,    0,    0,    0,  235,  264,    0,    0,  237,
    0,    0,  291,    0,    0,  257,  256,    0,    0,    0,
    0,    0,   11,   13,   12,    0,  293,    0,    0,    0,
  425,  428,  426,  409,  424,    0,    0,    0,    0,  283,
    0,    0,    0,  223,    0,  520,  224,  287,    0,  226,
    0,  498,  288,  497,    0,    0,    0,    0,  439,  408,
  422,  410,  411,  412,  415,    0,  417,    0,  418,    0,
    0,    0,   15,   16,   17,   18,   19,   36,   37,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  375,    0,    0,    0,    0,    0,  376,    0,
    0,   25,    0,    0,    0,   30,    0,    0,   23,  265,
    0,   31,   24,    0,   33,    0,   67,   74,   50,   54,
    0,  462,    0,    0,    0,    0,    0,   96,    0,    0,
    0,    0,    0,  476,  475,  474,  477,  485,  489,  488,
  484,    0,    0,    0,    0,  482,  472,    0,  479,    0,
    0,    0,    0,  280,    0,    0,  393,  337,  336,    0,
    0,    0,    0,    0,    0,    0,  341,  340,  303,  339,
  306,    0,    0,    0,    0,  315,    0,  241,  501,    0,
    0,    0,    0,    0,    0,    0,  263,    0,    0,    0,
  500,    0,  290,    0,    0,    0,  260,  254,    0,    0,
    0,    0,    0,    0,    0,  228,   10,    0,    0,    0,
   22,    0,    0,    0,    0,    0,  227,    0,  266,    0,
    0,    0,    0,  414,  416,  420,    0,    0,    0,  364,
    0,  367,  362,  384,  381,    0,    0,  374,    0,    0,
    0,  233,  373,    0,  232,   75,    0,   26,  371,   49,
  370,   48,  269,    0,    0,   72,    0,  321,    0,    0,
  324,    0,  328,    0,    0,    0,  464,    0,  470,  492,
    0,  471,    0,  468,  486,  490,  107,    0,    0,  395,
  396,    0,    0,  344,    0,  338,    0,    0,    0,    0,
  311,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  239,    0,    0,    0,    0,
    0,  247,  259,    0,    0,  229,    0,    0,  230,    0,
   21,    0,  430,  431,  432,  433,  427,  282,    0,    0,
    0,    0,  363,    0,    0,  348,    0,    0,    0,    0,
   29,    0,   58,    0,   27,    0,   28,   56,    0,    0,
    0,   51,    0,  461,  319,  495,    0,  481,    0,  326,
    0,  483,  491,    0,    0,    0,  480,    0,    0,  398,
  345,    0,    3,  400,    0,    0,  342,    0,  389,    0,
    0,  313,  309,    0,    0,    0,  236,    0,  238,  253,
    0,    0,  244,    0,  261,    0,    0,  294,  429,  225,
    0,    0,    0,    0,    0,    0,    0,  361,  365,    0,
    0,    0,    0,  268,    0,    0,    0,  463,  469,    0,
  466,  467,  397,    0,  399,    0,  299,  301,    0,    0,
  304,  307,  316,    0,    0,    0,  243,    0,  249,    0,
  231,    0,    0,    0,    0,    0,    0,    0,    0,  349,
  368,  385,  382,    0,  322,    0,    0,    0,    0,  388,
  390,  391,  386,    0,  240,  245,    0,    0,    0,  248,
  358,    0,    0,    0,    0,    0,    0,    0,  352,   52,
  329,  465,  392,    0,    0,    0,    0,  250,    0,  357,
    0,    0,  343,  317,  246,    0,  251,  354,    0,    0,
  353,  252,
/* start of table dgoto */  1, 
  189,   62,   63,   64,   65,   66,  288,  252,  435,   67,
   68,  291,  293,  466,   69,   70,   71,  110,  379,  380,
   73,   74,   75,   76,   77,   78,   79,   80,  382,  226,
  254,  796,  797,  584,  883,  576,  699,  789,  793,  228,
  710,  229,  617,  417,  662,  663,  240,  270,  406,  607,
   82,  231,  532,  367,   84,   85,  563,  564,  565,  566,
  783,  689,  274,  232,  233,  203,  234,  747,  393,  754,
  652,  755,  357,  540,  336,  235,   88,  204,   89,   90,
   91,  265,   92,   93,  236,  286,   95,  115,  547,  513,
  116,  206,  260,  568,  569,  570,    2,  212,  213,  426,
  250,  404,  677,  835,  193,  573,  249,  428,  495,  447,
  241,  620,  730,  207,  442,  628,  208,  580,  209,  216,
  589,  714,  217,  715,  214,  384,  385,  218,  720,  884,
  544,  581,  541,  773,  372,  377,  376,  552,  777,  506,
  757,  508,  759,  507,  758,  633,  632,  543,  571,  572,
  373,
/* start of table sindex */  0, 
    0,14085,14503,19686,19986,18103,17800,    0,    0,    0,
  129,    0,    0,    0,14808,14808,    0,    0,14808,   44,
   54,    0,    0,    0,    0,16208,17705,   68,    0,    3,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,17113,17113, 2356, -198,14273,16208,
16108,16308,20086,18196,    0,    0,  168,  248, -178,17213,
17113,    0,  -20,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  129,  830,   -4, 8977,    0,   62,    0,  -26,
   41,    0,   35,  -14,    0,   57,  304,    0,  323,19786,
    0,  364,    0,    0,    0,  119,  830,    0,    0,    0,
   44,   54,   68,    0,    0,16208,  -91,14085,   56,   61,
    0,   50,    0,    0,  119,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   -8,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  412,    0,    0,    0,14085,16208,16208,    0,
    0,    0,  394,16208,    0,16208,16208,19886,17113,  163,
17113,17113,17113, 8977,    0,  118,   63,  433,    0,    0,
  148,  448,    0,  183,  487,    0,    0,    0,14603,    0,
14908,14808,    0,    0,    0,  246,    0,    0,  513,  444,
14085,  498,    0,   92,  252,    0,    0,    0,    0,    0,
  217,14273,  529,    0,  530,   -4,17113,   68,  130,  484,
  133,    0,  159,  456,  133,    0,  280,  107,    0,    0,
    0,    0,    0,    0,  696,    0,    0,  757,  639,  224,
  763,  243, -206,  285,  293,    0,    0,    0,    0,    0,
14393,16208,16208,16208,16208,14503,16208,16208,17113,17113,
17113,17113,17113,17113,17113,17113,17113,17113,17113,17113,
17113,17113,17113,    0,17113,17113,17113,17113,17113,17113,
17113,17113,17113,17113,    0,    0,    0,    0,    0,18534,
18569,16108, 2356,  263,17213, 2356, 2356,17213,16408,16408,
14273,20086,  574,    0,    0,  275,    0,  513,   -4,    0,
    0,    0,    0,    0,  129,    0,    0,    0,18604, 2356,
    0,14085,16208,    0,    0,    0,  656,    0,  358,  362,
   -4,  439,  439,  361,  365,    0,  129,  199,  199,  328,
  143,    0,  366,    0,    0,    0,    0,  217,  604,    0,
17113,18907,18942,  312,15008,    0,    0,17113,15108,    0,
17113,17113,    0,  620,14708,    0,    0,   62,  619,   68,
   34,  624,    0,    0,    0,17800,    0,17113,14085,  541,
    0,    0,    0,    0,    0,18907,18942,17113,  629,    0,
    0,   68, 6553,    0,16508,    0,    0,    0,16308,    0,
17113,    0,    0,    0,    0,18977,19012,    0,    0,    0,
    0,    0,    0,    0,    0,   40,    0,  642,    0,17113,
17113,  830,    0,    0,    0,    0,    0,    0,    0,  252,
 1871, 1871, 1871, 1871,  689,  689, 9557, 9065, 1871, 1871,
 6527, 6527,  125,  125,17113,  689,  689,  861,  861,  201,
  145,  145,  252,  252,  252,  -57,  -57,  -57,  338,    0,
  339,   54,    0,    0,  341,  352,   54,  602,    0,17213,
 8977,    0,   54,   54, 8977,    0,17113,19457,    0,    0,
  653,    0,    0,    0,    0,  660,    0,    0,    0,    0,
  129,    0,16208,14085,    0,    0,   54,    0,   54,  441,
   95,18499,  646,    0,    0,    0,    0,    0,    0,    0,
    0,  470,14085,  129,  664,    0,    0,  666,    0,  671,
  415,  419,17800,    0,16608,  455,    0,    0,    0,14085,
  463,14085,16708,  469,14085,  361,    0,    0,    0,    0,
    0,    0,19047,19082,    0,    0,  389,    0,    0,  401,
  339,  403,  405,17113,17113,  118,    0,  704,17113,  118,
    0,19457,    0,17113, 8977,    9,    0,    0,  707,  712,
15208,  721, 2356, 2356,  722,    0,    0,16208, 8977,  643,
    0,14085,  488, 8977,    0,  723,    0,17113,    0,    0,
    0,    0,    0,    0,    0,    0,  252,  252, 7993,    0,
17994,    0,    0,    0,    0,17213,17113,    0,  275,17213,
17213,    0,    0,  275,    0,    0, 8977,    0,    0,    0,
    0,    0,    0,17113,16808,    0,  -57,    0,  129,  511,
    0,  739,    0,17113,   68,  517,    0,   87,    0,    0,
  -12,    0,  470,    0,    0,    0,    0,    0,  440,    0,
    0,14085,  522,    0,  147,    0,  455,17113,  744,  439,
    0,  525,  527,14085,14085,    0,    0,    0,    0,16208,
17113,17113,17113,  604,15308,    0,  604,  604,15408,  751,
15508,    0,    0,   62,   34,    0,   54,   54,    0,  152,
    0,  673,    0,    0,    0,    0,    0,    0, 6553,17113,
 5735,20186,    0,  683,  764,    0,14085,14085,14085, 8977,
    0, 8977,    0, 8977,    0, 8977,    0,    0, 8977,17113,
    0,    0,14085,    0,    0,    0,  656,    0,  773,    0,
  646,    0,    0,  666,  774,  666,    0,20186,  439,    0,
    0,14085,    0,    0,16208,  561,    0,  562,    0,16908,
14085,    0,    0,  570,  573,  199,    0,17113,    0,    0,
17113,  794,    0,  805,    0,17113,  810,    0,    0,    0,
 8977,  554,  515,  216,    0,  819,    0,    0,    0,19586,
  607,  614,  743,    0,14085,  616,14085,    0,    0,   87,
    0,    0,    0,14085,    0,  439,    0,    0,17113,  236,
    0,    0,    0, 8485,  604,15608,    0,15708,    0,  604,
    0,    0,20186,19125,19428,    0,  534,17417,20186,    0,
    0,    0,    0,  760,    0,  622,  666,  362,14085,    0,
    0,    0,    0,14085,    0,    0,17113,  843,17113,    0,
    0,    0,    0,    0,    0,20186,  542,  852,    0,    0,
    0,    0,    0,  147,  633,  604,15808,    0,  604,    0,
20186,  551,    0,    0,    0,17113,    0,    0,20186,  604,
    0,    0,
/* start of table rindex */  0, 
    0,  490,    0,    0,    0,    0,    0,    0,    0,    0,
15908,    0,    0,    0,13267,13390,    0,    0,13573, 4753,
 4218,    0,    0,    0,    0,    0,    0,17013,    0,    0,
    0,    0, 2457, 3448,    0,    0, 2578,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  414,    0,
  812,  786,  237,  777,    0,    0,  790, -156,    0,    0,
    0,    0, 8427,    0,    0,    0,    0,    0,    0,    0,
    0,    0, 1491, 1805, 7654,19353, 8735,19534,    0, 8824,
    0,    0,    0,19545,    0,13876,    0,    0,    0,  409,
    0,    0,    0,13693,    0,16008, 2197,    0,    0,    0,
 8919, 7443,  878, 6063, 6380,    0,    0,  414,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  226, 1034, 1162, 1265,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0, 1315, 1543, 1682,    0,
 1779,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0, 7690,    0,    0,    0,  393,    0,    0,    0,
    0,  260,  274,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,12233,    0,  974,    0, 2001,    0,    0,
    0, 2001,    0, 7935,    0, 7751,    0,    0,    0,    0,
    0,  879,    0,    0,    0,    0,    0,    0,17313,    0,
  136,    0,    0,    0, 9406,    0,    0,    0,    0,    0,
13998,  414,    0,  452,    0,  180,    0,  826,  832,    0,
  832,    0,  806,    0,  806,    0,    0,    0, 1020,    0,
 1136,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0, 9227, 9316,    0,    0,    0,    0,    0,
 1673,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  812,    0,14179,    0,    0,    0,    0,    0,    0,
  414,  424,  479,    0,    0,13509,    0,    0,  382,    0,
 6907,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  393,    0,    0,    0,    0,  395,    0,    0,  378,
 7560,    0,    0,    0,    0,    0,  659,    0,    0,    0,
    0,  585,    0,    0,    0,  637,    0, 8243, 2001,    0,
    0,    0,    0, 8332,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  893,    0,    0,  249,  271,  899,
  899,    0,    0,    0,    0,    0,    0,    0,  136,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  318,  899,  826,    0,  848,    0,    0,    0,  -52,    0,
  818,    0,    0,    0, 1878,    0,    0, 2081,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0, 6125,    0,    0,    0,    0,    0,    0,    0, 9708,
 1191,11585,11686,11799,11194,11282,11893,12135,11985,12041,
 8151, 8643,10494,10800,    0,11408,11496,10979,11105,10889,
10584,10674, 9797, 9887,10189, 7119, 7119, 7322, 5088, 3783,
 5623,16008,    0, 3883, 5188, 5523, 4318,    0,    0,    0,
12320,    0, 5958, 5958,12412,    0,    0, 9167,    0,    0,
    0,    0,    0, 2299,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  393, 6485, 6802,    0,    0, 7840,    0,
  899,    0,  106,    0,    0,    0,    0,    0,    0,    0,
    0,  282,  393,    0,  414,    0,    0,  414,    0,  414,
  576,    0,    0,    0,   24,  273,    0,    0,    0,  345,
 7014,  435,    0,    0,  235,    0,    0,    0,    0,    0,
    0,  837,    0,    0, 1011,    0,    0,    0,    0, 2913,
 4653, 3013, 3348,    0,    0,13176,    0, 2001,    0,    0,
    0,12871,    0,    0,   45,    0,    0,    0,  879,    0,
    0,    0,    0,    0,    0,    0,    0,    0,12448,    0,
    0,  136,    0,12510,  150,    0,    0,    0,    0, 1736,
  783, 1912, 2264,    0,    0,    0,10278,10368,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,13815,    0,
    0,    0,    0,17893,    0,    0,12597,    0,    0,    0,
    0,    0,    0,    0,    0,    0, 7322,    0,    0,    0,
    0,    0,    0,    0,  899,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   59,  492,    0,
    0,  212,  681,    0,  681,    0,  681,    0,  569,    0,
    0,    0,    0,  235,  235,  756,   84,  799, 1717,    0,
    0,    0,    0, 2001,    0,    0, 2001,  879,    0,    0,
    0,    0,    0,    0,  899,    0,   81,   81,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  826,    0,
  824,    0,    0,    0,  827,    0,  235,  235,  136,12691,
    0,12783,    0,12826,    0,12933,    0,    0,13025,    0,
 1388,    0,  393,    0,    0,    0,  395,    0,    0,    0,
    0,    0,    0,  414,  414,  414,    0,    0,    0,    0,
    0,  235,    0,    0,    0,    0,    0,    0,    0,    0,
  210,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  879,    0,  879,    0,    0,    0,    0,    0,    0,
13114,    0,    0,    0,  127,  828,  900,    0,    0,  831,
    0,    0,    0,    0,  136,    0,  393,    0,    0,    0,
    0,    0,    0,  393,    0,    0,    0,    0,    0,  681,
    0,    0,    0, 2001,  879,    0,    0,    0,    0,  879,
    0,  939,    0,    0,    0, 1039,    0,  844,    0,    0,
    0,    0,    0,    0,    0,    0,  414,  378,  345,    0,
    0,    0,    0,  235,    0,    0,    0,  879,    0,    0,
    0,  593,  105, 1243, 1587,    0,    0,  846,    0,    0,
    0,    0,    0,  681,    0,  879,    0,    0,  879,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  879,
    0,    0,
/* start of table gindex */  0, 
    0,    0,    0,  887,    0,    0,    0,  461, -189,    0,
    0,    0,    0,    0,    0,    0,   38,  956, -316,  155,
    0,   18,  287, 1091,   70,   -9,    8,    0, -124, 1801,
   -2,   67, -432, -540,    0,   89,    0,    0,    0,   25,
    0,  528,    0,    0,   -1,  -65,    2,  640, -168,   14,
  969, 1343, -296,    0, -226,    0,  209,  425,  295, -588,
 -352, -489,    0,  -15, -397,    0,   58,    0,    0,    0,
 -395,    0,  913, -375,    0,  460, 1476,  -19,  782,    0,
  -21, -101,  -85,  -13,  221,    0,   43, 1528,  -25,    0,
  -76,    5,   12, -557,  308,    0,    0,  -46,  929,    0,
    0,  -77,    0,    0,    0,    0,  -60,    0,    0,  213,
    0,    0,    0,    0,    0,    0,    0, -368,    0,    0,
 -380,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   46,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,
/* start of table table */  81, 
   81,  112,  112,  344,  353,  225,  225,  247,  591,  225,
  191,  611,  227,  227,  582,  421,  227,  192,  192,  243,
   97,  259,  244,  227,  567,  560,  300,  264,  230,  230,
  362,  347,  230,  526,  371,  271,  275,  375,  192,  280,
  521,  268,  358,  248,  202,  713,   81,  227,  227,  205,
  278,  611,  729,  535,   40,  550,  215,  227,  287,   86,
   86,  113,  113,  410,  202,  192,  651,  297,  394,  205,
  210,  644,  521,  111,  111,  269,  273,  248,  692,  624,
  694,  526,  526,  239,  383,   40,  341,  278,  221,  387,
  526,  388,  389,  242,  355,  370,  338,  356,  461,  782,
  243,  465,  265,  227,  248,   81,   86,  468,  403,  261,
  279,  654,  655,  355,   89,  478,  394,  394,  215,  211,
  366,  526,  111,  507,  560,  251,  526,   81,  561,  284,
  784,  342,  343,  294,  295,  786,   91,  437,  210,  446,
  342,  343,  446,  703,  507,  526,  478,  279,   89,  478,
  284,  469,  457,  342,  343,  368,   72,  419,  242,  111,
  439,  334,   89,   89,  478,   86,  332,  330,  446,  331,
   91,  333,  277,  445,  284,  284,  449,  473,  474,  475,
  476,  334,  342,  343,   91,   91,  332,  211,  594,  281,
   64,  333,  818,   72,  526,  284,  284,  342,  343,  290,
  458,  419,  451,  263,   81,  227,  227,  444,  210,  567,
   62,  227,  360,  227,  227,  391,  361,  277,  277,  526,
  281,  526,   94,   94,  114,  114,  114,  680,   89,  225,
  598,  225,  418,  342,  343,  163,  227,  334,  227,  227,
  264,  338,  332,  330,  526,  331,  686,  333,   81,  536,
   91,  782,  230,  337,  230,  230,  588,  211,  234,   81,
  526,  865,  263,  163,   86,  163,  419,  163,  526,   94,
  526,  450,  650,  281,  798,  392,  645,   70,  700,  292,
  526,  772,  877,  763,  163,  307,  308,  111,  768,  234,
  526,  473,  548,  526,  839,  841,  842,   70,   81,  227,
  227,  227,  227,   81,  227,  227,  342,  343,   86,  882,
  281,  526,  245,  595,  512,  517,  365,  519,  472,   86,
  523,  524,  473,  477,  346,  394,  271,  611,   94,  264,
  374,   55,  268,  611,   40,   40,  266,  559,  537,  227,
  586,  801,  227,  335,  549,  227,  227,  227,   81,  278,
  401,  514,  522,  431,  526,  526,  529,  533,   86,  622,
  625,   72,   89,   86,  349,  526,  269,  363,  364,   81,
  227,   89,  623,  531,  531,  345,  512,  517,   62,  340,
  514,  636,  637,  350,  307,  308,  221,  902,  369,  608,
  781,  320,  359,   91,  735,  432,  433,  348,  337,  619,
  394,  402,  526,  526,  473,  430,  627,  354,   86,  279,
  542,  549,  227,  514,  795,  702,  263,  881,  679,  526,
  844,  111,  284,  526,  567,  853,   81,   94,  618,   86,
  436,  549,  215,  590,  590,  558,  559,  192,  396,  726,
  320,  307,  308,  284,  526,  456,  309,  514,  210,   65,
  732,  526,  386,  473,  526,  378,  836,  669,  671,  401,
  611,   87,   87,  202,   68,  277,  309,  514,  205,   65,
  398,   94,  526,  526,  526,  526,  405,  879,  526,  587,
  526,  593,   94,  450,   68,  452,   86,  454,  408,  526,
  682,  409,   63,  526,  381,  381,  579,  211,  526,  526,
  381,  526,  381,  381,  702,  263,  583,  560,   87,  611,
  658,  561,   61,  659,  285,   70,  289,  227,  664,   66,
  876,   94,  309,  411,  670,  672,   94,  668,  312,  163,
  310,  163,  163,  163,  163,  526,  526,  322,  323,   66,
  227,   81,  516,  237,  516,  658,  238,  412,  526,  526,
  526,  423,  427,  697,  864,  807,  683,  438,  809,  810,
   81,  424,  425,  192,  450,  756,  429,   87,  549,  440,
  441,   94,  281,  309,  163,  163,  448,   81,  387,   81,
  453,  463,   81,  630,  192,  487,  678,  455,  381,  381,
  381,  381,   94,  478,  479,  806,   62,  737,  738,  531,
  467,   86,   87,  520,  514,  608,  470,  709,  526,  687,
  202,  734,  526,  526,  471,  205,  487,  538,  227,  487,
   86,  574,  539,  575,  779,  227,  387,  387,   83,   81,
  277,  583,  506,  585,  487,  592,   87,   86,  510,   86,
  526,  526,   86,  857,  596,  859,  526,  597,  278,   94,
   87,   87,  604,  227,  526,  526,  526,  227,  227,  551,
  613,  526,  621,  761,  626,  631,   87,  765,  767,  635,
  846,  669,  671,  646,  817,  277,  277,  394,  656,  657,
   76,  660,  455,  510,  510,  885,  886,   65,  820,   86,
  746,  890,  661,  560,  666,  562,  675,  561,  526,   81,
  676,  266,   68,  526,  681,  578,  684,  688,  279,  691,
   87,   81,   81,  814,  693,  510,   87,  227,  695,  908,
  111,   87,  696,  702,  774,  334,  329,  455,  455,  706,
  332,  330,  711,  331,  705,  333,  707,  915,  222,  712,
  917,  721,  813,  722,  870,  723,  866,  725,  824,  824,
  731,  922,  733,  812,   81,   81,   81,   66,  526,   86,
   87,  736,  739,  748,   94,   87,  414,  741,  416,  420,
   81,   86,   86,  553,  775,  554,  555,  556,  557,  776,
  780,  788,  328,   94,  792,  824,  742,  800,  802,   81,
  803,  815,  227,  743,  744,  506,  745,  819,   81,   79,
   94,  510,   94,   43,   44,   94,  829,  830,  825,  825,
   87,  431,  327,  838,   86,   86,   86,  840,  558,  559,
  111,  111,  507,   81,  847,  848,   81,  278,  284,  381,
   86,   87,   81,  851,   81,  387,  852,  856,  506,  549,
  888,   81,   78,   81,  506,  825,  510,  510,  858,   86,
  861,  590,   94,  432,  433,  434,  791,  111,   86,   87,
  824,  862,  867,   91,  863,  824,  824,  873,  804,  805,
  871,  281,  510,  284,  284,  514,   81,  872,  510,  875,
   82,   81,  285,  896,  900,  901,  907,  279,   87,  506,
  506,  911,   86,  824,   86,  912,  914,  334,  329,  111,
  919,   86,  332,  330,  526,  331,   81,  333,  824,   84,
  526,  831,  832,  833,  740,   84,  824,  517,  517,  526,
  825,  506,   94,  277,  519,  825,  825,  285,  285,  515,
  519,  510,  111,  258,   94,   94,   86,  111,  111,  517,
  521,   86,  521,   84,  526,  455,  845,  360,   90,  299,
  350,  359,  431,  825,  351,  850,  462,   84,   84,  553,
  117,  554,  555,  556,  557,  111,  903,  356,  825,  355,
  913,  827,  827,  265,  190,  455,  825,   94,   94,   94,
  111,  518,   90,  265,  285,  837,  685,  785,  111,  874,
  455,  455,  339,   94,  432,  433,   90,   90,  878,  395,
  787,  301,    0,   87,  558,  559,  381,    0,  827,  431,
  309,    0,   94,    0,  265,    0,    0,  265,    0,    0,
    0,   94,   87,   84,    0,  322,  323,    0,    0,  285,
  285,  265,  265,  904,   87,  510,    0,    0,  905,   87,
    0,   87,    0,  156,   87,    0,    0,    0,   85,    0,
  281,  432,  433,  459,   77,   94,  286,   94,    0,    0,
   83,   81,   90,   83,   94,  277,  265,    0,    0,    0,
  431,  156,    0,  156,    0,  156,  431,   86,  506,  449,
   83,  381,   85,  827,  286,    0,    0,    0,  827,  827,
  423,   87,  156,    0,  510,    0,   85,   85,  265,   94,
    0,  286,  286,  413,   94,  224,  224,    0,    0,  224,
  277,  277,  432,  433,  460,   90,  827,    0,  432,  433,
  464,  284,  302,  303,  304,  305,  306,    0,    0,  286,
  286,  827,  423,  423,  423,  253,  255,  506,    0,  827,
    0,  224,  224,   83,    0,  413,  413,  413,    0,    0,
  296,  298,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   87,   85,    0,    0,    0,   84,    0,    0,    0,
    0,  166,    0,   87,   87,  285,   76,    0,    0,   76,
    0,  455,  309,    0,    0,    0,    0,    0,    0,    0,
  200,    0,    0,    0,    0,    0,   76,  322,  323,  166,
  200,  166,    0,  166,    0,   90,    0,  451,    0,    0,
  826,  828,    0,    0,    0,    0,   87,   87,   87,    0,
  166,    0,    0,    0,    0,    0,  455,  455,    0,    0,
    0,  200,   87,    0,  200,  265,  265,  265,  455,    0,
  265,  265,  265,    0,  265,    0,    0,  843,  200,  200,
    0,   87,   86,  200,  265,  265,    0,    0,    0,   76,
   87,    0,    0,  265,  265,    0,  265,  265,  265,  265,
  265,    0,    0,    0,  151,    0,    0,  285,    0,    0,
    0,    0,  506,  200,    0,    0,   86,    0,  506,   85,
    0,    0,    0,    0,   87,    0,   87,    0,   83,    0,
   86,   86,  151,   87,  151,   85,  151,    0,    0,  224,
  452,  224,  224,  296,    0,  200,    0,    0,    0,    0,
    0,  265,  891,  151,  134,    0,    0,  898,  899,  224,
    0,  224,  224,  506,  506,    0,    0,  156,   87,  156,
  156,  156,  156,   87,   83,   83,    0,    0,    0,  286,
    0,    0,  134,    0,  134,  910,  134,  443,  277,    0,
  454,    0,    0,    0,    0,  506,   86,    0,    0,    0,
  918,    0,  449,  134,    0,    0,    0,  286,  921,    0,
    0,    0,  156,  156,    0,    0,    0,  267,    0,    0,
    0,   83,    0,    0,    0,    0,    0,  267,    0,  480,
  481,  482,  483,  484,  485,  486,  487,  488,  489,  490,
  491,  492,  493,  494,   76,  496,  497,  498,  499,  500,
  501,  502,  503,  504,  505,    0,    0,    0,  267,    0,
    0,  266,  224,    0,    0,  521,    0,    0,  525,  528,
  224,    0,    0,    0,    0,  267,  267,    0,    0,    0,
   83,    0,  200,  200,  200,    0,    0,  200,  200,  200,
    0,  200,    0,    0,    0,  166,    0,  166,  166,  166,
  166,  200,  200,    0,  455,    0,    0,    0,    0,    0,
  200,  200,    0,  200,  200,  200,  200,  200,    0,    0,
  515,  224,    0,    0,    0,  224,    0,    0,  521,  224,
  451,  525,  612,    0,    0,  615,    0,    0,    0,   86,
  166,  166,  267,    0,    0,    0,    0,    0,  629,  200,
  200,    0,  506,  200,  200,    0,  272,  276,  634,   96,
   96,  515,  200,    0,    0,  224,    0,    0,  200,  224,
    0,  224,   96,   96,    0,    0,   96,    0,    0,   83,
    0,    0,  135,   96,    0,    0,    0,    0,    0,    0,
  647,  648,    0,    0,    0,    0,    0,    0,  151,    0,
  151,  151,  151,  151,    0,    0,   96,   96,   96,    0,
  135,  506,  135,    0,  135,  649,    0,   96,  453,    0,
    0,    0,    0,   83,    0,    0,   88,    0,    0,    0,
    0,  135,    0,  452,   83,    0,    0,    0,    0,    0,
  667,    0,    0,  151,  151,  515,    0,  224,  134,    0,
  134,  134,  134,  134,    0,    0,  507,    0,    0,    0,
   88,    0,  507,   96,    0,   96,    0,    0,    0,    0,
    0,    0,    0,   83,   88,   88,    0,    0,   83,  267,
  267,  267,    0,  454,  267,  267,  267,    0,  267,    0,
    0,    0,    0,  134,  134,  224,    0,    0,    0,    0,
    0,    0,  516,  224,    0,    0,    0,  507,  507,    0,
  267,  267,  267,  267,  267,    0,    0,    0,    0,    0,
    0,   97,    0,   83,  667,  224,    0,    0,    0,  224,
    0,    0,    0,  407,  224,    0,    0,  407,    0,  507,
   88,  224,    0,  516,   83,    0,    0,  422,    0,   97,
    0,   97,    0,   97,    0,    0,    0,  444,  749,  267,
    0,    0,    0,    0,   96,   96,   96,    0,    0,    0,
   97,   96,    0,   96,   96,    0,  760,  762,    0,    0,
  764,  766,  515,  515,  515,    0,  507,    0,  515,  515,
   80,  515,  507,    0,  769,  224,   96,    0,   96,   96,
    0,   83,    0,    0,  224,  506,   79,    0,   96,   79,
    0,  510,    0,    0,    0,    0,    0,    0,   98,   96,
    0,    0,    0,    0,    0,    0,   79,  516,  224,    0,
    0,    0,    0,    0,    6,    0,    0,  507,  507,    0,
    0,  760,  764,  766,    6,  224,   98,  272,   98,  224,
   98,  224,    0,    0,  447,    0,  510,  510,   96,   96,
   96,   96,   96,   96,   96,   96,    0,   98,    0,  507,
  821,    0,    0,    0,    0,    6,  135,    0,  135,  135,
  135,  135,    0,   88,    0,  577,    0,    0,  510,   79,
  224,    0,    0,    6,    0,    0,  507,    0,    0,   96,
    0,    0,   96,    0,  407,   96,   96,   96,   96,    0,
    0,  453,    0,    0,    0,    0,   83,    0,    0,    0,
  224,  135,  135,    0,  407,    0,    0,    0,  854,   96,
   96,  224,    0,    0,    0,   83,  224,  334,  329,    0,
    0,    0,  332,  330,    0,  331,    0,  333,   82,    0,
    0,   82,   83,  285,   83,  507,    0,   83,    0,    6,
  326,    0,  325,    0,  516,  516,  516,    0,   82,  224,
  516,  516,   96,  516,    0,    0,  224,    0,  224,    0,
    0,  506,   78,    0,    0,   78,   96,  506,    0,    0,
    0,    0,    0,    0,  328,    0,    0,    0,  285,  285,
    0,    0,   78,    0,   83,    0,    0,  224,    0,  224,
    0,  653,  653,  653,    0,   97,    0,   97,   97,   97,
   97,    0,  665,    0,  327,   88,  507,  224,  665,  665,
  526,   82,  506,  506,    0,    0,  224,    0,    0,    0,
  526,    0,    0,    0,   79,  510,    0,    0,    0,  397,
  444,  399,  400,    0,  665,    0,    0,    0,    0,    0,
   97,   97,    0,    0,  506,   78,    0,    0,    0,    0,
  690,  526,    0,  690,   83,  690,    0,   96,    0,    0,
  701,  704,    0,    0,    0,  507,   83,   83,  526,  526,
    0,    0,    0,    0,    0,    0,    6,    6,    6,    0,
   96,   96,    6,    6,  510,    6,    0,    0,    0,    0,
    0,    0,   98,  407,   98,   98,   98,   98,    0,    0,
   96,    0,    0,  526,  407,    0,    0,    0,    0,   83,
   83,   83,    0,    0,    0,    0,    0,   96,    0,   96,
    0,    0,   96,    0,    0,   83,    0,  447,    0,    0,
    0,   77,    0,    0,   77,  526,  286,   98,   98,    0,
    0,    0,    0,    0,   83,    0,    0,    0,    0,    0,
    0,   77,    0,   83,    0,    0,    0,    0,   96,  530,
  534,    0,  653,    0,    0,   96,   82,    0,    0,   96,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  286,  286,    0,  790,    0,    0,   83,  794,   83,
  704,    0,  704,   96,    0,    0,   83,   96,   96,    0,
   78,  506,  309,    0,    0,    0,    8,  314,  315,  407,
    0,  599,  407,  407,   77,  606,    8,  322,  323,  610,
    0,    0,  665,  665,    0,  616,  285,    0,    0,    0,
    0,   83,    0,    0,    0,    0,   83,    0,    0,   96,
    0,    0,    0,    0,    0,    0,    0,    8,    0,    0,
    0,   96,   96,    0,    0,  639,    0,   96,    0,  610,
  506,  639,    0,    0,    0,    8,    0,    0,    0,  690,
  690,  690,  526,  526,  526,    0,    0,  526,  526,  526,
    0,  526,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  526,  526,    0,   96,   96,   96,  407,    0,  407,
  526,  526,    0,  526,  526,  526,  526,  526,   32,    0,
   96,    0,    0,  507,   80,    0,    0,   80,   32,  507,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   96,
    0,    8,   96,    0,   80,  704,    0,  673,   96,  407,
  407,    0,    0,    0,    0,  407,    0,    0,    0,   32,
    0,    0,  265,    0,    0,    0,    0,    0,  526,    0,
    0,    0,  690,  577,  507,  507,    0,   32,    0,   77,
    0,    0,   96,  407,   96,    0,    0,    0,    0,    0,
    0,   96,    0,    0,    0,  698,    0,    0,    0,  704,
    0,  407,    0,  530,  407,    0,  507,   80,    0,    0,
    0,    0,  185,  180,    0,  407,    0,  183,  181,    0,
  182,    0,  184,    0,    0,  724,   96,    0,    0,  727,
    0,   96,    0,    0,  728,  177,    0,  176,    0,  286,
    0,  606,    0,   32,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  179,
    0,  187,    0,    0,    0,    0,  444,    0,    8,    8,
    8,    0,    0,    0,    8,    8,  444,    8,    0,    0,
    0,    0,    0,    0,    0,  771,    0,    0,    0,  178,
    0,  186,    0,    0,  778,    0,    0,    0,    0,  503,
    0,    0,    0,  444,  444,    0,  503,  444,  444,  444,
  444,  444,  444,  444,    0,    0,    0,    0,  799,    0,
    0,    0,    0,    0,  444,  444,  444,  444,  444,  444,
    0,    0,    0,    0,    0,  610,    0,    0,    0,    0,
    0,  610,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   80,  507,    0,    0,    0,  444,  444,  444,
  444,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   32,   32,   32,    0,    0,    0,   32,   32,    0,   32,
  834,    0,    0,    0,    0,    0,    0,  447,    0,  503,
  444,  444,  503,    0,    0,    0,    0,  447,    0,    0,
    0,   32,   32,   32,   32,   32,    0,    0,    0,    0,
  639,    0,  507,    0,    0,    0,    0,    0,    0,    0,
  504,  855,    0,    0,  447,  447,  860,  504,  447,  447,
  447,  447,  447,  447,  447,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  447,  447,  447,  447,  447,
  447,    0,    0,    0,    0,    0,    0,    0,    0,  880,
    0,    0,    0,    0,    0,    0,  606,    0,  610,  256,
  257,    0,    0,  258,    0,    0,    0,    0,  447,  447,
  447,  447,    0,    0,  162,  163,    0,  164,  165,  166,
  167,    0,  168,  169,    0,    0,  170,  906,    0,  909,
  171,  172,  173,  174,    0,    0,    0,    0,    0,    0,
  504,  447,  447,  504,  175,    0,    0,  610,    0,    0,
    0,    0,    0,  503,  503,  503,  920,  503,  444,  444,
  444,  503,  503,  444,  444,  444,  503,  444,  503,  503,
  503,  503,  503,  503,  503,  444,  503,  444,  444,  503,
  503,  503,  503,  503,  503,  503,  444,  444,    0,  444,
  444,  444,  444,  444,    0,  503,    0,    0,  503,  503,
  503,  503,  503,  503,  503,  503,  503,  503,  503,  503,
    0,  503,  503,  503,    0,  503,  503,  503,  444,  444,
  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,
  444,    0,    0,  444,  444,  444,  503,  444,  444,  503,
  503,    0,  503,  503,  444,  503,  503,  503,  503,  503,
  503,  503,    0,    0,    0,    0,  503,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  504,  504,  504,    0,  504,  447,
  447,  447,  504,  504,  447,  447,  447,  504,  447,  504,
  504,  504,  504,  504,  504,  504,  447,  504,  447,  447,
  504,  504,  504,  504,  504,  504,  504,  447,  447,    0,
  447,  447,  447,  447,  447,    0,  504,    0,    0,  504,
  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,
  504,    0,  504,  504,  504,    0,  504,  504,  504,  447,
  447,  447,  447,  447,  447,  447,  447,  447,  447,  447,
  447,  447,  510,    0,  447,  447,  447,  504,  447,  447,
  504,  504,  510,  504,  504,  447,  504,  504,  504,  504,
  504,  504,  504,    0,    0,    0,    0,  504,    0,    0,
    0,    0,    0,    0,    0,  506,    0,    0,    0,  510,
  510,    0,  506,  510,  510,  510,  510,  510,  510,  510,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  510,  510,  510,   87,  510,  510,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  510,  510,  510,  510,    0,    0,    0,
    0,    0,  506,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  506,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  510,  510,  510,  506,    0,
    0,    0,    0,    0,    0,  506,    0,    0,    0,  506,
  506,    0,  506,  506,  506,  506,  506,  506,  506,  506,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  506,  506,  506,   86,  506,  506,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  506,  506,  506,  506,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  506,  506,  506,  506,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  506,
  506,  506,    0,  506,  510,  510,  510,  506,  506,  510,
  510,  510,  506,  510,  506,  506,  506,  506,  506,  506,
  506,    0,  510,  510,  510,  506,  506,  506,  506,  506,
  506,  506,  510,  510,    0,  510,  510,  510,  510,  510,
    0,  506,    0,    0,  506,  506,  506,  506,  506,  506,
  506,  506,  506,  506,  506,  506,    0,  506,  506,  506,
    0,  506,  506,  506,  510,  510,  510,  510,  510,  510,
  510,  510,  510,  510,  510,  510,  510,    0,    0,  510,
  510,  510,  506,    0,  510,  506,  506,    0,  506,  506,
  510,  506,  506,  506,  506,  506,  506,  506,    0,  506,
  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
  506,    0,  506,  506,  506,  506,  506,  506,  506,  506,
  506,  506,  506,  506,    0,  506,  506,  506,  506,  506,
    0,  506,    0,    0,  506,  506,  506,  506,  506,  506,
  506,  506,  506,  506,  506,  506,    0,  506,  506,  506,
    0,  506,  506,  506,  506,  506,  506,  506,  506,  506,
  506,  506,  506,  506,  506,  506,  506,  507,    0,  506,
  506,  506,  506,    0,  506,  506,  506,  507,  506,  506,
  506,  506,  506,  506,  506,  506,  506,  506,    0,    0,
    0,    0,  506,    0,    0,    0,    0,    0,    0,    0,
  507,    0,    0,    0,  507,  507,    0,  507,  507,  507,
  507,  507,  507,  507,  507,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  507,  507,  507,   88,  507,
  507,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  507,  507,
  507,  507,    0,    0,    0,    0,    0,  278,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  278,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  507,  507,  507,  507,    0,    0,    0,    0,    0,    0,
  505,    0,    0,    0,  278,  278,    0,  505,  278,  278,
  278,  278,  278,  278,  278,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  278,  278,  278,    0,  278,
  278,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  278,  278,
  278,  278,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  505,  278,  278,  505,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  507,  507,  507,    0,  507,  507,
  507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,    0,  507,  507,  507,
  507,  507,  507,  507,  507,  507,  507,  507,  507,    0,
  507,  507,  507,  507,  507,    0,  507,    0,    0,  507,
  507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
  507,    0,  507,  507,  507,    0,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
  507,  507,    0,    0,  507,  507,  507,  507,    0,  507,
  507,  507,    0,  507,  507,  507,  507,  507,  507,  507,
  507,  507,  507,    0,  505,  505,  505,  507,  505,  278,
  278,  278,  505,  505,  278,  278,  278,  505,  278,  505,
  505,  505,  505,  505,  505,  505,    0,  505,  278,  278,
  505,  505,  505,  505,  505,  505,  505,  278,  278,    0,
  278,  278,  278,  278,  278,    0,  505,    0,    0,  505,
  505,  505,  505,  505,  505,  505,  505,  505,  505,  505,
  505,    0,  505,  505,  505,    0,  505,  505,  505,  278,
  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,
  278,  278,  511,    0,  278,  278,  278,  505,    0,  278,
  505,  505,  511,  505,  505,  278,  505,  505,  505,  505,
  505,  505,  505,    0,    0,    0,    0,  505,    0,    0,
    0,    0,    0,    0,    0,  508,    0,    0,    0,  511,
  511,    0,  508,  511,  511,  511,  511,  511,  511,  511,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  511,  511,  511,    0,  511,  511,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  511,  511,  511,  511,    0,    0,    0,
    0,    0,  512,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  512,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  511,  511,  511,  508,    0,
    0,    0,    0,    0,    0,  509,    0,    0,    0,  512,
  512,    0,  509,  512,  512,  512,  512,  512,  512,  512,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  512,  512,  512,    0,  512,  512,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  512,  512,  512,  512,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  512,  512,  512,  509,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  508,
  508,  508,    0,  508,  511,  511,  511,  508,  508,  511,
  511,  511,  508,  511,  508,  508,  508,  508,  508,  508,
  508,    0,  511,  511,  511,  508,  508,  508,  508,  508,
  508,  508,  511,  511,    0,  511,  511,  511,  511,  511,
    0,  508,    0,    0,  508,  508,  508,  508,  508,  508,
  508,  508,  508,  508,  508,  508,    0,  508,  508,  508,
    0,  508,  508,  508,  511,  511,  511,  511,  511,  511,
  511,  511,  511,  511,  511,  511,  511,    0,    0,  511,
  511,  511,  508,    0,  511,  508,  508,    0,  508,  508,
  511,  508,  508,  508,  508,  508,  508,  508,    0,  509,
  509,  509,  508,  509,  512,  512,  512,  509,  509,  512,
  512,  512,  509,  512,  509,  509,  509,  509,  509,  509,
  509,    0,  512,  512,  512,  509,  509,  509,  509,  509,
  509,  509,  512,  512,    0,  512,  512,  512,  512,  512,
    0,  509,    0,    0,  509,  509,  509,  509,  509,  509,
  509,  509,  509,  509,  509,  509,    0,  509,  509,  509,
    0,  509,  509,  509,  512,  512,  512,  512,  512,  512,
  512,  512,  512,  512,  512,  512,  512,  379,    0,  512,
  512,  512,  509,    0,  512,  509,  509,  379,  509,  509,
  512,  509,  509,  509,  509,  509,  509,  509,    0,    0,
    0,    0,  509,    0,    0,    0,    0,    0,    0,    0,
  255,    0,    0,    0,  379,  379,    0,    0,  379,  379,
  379,  379,  379,  379,  379,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  379,  379,  379,    0,  379,
  379,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  379,  379,
  379,  379,    0,    0,    0,    0,    0,  526,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  526,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  379,  379,  379,  255,    0,    0,    0,    0,    0,    0,
  255,    0,    0,    0,  526,  526,    0,    0,  526,  526,
  526,  526,  526,  526,  526,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  526,  526,  526,    0,  526,
  526,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  526,  526,
  526,  526,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  526,  526,  526,  255,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  255,  255,  255,    0,  255,  379,
  379,  379,  255,  255,  379,  379,  379,  255,  379,  255,
  255,  255,  255,  255,  255,  255,    0,  379,  379,  379,
  255,  255,  255,  255,  255,  255,  255,  379,  379,    0,
  379,  379,  379,  379,  379,    0,  255,    0,    0,  255,
  255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
  255,    0,  255,  255,  255,    0,  255,  255,  255,  379,
  379,  379,  379,  379,  379,  379,  379,  379,  379,  379,
  379,  379,    0,    0,  379,  379,  379,  255,    0,  379,
  255,  255,    0,  255,  255,  379,  255,  255,  255,  255,
  255,  255,  255,    0,  255,  255,  255,  255,  255,  526,
  526,  526,  255,  255,  526,  526,  526,  255,  526,  255,
  255,  255,  255,  255,  255,  255,    0,  526,  526,  526,
  255,  255,  255,  255,  255,  255,  255,  526,  526,    0,
  526,  526,  526,  526,  526,    0,  255,    0,    0,  255,
  255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
  255,    0,  255,  255,  255,    0,  255,  255,  255,  526,
  526,  526,  526,  526,  526,  526,  526,  526,  526,  526,
  526,  526,  284,    0,  526,  526,  526,  255,    0,  526,
  255,  255,  284,  255,  255,  526,  255,  255,  255,  255,
  255,  255,  255,    0,    0,    0,    0,  255,    0,    0,
    0,    0,    0,    0,    0,  507,    0,    0,    0,  284,
  284,    0,  507,  284,  284,  284,  284,  284,  284,  284,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  284,  284,  284,   89,  284,  284,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  284,  284,  284,  284,    0,    0,    0,
    0,    0,  292,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  292,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  284,  284,  507,    0,
    0,    0,    0,    0,    0,  255,    0,    0,    0,  292,
  292,    0,    0,  292,  292,  292,  292,  292,  292,  292,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  292,  292,  292,    0,  292,  292,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  292,  292,  292,  292,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  292,  292,  255,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  507,
  507,  507,    0,  507,  284,  284,  284,  507,  507,  284,
  284,  284,  507,  284,  507,  507,  507,  507,  507,  507,
  507,    0,    0,  284,  284,  507,  507,  507,  507,  507,
  507,  507,  284,  284,    0,  284,  284,  284,  284,  284,
    0,  507,    0,    0,  507,  507,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,    0,  507,  507,  507,
    0,  507,  507,  507,  284,  284,  284,  284,  284,  284,
  284,  284,  284,  284,  284,  284,  284,    0,    0,  284,
  284,  284,  507,    0,  284,  507,  507,    0,  507,  507,
  284,  507,  507,  507,  507,  507,  507,  507,    0,  255,
  255,  255,  507,  255,  292,  292,  292,  255,  255,  292,
  292,  292,  255,  292,  255,  255,  255,  255,  255,  255,
  255,    0,    0,  292,  292,  255,  255,  255,  255,  255,
  255,  255,  292,  292,    0,  292,  292,  292,  292,  292,
    0,  255,    0,    0,  255,  255,  255,  255,  255,  255,
  255,  255,  255,  255,  255,  255,    0,  255,  255,  255,
    0,  255,  255,  255,  292,  292,  292,  292,  292,  292,
  292,  292,  292,  292,  292,  292,  292,  510,    0,  292,
  292,  292,  255,    0,  292,  255,  255,  510,  255,  255,
  292,  255,  255,  255,  255,  255,  255,  255,    0,    0,
    0,    0,  255,    0,    0,    0,    0,    0,    0,    0,
  506,    0,    0,    0,  510,  510,    0,  506,  510,  510,
  510,   79,  510,  510,  510,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  510,  510,   87,  510,
  510,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  510,  510,
    0,  510,    0,    0,    0,    0,    0,  506,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  506,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  510,  510,  510,  506,    0,    0,    0,    0,    0,    0,
  506,    0,    0,    0,  506,  506,    0,  506,  506,  506,
  506,   78,  506,  506,  506,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  506,  506,   86,  506,
  506,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  506,  506,
    0,  506,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  506,  506,  506,  506,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  506,  506,  506,    0,  506,  510,
  510,  510,  506,  506,    0,  510,  510,  506,  510,  506,
  506,  506,  506,  506,  506,  506,    0,  510,    0,    0,
  506,  506,  506,  506,  506,  506,  506,  510,  510,    0,
  510,  510,  510,  510,  510,    0,  506,    0,    0,  506,
  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
  506,    0,  506,  506,  506,    0,  506,  506,  506,  510,
  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,
  510,  510,    0,    0,  510,  510,  510,  506,    0,    0,
  506,  506,    0,  506,  506,    0,  506,  506,  506,  506,
  506,  506,  506,    0,  506,  506,  506,  506,  506,  506,
  506,  506,  506,  506,    0,  506,  506,  506,  506,  506,
  506,  506,  506,  506,  506,  506,    0,  506,    0,    0,
  506,  506,  506,  506,  506,  506,  506,  506,  506,    0,
  506,  506,  506,  506,  506,    0,  506,    0,    0,  506,
  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
  506,    0,  506,  506,  506,    0,  506,  506,  506,  506,
  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
  506,  506,  507,    0,  506,  506,  506,  506,    0,    0,
  506,  506,  507,  506,  506,    0,  506,  506,  506,  506,
  506,  506,  506,    0,    0,    0,    0,  506,    0,    0,
    0,    0,    0,    0,    0,  507,    0,    0,    0,  507,
  507,    0,  507,  507,  507,  507,   80,  507,  507,  507,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  507,  507,   88,  507,  507,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  507,  507,    0,  507,    0,    0,    0,
    0,    0,  284,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  284,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  507,  507,  507,  507,    0,
    0,    0,    0,    0,    0,  507,    0,    0,    0,  284,
  284,    0,  507,  284,  284,  284,   81,  284,  284,  284,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  284,  284,   89,  284,  284,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  284,  284,    0,  284,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  284,  284,  507,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  823,  507,
  507,  507,    0,  507,  507,  507,  507,  507,  507,    0,
  507,  507,  507,  507,  507,  507,  507,  507,  507,  507,
  507,    0,  507,    0,    0,  507,  507,  507,  507,  507,
  507,  507,  507,  507,    0,  507,  507,  507,  507,  507,
    0,  507,    0,    0,  507,  507,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,    0,  507,  507,  507,
    0,  507,  507,  507,  507,  507,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,  507,    0,    0,  507,
  507,  507,  507,    0,    0,  507,  507,    0,  507,  507,
    0,  507,  507,  507,  507,  507,  507,  507,    0,  507,
  507,  507,  507,  507,  284,  284,  284,  507,  507,    0,
  284,  284,  507,  284,  507,  507,  507,  507,  507,  507,
  507,    0,    0,    0,    0,  507,  507,  507,  507,  507,
  507,  507,  284,  284,    0,  284,  284,  284,  284,  284,
    0,  507,    0,    0,  507,  507,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,    0,  507,  507,  507,
    0,  507,  507,  507,  284,  284,  284,  284,  284,  284,
  284,  284,  284,  284,  284,  284,  284,  526,    0,  284,
  284,  284,  507,    0,    0,  507,  507,  526,  507,  507,
    0,  507,  507,  507,  507,  507,  507,  507,    0,    0,
    0,    0,  507,    0,    0,    0,    0,    0,    0,    0,
  255,    4,    5,    6,    0,    8,    0,    0,  526,    9,
   10,    0,    0,  526,   11,    0,   12,   13,   14,   98,
   99,   17,   18,    0,    0,  526,  526,  100,  101,  102,
   22,   23,   24,   25,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  103,    0,    0,   31,   32,   33,   34,
   35,   36,   37,   38,   39,    0,   40,   41,    0,   42,
   43,   44,    0,    0,    0,   47,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   92,    0,  822,    0,    0,  108,   50,    0,
   51,   52,  526,  255,    0,   54,   55,   56,   57,   58,
    0,    0,    0,    0,  109,   92,    0,    0,    0,    0,
    0,    0,  503,    0,    0,    0,    0,    0,  444,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   92,   92,    0,    7,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    7,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  444,  444,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    7,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    7,    0,  503,    0,    0,   92,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  255,  255,  255,    0,  255,  526,
  526,  526,  255,  255,  526,  526,  526,  255,  526,  255,
  255,  255,  255,  255,  255,  255,    0,    0,  526,    0,
  255,  255,  255,  255,  255,  255,  255,  526,  526,    7,
  526,  526,  526,  526,  526,    0,  255,    0,    0,  255,
  255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
  255,    0,  255,  255,  255,    0,  255,  255,  255,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  526,  255,    0,    0,
  255,  255,    0,  255,  255,    0,  255,  255,  255,  255,
  255,  255,  255,    0,    0,    0,    0,  255,   92,   92,
   92,   92,   92,   92,   92,   92,   92,   92,   92,    0,
    0,   92,   92,    0,   92,   92,   92,   92,   92,   92,
   92,    0,  503,    0,    0,   92,   92,   92,   92,   92,
   92,   92,    0,    0,   92,    0,    0,    0,    0,    0,
   92,   92,   92,   92,   92,   92,   92,   92,   92,   92,
   92,   92,   92,    0,   92,   92,    0,   92,   92,   92,
    0,   92,   92,   92,    0,    0,    7,    7,    7,   93,
    0,    0,    7,    7,    0,    7,    0,    0,    0,    0,
    0,  444,   92,    0,    0,   92,   92,    0,   92,   92,
    0,   92,   93,   92,   92,   92,   92,   92,    0,  504,
    0,    0,   92,    0,    0,  447,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   93,   93,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  447,  447,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   92,    0,    0,    0,    0,    0,
    0,    0,  504,    0,    0,   93,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   92,    0,    0,
    0,    0,    0,    0,  506,    0,    0,    0,    0,    0,
  510,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   92,   92,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  248,  334,  329,    0,    0,    0,  332,  330,
    0,  331,    0,  333,    0,  510,  510,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  326,    0,  325,  334,
  329,    0,    0,    0,  332,  330,    0,  331,    0,  333,
    0,    0,    0,    0,    0,    0,    0,  510,    0,    0,
   92,    0,  326,    0,  325,  324,    0,    0,    0,    0,
  328,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   93,   93,   93,   93,   93,
   93,   93,   93,   93,   93,   93,  328,    0,   93,   93,
  327,   93,   93,   93,   93,   93,   93,   93,    0,  504,
    0,    0,   93,   93,   93,   93,   93,   93,   93,    0,
    0,   93,    0,    0,    0,    0,  327,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
    0,   93,   93,    0,   93,   93,   93,    0,   93,   93,
   93,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  447,   93,
    0,    0,   93,   93,    0,   93,   93,    0,   93,    0,
   93,   93,   93,   93,   93,    0,    0,    0,    0,   93,
   92,   92,   92,   92,   92,   92,   92,   92,   92,   92,
   92,    0,    0,   92,   92,    0,   92,   92,   92,   92,
   92,   92,   92,    0,  510,    0,    0,   92,   92,   92,
   92,   92,   92,   92,    0,    0,   92,    0,    0,    0,
    0,    0,   92,   92,   92,   92,   92,   92,   92,   92,
   92,   92,   92,   92,   92,    0,   92,   92,    0,   92,
   92,   92,    0,   92,   92,   92,    0,    0,    0,    0,
    0,   93,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  510,   92,    0,    0,   92,   92,    0,
   92,   92,    0,   92,   93,   92,   92,   92,   92,   92,
    0,  507,    0,    0,   92,    0,    0,  284,  309,  310,
  311,  312,  313,  314,  315,  316,  317,  318,  319,    0,
   93,   93,    0,  322,  323,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  309,  310,  311,  312,  313,  314,
  315,  316,  317,  318,  319,  320,  321,    0,    0,  322,
  323,    0,  284,  284,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   93,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   93,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   93,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  285,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   93,   93,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  285,  285,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  335,    0,    0,    0,    0,    0,    0,
    0,    0,   93,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  335,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,   93,    0,    0,
   93,   93,  335,   93,   93,   93,   93,   93,   93,   93,
    0,    0,    0,    0,   93,   93,   93,   93,   93,   93,
   93,    0,    0,   93,    0,    0,    0,    0,    0,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,    0,   93,   93,    0,   93,   93,   93,    0,
   93,   93,   93,    0,    0,    0,    0,    0,  526,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  335,
  284,   93,    0,    0,   93,   93,    0,   93,   93,    0,
   93,  526,   93,   93,   93,   93,   93,    0,    0,    0,
    0,   93,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,   93,    0,    0,   93,   93,  526,   93,   93,
   93,   93,   93,   93,   93,    0,    0,    0,    0,   93,
   93,   93,   93,   93,   93,   93,    0,    0,   93,    0,
    0,    0,    0,    0,   93,   93,   93,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,    0,   93,   93,
    0,   93,   93,   93,    0,   93,   93,   93,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  526,  285,   93,    0,    0,   93,
   93,    0,   93,   93,    0,   93,    0,   93,   93,   93,
   93,   93,    0,    0,    0,    0,   93,    0,    0,  335,
  335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
    0,  335,  335,  335,  335,  335,  335,  335,  335,  335,
  335,  335,    0,    0,    0,    0,  335,  335,  335,  335,
  335,  335,  335,    0,    0,  335,    0,    0,    0,    0,
    0,  335,  335,  335,  335,  335,  335,  335,  335,  335,
  335,  335,  335,  335,    0,  335,  335,    0,  335,  335,
  335,  526,  335,  335,  335,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  335,  526,    0,  335,  335,    0,  335,
  335,    0,  335,    0,  335,  335,  335,  335,  335,    0,
    0,    0,    0,  335,  526,  526,  526,  526,  526,  526,
  526,    0,  526,  526,  526,    0,    0,    0,  526,    0,
  526,  526,  526,  526,  526,  526,  526,    0,    0,    0,
    0,  526,  526,  526,  526,  526,  526,  526,    0,    0,
  526,    0,    0,    0,    0,    0,  526,  526,  526,  526,
  526,  526,  526,  526,  526,  526,  526,  526,  526,    0,
  526,  526,    0,  526,  526,  526,    0,  526,  526,  526,
    0,    0,  379,    0,    0,    0,  526,  526,    0,    0,
    0,    0,  379,    0,    0,    0,    0,    0,  526,    0,
    0,  526,  526,    0,  526,  526,    0,  526,    0,  526,
  526,  526,  526,  526,    0,    0,    0,    0,  526,  379,
  379,    0,    0,  379,  379,  379,  379,  379,  379,  379,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  379,  379,  379,    0,  379,  379,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  379,  379,  379,  379,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   41,
    0,    0,    0,    0,    0,  379,  379,  379,    0,   41,
    0,    0,    0,    0,    0,    0,    0,  526,  526,  526,
  526,  526,  526,    0,    0,    0,  526,  526,    0,    0,
    0,  526,    0,  526,  526,  526,  526,  526,  526,  526,
   41,    0,    0,    0,  526,  526,  526,  526,  526,  526,
  526,    0,    0,  526,    0,    0,    0,   41,   41,  526,
  526,  526,  526,  526,  526,  526,  526,  526,  526,  526,
  526,  526,    0,  526,  526,    0,  526,  526,  526,    0,
  526,  526,  526,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   34,    0,    0,    0,    0,    0,    0,
    0,  526,    0,   34,  526,  526,    0,  526,  526,    0,
  526,    0,  526,  526,  526,  526,  526,    0,    0,    0,
    0,  526,    0,    0,   41,    0,    0,    0,    0,   14,
    0,    0,    0,    0,   34,    0,    0,    0,    0,   14,
    0,    0,    0,    0,  379,  379,  379,    0,    0,  379,
  379,  379,   34,  379,    0,    0,    0,    0,    0,    0,
    0,    0,  379,  379,  379,    0,    0,    0,    0,    0,
   14,    0,  379,  379,    0,  379,  379,  379,  379,  379,
    0,    0,    0,    0,    0,    0,    0,    0,   14,    0,
  455,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  455,    0,    0,    0,  379,  379,  379,  379,  379,  379,
  379,  379,  379,  379,  379,  379,  379,    0,   34,  379,
  379,  379,    0,    0,  379,    0,    0,  455,  455,    0,
  379,  455,  455,  455,  455,  455,  455,  455,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  455,  455,
  455,   84,  455,  455,   14,    0,    0,    0,    0,    0,
    0,   41,   41,   41,    0,    0,   41,   41,   41,    0,
   41,    0,    0,    0,    0,    0,    0,    0,    0,  526,
   41,  455,  455,  455,  455,    0,    0,    0,    0,  526,
    0,    0,   41,   41,   41,   41,   41,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  455,  455,  526,  526,    0,    0,
  526,  526,  526,  526,  526,  526,  526,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  526,  526,  526,
    0,  526,  526,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   34,   34,   34,    0,    0,
    0,   34,   34,    0,   34,    0,    0,    0,    0,    0,
  526,  526,  526,  526,  277,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  277,    0,   34,   34,   34,   34,
   34,   14,   14,   14,    0,    0,    0,   14,   14,    0,
   14,    0,  526,  526,  526,    0,    0,    0,    0,    0,
    0,  277,  277,    0,    0,  277,  277,  277,  277,  277,
  277,  277,   14,   14,   14,   14,   14,    0,    0,    0,
    0,    0,  277,  277,  277,   91,  277,  277,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  455,  455,  455,    0,    0,  455,  455,  455,
    0,  455,    0,    0,    0,  277,  277,  277,  277,  334,
  329,  455,  455,    0,  332,  330,    0,  331,    0,  333,
  455,  455,    0,  455,  455,  455,  455,  455,    0,    0,
  750,    0,  326,    0,  325,  324,    0,    0,  277,  277,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  455,  455,  455,  455,  455,  455,  455,  455,
  455,  455,  455,  455,  455,    0,  328,  455,  455,  455,
    0,  456,  455,    0,    0,    0,    0,    0,  455,    0,
    0,  526,  526,  526,    0,    0,  526,  526,  526,    0,
  526,    0,    0,    0,    0,    0,  327,    0,    0,  526,
  526,  526,    0,    0,    0,    0,    0,    0,    0,  526,
  526,    0,  526,  526,  526,  526,  526,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  185,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  185,  526,  526,  526,  526,  526,  526,  526,  526,  526,
  526,  526,  526,  526,    0,    0,  526,  526,  526,    0,
    0,  526,    0,    0,    0,    0,    0,  526,    0,    0,
    0,  185,    0,    0,  185,    0,  277,  277,  277,    0,
    0,  277,  277,  277,    0,  277,    0,    0,  185,  185,
    0,    0,    0,  185,    0,  277,  277,    0,    0,    0,
    0,    0,    0,    0,  277,  277,    0,  277,  277,  277,
  277,  277,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  285,  185,    0,    0,    0,    0,    0,    0,
    0,    0,  285,    0,    0,    0,  277,  277,  277,  277,
  277,  277,  277,  277,  277,  277,  277,  277,  277,    0,
    0,  277,  277,  277,    0,  185,  277,    0,    0,  285,
  285,    0,  277,  285,  285,  285,  285,  285,  285,  285,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  285,  285,  285,   90,  285,  285,    0,    0,    0,    0,
    0,    0,    0,    0,  309,  310,  311,  312,  313,  314,
  315,  316,  317,  318,  319,  320,  321,    0,    0,  322,
  323,  286,    0,  285,  285,  285,  285,    0,    0,    0,
    0,  286,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  285,  285,  286,  286,
    0,    0,  286,  286,  286,  286,  286,  286,  286,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  286,
  286,  286,   85,  286,  286,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  185,  185,  185,    0,    0,  185,  185,  185,
    0,  185,  286,  286,  286,  286,  404,    0,    0,    0,
    0,  185,  185,    0,    0,    0,  404,    0,    0,    0,
  185,  185,    0,  185,  185,  185,  185,  185,    0,    0,
    0,    0,    0,    0,    0,  286,  286,    0,    0,    0,
    0,    0,    0,  404,  404,    0,    0,  404,  404,  404,
  404,  404,  404,  404,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  404,  404,  404,    0,  404,  404,
    0,    0,  185,    0,    0,    0,    0,    0,  185,    0,
    0,    0,    0,    0,  285,  285,  285,    0,    0,  285,
  285,  285,    0,  285,    0,    0,    0,  404,  404,  404,
  404,  334,  329,  285,  285,    0,  332,  330,  597,  331,
    0,  333,  285,  285,    0,  285,  285,  285,  285,  285,
    0,    0,    0,    0,  326,    0,  325,  324,    0,    0,
  404,  404,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  285,  285,  285,  285,  285,  285,
  285,  285,  285,  285,  285,  285,  285,    0,  328,  285,
  285,  285,    0,    0,  285,    0,    0,    0,    0,    0,
  285,    0,    0,  286,  286,  286,    0,    0,  286,  286,
  286,    0,  286,    0,    0,    0,    0,    0,  327,    0,
    0,    0,  286,  286,    0,    0,    0,    0,    0,    0,
    0,  286,  286,    0,  286,  286,  286,  286,  286,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  186,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  186,  286,  286,  286,  286,  286,  286,  286,
  286,  286,  286,  286,  286,  286,    0,    0,  286,  286,
  286,    0,    0,  286,    0,    0,    0,    0,    0,  286,
    0,    0,    0,  186,    0,    0,  186,    0,  404,  404,
  404,    0,    0,  404,  404,  404,    0,  404,    0,    0,
  186,  186,    0,    0,    0,  186,    0,  404,  404,    0,
    0,    0,    0,    0,    0,    0,  404,  404,    0,  404,
  404,  404,  404,  404,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  220,  186,    0,    0,    0,    0,
    0,    0,    0,    0,  220,    0,    0,    0,  404,  404,
  404,  404,  404,  404,  404,  404,  404,  404,  404,  404,
  404,    0,    0,  404,  404,  404,    0,  186,  404,    0,
    0,  220,  220,    0,  404,  220,  220,  220,  220,  220,
  334,  220,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  220,  220,  220,    0,  220,  220,    0,    0,
    0,    0,    0,    0,    0,    0,  309,  310,  311,  312,
  313,  314,  315,  316,  317,  318,  319,  320,  321,    0,
    0,  322,  323,  296,    0,  334,  334,  220,  220,    0,
    0,    0,    0,  296,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  220,  220,
  296,  296,    0,    0,  296,  296,  296,  296,  296,  296,
  296,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  296,  296,  296,    0,  296,  296,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  186,  186,  186,    0,    0,  186,
  186,  186,    0,  186,  296,  296,  296,  296,  292,    0,
    0,    0,    0,  186,  186,    0,    0,    0,  292,    0,
    0,    0,  186,  186,    0,  186,  186,  186,  186,  186,
    0,    0,    0,    0,    0,    0,    0,  296,  296,    0,
    0,    0,    0,    0,    0,  292,  292,    0,    0,  292,
  292,  292,  292,  292,  292,  292,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  292,  292,  292,    0,
  292,  292,    0,    0,  186,    0,    0,    0,    0,    0,
  186,    0,    0,    0,    0,    0,  220,  220,  220,    0,
    0,  220,  220,  220,    0,  220,    0,    0,    0,  292,
  292,  292,  292,  334,  329,  220,  220,    0,  332,  330,
    0,  331,    0,  333,  220,  220,    0,  220,  220,  220,
  220,  220,    0,    0,    0,    0,  326,    0,  325,  324,
    0,    0,  292,  292,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  220,  220,  220,  220,
  220,  220,  220,  220,  220,  220,  220,  220,  220,    0,
  328,  220,  220,  334,    0,    0,  220,    0,    0,    0,
    0,    0,  220,    0,    0,  296,  296,  296,    0,    0,
  296,  296,  296,    0,  296,    0,    0,    0,    0,    0,
  327,  334,  329,    0,  296,  296,  332,  330,    0,  331,
    0,  333,    0,  296,  296,    0,  296,  296,  296,  296,
  296,    0,    0,    0,  326,    0,  325,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  296,  296,  296,  296,  296,
  296,  296,  296,  296,  296,  296,  296,  296,  328,    0,
  296,  296,  296,    0,    0,  296,  175,    0,    0,    0,
    0,  296,    0,    0,    0,    0,  175,    0,    0,    0,
  292,  292,  292,    0,    0,  292,  292,  292,  327,  292,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  292,
  292,    0,    0,    0,    0,    0,    0,  175,  292,  292,
  221,  292,  292,  292,  292,  292,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  175,  442,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  442,    0,    0,    0,
  292,  292,  292,  292,  292,  292,  292,  292,  292,  292,
  292,  292,  292,    0,    0,  292,  292,  292,    0,    0,
  292,    0,    0,  442,  442,    0,  292,  442,  442,  442,
  442,  442,  442,  442,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  442,  442,  442,    0,  442,  442,
    0,  175,    0,    0,    0,    0,    0,    0,  309,  310,
  311,  312,  313,  314,  315,  316,  317,  318,  319,  320,
  321,    0,    0,  322,  323,  443,    0,  442,  442,  442,
  442,    0,    0,    0,    0,  443,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  442,  442,  443,  443,    0,    0,  443,  443,  443,  443,
  443,  443,  443,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  443,  443,  443,    0,  443,  443,    0,
    0,    0,    0,    0,    0,    0,  309,  310,  311,  312,
  313,  314,  315,  316,    0,  318,  319,    0,    0,    0,
    0,  322,  323,    0,    0,  196,  443,  443,  443,  443,
    0,    0,    0,    0,    0,  196,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  175,  175,
  175,    0,    0,    0,  175,  175,    0,  175,    0,  443,
  443,    0,  196,  196,    0,    0,  196,  196,  196,  196,
  196,    0,  196,    0,    0,    0,  175,  175,    0,  175,
  175,  175,  175,  196,  196,  196,    0,  196,  196,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  442,  442,
  442,    0,    0,  442,  442,  442,    0,  442,  196,  196,
    0,    0,    0,    0,    0,    0,    0,  442,  442,    0,
    0,    0,    0,    0,    0,    0,  442,  442,    0,  442,
  442,  442,  442,  442,    0,    0,    0,    0,    0,  196,
  196,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  442,
  442,  442,  442,  442,  442,  442,  442,  442,  442,  442,
  442,    0,    0,  442,  442,  442,    0,    0,  442,    0,
    0,    0,    0,    0,  442,    0,    0,  443,  443,  443,
    0,    0,  443,  443,  443,    0,  443,    0,    0,    0,
    0,    0,    0,  334,  329,    0,  443,  443,  332,  330,
    0,  331,    0,  333,    0,  443,  443,    0,  443,  443,
  443,  443,  443,    0,    0,    0,  326,    0,  325,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  443,  443,
  443,  443,  443,  443,  443,  443,  443,  443,  443,  443,
  328,    0,  443,  443,  443,    0,    0,  443,    0,    0,
    0,    0,    0,  443,    0,    0,    0,  196,  196,  196,
    0,    0,  196,  196,  196,    0,  196,    0,    0,    0,
  327,    0,    0,    0,    0,    0,  196,  196,    0,    0,
    0,    0,    0,    0,    0,  196,  196,    0,  196,  196,
  196,  196,  196,    0,    0,    0,    0,  192,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  192,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  196,  196,
  196,  196,  196,  196,  196,  196,  196,  196,  196,  196,
    0,    0,  196,  196,  192,  192,    0,  196,  192,  192,
  192,  192,  192,  196,  192,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  192,  192,  192,    0,  192,
  192,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  189,    0,    0,    0,
  192,  192,    0,    0,    0,    0,  189,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  192,  192,  189,  189,    0,    0,  189,  189,  189,
  189,  189,    0,  189,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  189,  189,  189,    0,  189,  189,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  309,  310,
  311,  312,  313,  314,  315,    0,  190,  318,  319,  189,
  189,    0,    0,  322,  323,    0,  190,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  189,  189,    0,  190,  190,    0,    0,  190,  190,  190,
  190,  190,    0,  190,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  190,  190,  190,    0,  190,  190,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  192,
  192,  192,    0,    0,  192,  192,  192,    0,  192,  190,
  190,    0,    0,    0,    0,    0,    0,    0,  192,  192,
    0,    0,    0,    0,    0,    0,    0,  192,  192,    0,
  192,  192,  192,  192,  192,    0,    0,    0,    0,    0,
  190,  190,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  192,  192,  192,  192,  192,  192,  192,  192,  192,  192,
  192,  192,    0,    0,  192,  192,    0,    0,    0,  192,
    0,    0,    0,    0,    0,  192,    0,    0,  189,  189,
  189,    0,    0,  189,  189,  189,    0,  189,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  189,  189,    0,
    0,    0,    0,    0,    0,    0,  189,  189,    0,  189,
  189,  189,  189,  189,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  189,
  189,  189,  189,  189,  189,  189,  189,  189,  189,  189,
  189,    0,    0,  189,  189,    0,    0,    0,  189,    0,
    0,    0,    0,    0,  189,    0,    0,    0,  190,  190,
  190,    0,    0,  190,  190,  190,    0,  190,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  190,  190,    0,
    0,    0,    0,    0,    0,    0,  190,  190,    0,  190,
  190,  190,  190,  190,    0,    0,    0,    0,  191,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  191,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  190,
  190,  190,  190,  190,  190,  190,  190,  190,  190,  190,
  190,    0,    0,  190,  190,  191,  191,    0,  190,  191,
  191,  191,  191,  191,  190,  191,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  191,  191,  191,    0,
  191,  191,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  193,    0,    0,
    0,  191,  191,    0,    0,    0,    0,  193,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  191,  191,  193,  193,    0,    0,  193,  193,
  193,  193,  193,    0,  193,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  193,  193,  193,    0,  193,
  193,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  194,    0,    0,
  193,  193,    0,    0,    0,    0,    0,  194,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  193,  193,    0,  194,  194,    0,    0,  194,  194,
  194,  194,  194,    0,  194,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  194,  194,  194,    0,  194,
  194,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  191,  191,  191,    0,    0,  191,  191,  191,    0,  191,
  194,  194,    0,    0,    0,    0,    0,    0,    0,  191,
  191,    0,    0,    0,    0,    0,    0,    0,  191,  191,
    0,  191,  191,  191,  191,  191,    0,    0,    0,    0,
    0,  194,  194,  212,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  212,    0,    0,    0,    0,    0,    0,
    0,  191,  191,  191,  191,  191,  191,  191,  191,  191,
  191,  191,  191,    0,    0,  191,  191,    0,    0,    0,
  191,  212,    0,    0,  212,    0,  191,  212,    0,  193,
  193,  193,    0,    0,  193,  193,  193,    0,  193,    0,
    0,  212,  212,  212,    0,  212,  212,    0,  193,  193,
    0,    0,    0,    0,    0,    0,    0,  193,  193,    0,
  193,  193,  193,  193,  193,    0,    0,    0,    0,    0,
    0,    0,    0,  187,    0,    0,  212,  212,    0,    0,
    0,    0,    0,  187,    0,    0,    0,    0,    0,    0,
  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,
  193,  193,    0,    0,  193,  193,    0,  212,  212,  193,
    0,  187,    0,    0,  187,  193,  187,  187,  187,  194,
  194,  194,    0,    0,  194,  194,  194,    0,  194,    0,
    0,  187,  187,  187,    0,  187,  187,    0,  194,  194,
    0,    0,    0,    0,    0,    0,    0,  194,  194,    0,
  194,  194,  194,  194,  194,    0,    0,    0,    0,    0,
    0,    0,    0,  188,    0,    0,  187,  187,    0,    0,
    0,    0,    0,  188,    0,    0,    0,    0,    0,    0,
  194,  194,  194,  194,  194,  194,  194,  194,  194,  194,
  194,  194,    0,    0,  194,  194,    0,  187,  187,  194,
    0,  188,    0,    0,  188,  194,  188,  188,  188,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  188,  188,  188,    0,  188,  188,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  212,  212,  212,    0,    0,
  212,  212,  212,    0,  212,    0,  188,  188,    0,    0,
    0,    0,    0,    0,  212,  212,    0,    0,    0,    0,
    0,    0,    0,  212,  212,    0,  212,  212,  212,  212,
  212,    0,    0,    0,    0,    0,    0,  188,  188,  213,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  213,
    0,    0,    0,    0,    0,    0,  212,  212,  212,  212,
  212,  212,  212,  212,  212,  212,  212,  212,    0,    0,
  212,  212,    0,    0,    0,  212,    0,  213,    0,    0,
  213,  212,    0,  213,    0,  187,  187,  187,    0,    0,
  187,  187,  187,    0,  187,    0,    0,  213,  213,  213,
    0,  213,  213,    0,  187,  187,    0,    0,    0,    0,
    0,    0,    0,  187,  187,    0,  187,  187,  187,  187,
  187,    0,    0,    0,    0,    0,    0,    0,  199,    0,
    0,    0,  213,  213,    0,    0,    0,    0,  199,    0,
    0,    0,    0,    0,    0,    0,  187,  187,  187,  187,
  187,  187,  187,  187,  187,  187,  187,  187,    0,    0,
  187,  187,    0,  213,  213,  187,  199,    0,    0,  199,
    0,  187,  199,    0,    0,  188,  188,  188,    0,    0,
  188,  188,  188,    0,  188,    0,  199,  199,  199,    0,
  199,  199,    0,    0,  188,  188,    0,    0,    0,    0,
    0,    0,    0,  188,  188,    0,  188,  188,  188,  188,
  188,    0,    0,    0,    0,    0,    0,    0,  197,    0,
    0,  199,  199,    0,    0,    0,    0,    0,  197,    0,
    0,    0,    0,    0,    0,    0,  188,  188,  188,  188,
  188,  188,  188,  188,  188,  188,  188,  188,    0,    0,
  188,  188,  199,  199,    0,  188,    0,    0,    0,  197,
    0,  188,  197,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  197,  197,  197,    0,
  197,  197,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  213,  213,  213,    0,    0,  213,  213,  213,    0,
  213,  197,  197,    0,    0,    0,    0,    0,    0,    0,
  213,  213,    0,    0,    0,    0,    0,    0,    0,  213,
  213,    0,  213,  213,  213,  213,  213,    0,    0,    0,
    0,    0,  197,  197,  198,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  198,    0,    0,    0,    0,    0,
    0,    0,  213,  213,  213,  213,  213,  213,  213,  213,
  213,  213,  213,  213,    0,    0,  213,  213,    0,    0,
    0,  213,    0,    0,    0,  198,    0,  213,  198,    0,
  199,  199,  199,    0,    0,  199,  199,  199,    0,  199,
    0,    0,  198,  198,  198,    0,  198,  198,    0,  199,
  199,    0,    0,    0,    0,    0,    0,    0,  199,  199,
    0,  199,  199,  199,  199,  199,    0,    0,    0,    0,
    0,    0,    0,  202,    0,    0,    0,  198,  198,    0,
    0,    0,    0,  202,    0,    0,    0,    0,    0,    0,
    0,  199,  199,  199,  199,  199,  199,  199,  199,  199,
  199,  199,  199,    0,    0,    0,    0,    0,  198,  198,
  199,    0,    0,    0,  202,    0,  199,  202,    0,    0,
  197,  197,  197,    0,    0,  197,  197,  197,    0,  197,
    0,  202,  202,  202,    0,  202,  202,    0,    0,  197,
  197,    0,    0,    0,    0,    0,    0,    0,  197,  197,
    0,  197,  197,  197,  197,  197,    0,    0,    0,    0,
    0,  204,    0,    0,    0,    0,  202,    0,    0,    0,
    0,  204,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  197,  197,  197,  197,  197,  197,  197,  197,  197,
  197,  197,  197,    0,    0,    0,    0,    0,  202,    0,
  197,    0,  204,    0,    0,  204,  197,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  204,
  204,  204,    0,  204,  204,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  198,  198,  198,    0,
    0,  198,  198,  198,  204,  198,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  198,  198,    0,    0,    0,
    0,    0,    0,    0,  198,  198,    0,  198,  198,  198,
  198,  198,    0,    0,    0,    0,  204,  201,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  201,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  198,  198,  198,
  198,  198,  198,  198,  198,  198,  198,  198,  198,    0,
    0,    0,    0,    0,    0,    0,  198,    0,  201,    0,
    0,  201,  198,    0,    0,  202,  202,  202,    0,    0,
  202,  202,  202,    0,  202,  201,  201,  201,    0,  201,
  201,    0,    0,    0,  202,  202,    0,    0,    0,    0,
    0,    0,    0,  202,  202,    0,  202,  202,  202,  202,
  202,    0,    0,    0,    0,  203,    0,    0,    0,    0,
  201,    0,    0,    0,    0,  203,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  202,  202,  202,  202,
  202,  202,  202,  202,  202,  202,  202,  202,    0,    0,
    0,    0,  201,    0,    0,  202,  203,    0,    0,  203,
    0,  202,    0,  204,  204,  204,    0,    0,  204,  204,
  204,    0,  204,  203,  203,  203,    0,  203,  203,    0,
    0,    0,  204,  204,    0,    0,    0,    0,    0,    0,
    0,  204,  204,    0,  204,  204,  204,  204,  204,    0,
    0,    0,    0,    0,  205,    0,    0,    0,  203,    0,
    0,    0,    0,    0,  205,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  204,  204,  204,  204,  204,  204,
  204,  204,  204,  204,  204,  204,    0,    0,    0,    0,
  203,    0,    0,  204,    0,  205,    0,    0,  205,  204,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  205,  205,    0,    0,    0,  205,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  201,
  201,  201,    0,    0,  201,  201,  201,  205,  201,    0,
    0,    0,    0,    0,    0,  206,    0,    0,  201,  201,
    0,    0,    0,    0,    0,  206,    0,  201,  201,    0,
  201,  201,  201,  201,  201,    0,    0,    0,    0,  205,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  206,    0,    0,  206,
  201,  201,  201,  201,  201,  201,  201,  201,  201,  201,
  201,  201,    0,  206,  206,    0,    0,    0,  206,  201,
    0,    0,    0,    0,    0,  201,    0,  203,  203,  203,
    0,    0,  203,  203,  203,    0,  203,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  203,  203,  206,    0,
    0,    0,    0,    0,    0,  203,  203,    0,  203,  203,
  203,  203,  203,    0,    0,    0,    0,    0,  207,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  207,    0,
  206,    0,    0,    0,    0,    0,    0,    0,  203,  203,
  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
    0,    0,    0,    0,    0,    0,    0,  203,    0,  207,
    0,    0,  207,  203,    0,    0,  205,  205,  205,    0,
    0,  205,  205,  205,    0,  205,  207,  207,    0,    0,
    0,  207,    0,    0,    0,  205,  205,    0,    0,    0,
    0,    0,    0,    0,  205,  205,    0,  205,  205,  205,
  205,  205,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  207,  214,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  214,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  205,  205,    0,    0,  205,  205,    0,
    0,    0,    0,  207,    0,    0,  205,    0,    0,    0,
    0,    0,  205,  214,    0,    0,  214,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  206,  206,  206,
  214,  214,  206,  206,  206,  214,  206,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  206,  206,    0,    0,
    0,    0,    0,    0,    0,  206,  206,    0,  206,  206,
  206,  206,  206,    0,  208,  214,    0,    0,    0,    0,
    0,    0,    0,    0,  208,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  206,  206,    0,  214,  206,  206,
    0,    0,    0,    0,    0,  208,    0,  206,  208,    0,
    0,    0,    0,  206,    0,    0,    0,    0,    0,    0,
  209,    0,  208,  208,    0,    0,    0,  208,    0,    0,
  209,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  207,  207,  207,    0,    0,  207,  207,  207,    0,  207,
    0,    0,    0,    0,    0,    0,    0,  208,    0,  207,
  207,  209,    0,    0,  209,    0,    0,    0,  207,  207,
    0,  207,  207,  207,  207,  207,    0,    0,  209,  209,
    0,    0,    0,  209,    0,    0,    0,    0,    0,  208,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  207,  207,    0,
    0,  207,  207,  209,  215,    0,    0,    0,    0,    0,
  207,    0,    0,    0,  215,    0,  207,    0,    0,    0,
    0,    0,    0,    0,  214,  214,  214,    0,    0,  214,
  214,  214,    0,  214,    0,  209,    0,    0,    0,    0,
    0,    0,    0,  214,  214,  215,    0,    0,  215,    0,
    0,    0,  214,  214,    0,  214,  214,  214,  214,  214,
    0,    0,  215,  215,    0,    0,    0,  215,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  214,  214,    0,    0,  214,  214,  215,    0,    0,
    0,    0,  221,    0,  214,    0,    0,    0,    0,    0,
  214,    0,  221,    0,    0,    0,  208,  208,  208,    0,
    0,  208,  208,  208,    0,  208,    0,    0,    0,  215,
    0,    0,    0,    0,    0,  208,  208,    0,    0,    0,
    0,    0,    0,  221,  208,  208,  221,  208,  208,  208,
  208,  208,    0,    0,    0,    0,    0,    0,    0,    0,
  221,  221,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  209,  209,  209,    0,    0,  209,  209,  209,
    0,  209,    0,  208,  208,    0,    0,  208,  208,  177,
    0,  209,  209,    0,    0,  221,  208,    0,    0,  177,
  209,  209,  208,  209,  209,  209,  209,  209,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  221,    0,    0,
  177,    0,    0,  177,    0,    0,    0,    0,    0,  209,
  209,    0,    0,  209,  209,    0,    0,  177,  177,    0,
    0,    0,  209,    0,    0,    0,    0,    0,  209,    0,
    0,    0,    0,    0,    0,    0,  215,  215,  215,    0,
    0,  215,  215,  215,    0,  215,    0,    0,    0,    0,
    0,  184,  177,    0,    0,  215,  215,    0,    0,    0,
    0,  184,    0,    0,  215,  215,    0,  215,  215,  215,
  215,  215,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  177,    0,    0,  217,    0,    0,
    0,    0,  184,    0,    0,  184,    0,  217,    0,    0,
    0,    0,    0,    0,  215,    0,    0,  215,  215,  184,
  184,    0,    0,    0,    0,    0,  215,    0,    0,    0,
    0,    0,  215,    0,    0,    0,    0,    0,  217,    0,
    0,  217,    0,    0,  221,  221,  221,    0,    0,  221,
  221,  221,    0,  221,  184,  217,  217,    0,    0,  183,
    0,    0,    0,  221,  221,    0,    0,    0,    0,  183,
    0,    0,  221,  221,    0,  221,  221,  221,  221,  221,
    0,    0,    0,    0,    0,    0,  184,    0,    0,    0,
  217,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  183,    0,    0,  183,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  183,  183,    0,
    0,    0,  217,    0,  221,    0,    0,    0,    0,    0,
  221,  177,  177,  177,    0,    0,  177,  177,  177,    0,
  177,    0,    0,    0,    0,    0,  178,    0,    0,    0,
  177,  177,  183,    0,    0,    0,  178,    0,    0,  177,
  177,    0,  177,  177,  177,  177,  177,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  183,    0,    0,  178,    0,    0,
  178,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  178,  178,    0,    0,    0,    0,
    0,  177,    0,    0,    0,    0,    0,  177,    0,    0,
    0,    0,    0,  184,  184,  184,    0,    0,  184,  184,
  184,    0,  184,    0,    0,    0,    0,    0,    0,  178,
  181,    0,  184,  184,    0,    0,    0,    0,    0,    0,
  181,  184,  184,    0,  184,  184,  184,  184,  184,  217,
  217,  217,    0,    0,  217,  217,  217,    0,  217,    0,
    0,  178,    0,    0,    0,    0,    0,    0,  217,  217,
    0,  181,    0,    0,  181,    0,    0,  217,  217,    0,
  217,  217,  217,  217,  217,    0,    0,    0,  181,  181,
    0,    0,    0,  184,    0,    0,    0,    0,    0,  184,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  183,  183,  183,    0,    0,  183,  183,  183,    0,
  183,    0,  182,  181,    0,    0,    0,    0,    0,  217,
  183,  183,  182,    0,    0,  217,    0,    0,    0,  183,
  183,    0,  183,  183,  183,  183,  183,    0,    0,    0,
    0,    0,    0,    0,    0,  181,    0,    0,    0,    0,
    0,    0,    0,  182,    0,  179,  182,    0,    0,    0,
    0,    0,    0,    0,    0,  179,    0,    0,    0,    0,
  182,  182,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  183,    0,    0,    0,    0,    0,  183,  178,  178,
  178,    0,    0,  178,  178,  178,  179,  178,    0,  179,
  175,    0,    0,    0,    0,  182,    0,  178,  178,    0,
  175,    0,    0,  179,  179,    0,  178,  178,    0,  178,
  178,  178,  178,  178,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  182,    0,    0,
    0,  175,    0,    0,  175,    0,    0,    0,  179,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  175,  175,
    0,    0,  180,    0,    0,    0,    0,    0,  178,    0,
    0,    0,  180,    0,  178,    0,    0,    0,    0,    0,
  179,    0,  181,  181,  181,    0,    0,  181,  181,  181,
    0,  181,    0,  175,    0,    0,    0,    0,    0,    0,
    0,  181,  181,  180,    0,    0,  180,    0,    0,    0,
  181,  181,    0,  181,  181,  181,  181,  181,    0,    0,
  180,  180,    0,    0,    0,  175,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  176,  180,    0,    0,    0,    0,
    0,    0,  181,    0,  176,    0,    0,    0,  181,    0,
    0,    0,    0,    0,  182,  182,  182,    0,    0,  182,
  182,  182,    0,  182,    0,    0,    0,  180,    0,    0,
    0,    0,    0,  182,  182,  176,    0,    0,  176,    0,
    0,    0,  182,  182,    0,  182,  182,  182,  182,  182,
    0,    0,  176,  176,    0,    0,    0,  179,  179,  179,
    0,    0,  179,  179,  179,    0,  179,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  179,  179,    0,    0,
    0,    0,    0,  219,    0,  179,  179,  176,  179,  179,
  179,  179,  179,  219,  182,    0,    0,    0,    0,    0,
  182,    0,  175,  175,  175,    0,    0,  175,  175,  175,
    0,  175,    0,    0,    0,    0,    0,    0,    0,  176,
    0,  175,  175,    0,  219,    0,    0,  219,    0,    0,
  175,  175,    0,  175,  175,  175,  175,  179,    0,    0,
    0,  219,  219,  179,    0,  266,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  266,    0,    0,    0,    0,
    0,    0,    0,    0,  180,  180,  180,    0,    0,  180,
  180,  180,    0,  180,    0,    0,  219,    0,    0,    0,
    0,    0,  175,  180,  180,    0,  266,    0,  175,  266,
    0,    0,  180,  180,    0,  180,  180,  180,  180,  180,
    0,    0,    0,  266,  266,    0,    0,    0,  219,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  330,    0,  266,    0,
    0,    0,    0,    0,  180,    0,  330,    0,    0,    0,
  180,    0,    0,    0,    0,    0,  176,  176,  176,    0,
    0,  176,  176,  176,    0,  176,    0,    0,    0,    0,
  266,    0,    0,  330,  330,  176,  176,  330,  330,  330,
  330,  330,  330,  330,  176,  176,    0,  176,  176,  176,
  176,  176,    0,    0,  330,  330,  330,    0,  330,  330,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  330,  330,    0,
  330,    0,    0,    0,    0,    0,  176,    0,    0,    0,
    0,    0,  176,    0,    0,  219,  219,  219,    0,    0,
  219,  219,  219,    0,  219,    0,    0,    0,    0,  331,
  330,  330,    0,    0,  219,  219,    0,    0,    0,  331,
    0,    0,    0,  219,  219,    0,  219,  219,  219,  219,
  219,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  331,  331,    0,    0,
  331,  331,  331,  331,  331,  331,  331,  266,  266,  266,
    0,    0,  266,  266,  266,    0,  266,  331,  331,  331,
    0,  331,  331,    0,    0,  219,  266,  266,    0,    0,
    0,  219,    0,    0,    0,  266,  266,    0,  266,  266,
  266,  266,  266,    0,    0,    0,    0,    0,    0,    0,
  331,  331,    0,  331,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   53,    0,
    0,    0,    0,  331,  331,    0,    0,    0,   53,    0,
    0,    0,    0,  266,    0,    0,    0,    0,  330,  330,
  330,    0,    0,  330,  330,  330,    0,  330,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  330,    0,   53,
    0,    0,    0,    0,    0,    0,  330,  330,    0,  330,
  330,  330,  330,  330,    0,    0,   53,   53,    0,    0,
    0,    0,  289,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  289,    0,    0,    0,    0,    0,  330,  330,
  330,  330,  330,  330,  330,  330,  330,  330,  330,  330,
  330,   53,    0,  330,  330,  330,    0,    0,  330,  289,
  289,    0,    0,  289,  289,  289,  289,  289,  289,  289,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  289,  289,  289,   53,  289,  289,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  331,  331,  331,    0,    0,  331,  331,  331,    0,
  331,    0,    0,  289,  289,    0,  289,    0,    0,    0,
  331,    0,    0,    0,    0,    0,    0,    0,    0,  331,
  331,    0,  331,  331,  331,  331,  331,    0,    0,    0,
    0,    0,  455,    0,    0,    0,  289,  289,    0,    0,
    0,    0,  455,    0,    0,    0,    0,    0,    0,    0,
    0,  331,  331,  331,  331,  331,  331,  331,  331,  331,
  331,  331,  331,  331,    0,    0,  331,  331,  331,  455,
  455,  331,    0,  455,  455,  455,   76,  455,  455,  455,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  455,  455,   84,  455,  455,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   53,   53,   53,    0,    0,   53,   53,   53,    0,   53,
    0,    0,    0,  455,  455,    0,  455,    0,    0,   53,
   53,    0,    0,    0,    0,    0,    0,    0,   53,   53,
    0,   53,   53,   53,   53,   53,    0,    0,    0,    0,
    0,    0,    0,    0,   57,    0,  455,  455,    0,    0,
    0,    0,    0,    0,   57,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  289,  289,  289,    0,    0,  289,
  289,  289,    0,  289,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  289,    0,   57,    0,    0,    0,    0,
    0,    0,  289,  289,    0,  289,  289,  289,  289,  289,
    0,    0,   57,   57,    0,  277,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,    0,    0,    0,    0,
    0,    0,    0,    0,  289,  289,  289,  289,  289,  289,
  289,  289,  289,  289,  289,  289,  289,   57,    0,  289,
  289,  289,  277,  277,  289,    0,  277,  277,  277,   83,
  277,  277,  277,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  277,  277,   91,  277,  277,   57,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  455,  455,  455,    0,    0,    0,
  455,  455,    0,  455,    0,    0,  277,  277,    0,  277,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  455,  455,    0,  455,  455,  455,  455,  455,
    0,    0,    0,    0,    0,    0,    0,  285,    0,  277,
  277,    0,    0,    0,    0,    0,    0,  285,    0,    0,
    0,    0,    0,    0,  455,  455,  455,  455,  455,  455,
  455,  455,  455,  455,  455,  455,  455,    0,    0,  455,
  455,  455,    0,  456,  285,  285,    0,    0,  285,  285,
  285,   82,  285,  285,  285,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  285,  285,   90,  285,
  285,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   57,   57,   57,    0,
    0,   57,   57,   57,    0,   57,    0,    0,  285,  285,
    0,  285,    0,    0,    0,   57,   57,    0,    0,    0,
    0,    0,    0,    0,   57,   57,    0,   57,   57,   57,
   57,   57,    0,    0,    0,    0,    0,   60,    0,    0,
    0,  285,  285,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  277,  277,  277,
    0,    0,    0,  277,  277,    0,  277,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,  277,    0,  277,  277,
  277,  277,  277,    0,    0,    0,    0,    0,  286,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  286,    0,
    0,    0,    0,    0,    0,    0,    0,  277,  277,  277,
  277,  277,  277,  277,  277,  277,  277,  277,  277,  277,
   61,    0,  277,  277,  277,  286,  286,    0,    0,  286,
  286,  286,   77,  286,  286,  286,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  286,  286,   85,
  286,  286,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  285,
  285,  285,    0,    0,    0,  285,  285,    0,  285,  286,
  286,    0,  286,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  285,  285,    0,
  285,  285,  285,  285,  285,    0,    0,    0,    0,    0,
    0,    0,  286,  286,    0,   60,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  285,
  285,  285,  285,  285,  285,  285,  285,  285,  285,  285,
  285,  285,    0,    0,  285,  285,  285,    0,    0,    0,
    3,    4,    5,    6,    7,    8,    0,    0,    0,    9,
   10,    0,    0,    0,   11,    0,   12,   13,   14,   15,
   16,   17,   18,    0,    0,    0,    0,   19,   20,   21,
   22,   23,   24,   25,    0,    0,   26,    0,    0,    0,
    0,    0,   27,   28,   29,   30,   31,   32,   33,   34,
   35,   36,   37,   38,   39,    0,   40,   41,   61,   42,
   43,   44,    0,   45,   46,   47,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   48,   60,    0,   49,   50,    0,
   51,   52,    0,   53,    0,   54,   55,   56,   57,   58,
  286,  286,  286,    0,   59,    0,  286,  286,    0,  286,
    0,  386,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  286,  286,
    0,  286,  286,  286,  286,  286,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  286,  286,  286,  286,  286,  286,  286,  286,  286,  286,
  286,  286,  286,    0,    0,  286,  286,  286,   61,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    3,    4,
    5,    6,    7,    8,    0,   60,    0,    9,   10,    0,
    0,    0,   11,    0,   12,   13,   14,   15,   16,   17,
   18,    0,    0,    0,    0,   19,   20,   21,   22,   23,
   24,   25,    0,    0,   26,    0,    0,    0,    0,    0,
   27,   28,   29,   30,   31,   32,   33,   34,   35,   36,
   37,   38,   39,    0,   40,   41,    0,   42,   43,   44,
    0,   45,   46,   47,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   48,    0,    0,  262,   50,    0,   51,   52,
    0,   53,    0,   54,   55,   56,   57,   58,   61,    0,
    0,    0,   59,    0,    0,  223,    0,    0,    0,    0,
    0,    0,    0,  413,    0,    0,    0,    0,    0,    4,
    5,    6,    7,    8,    0,    0,    0,    9,   10,    0,
    0,    0,   11,    0,   12,   13,   14,   15,   16,   17,
   18,    0,    0,    0,    0,   19,   20,   21,   22,   23,
   24,   25,    0,    0,   26,    0,    0,    0,    0,    0,
   27,   28,   29,   30,   31,   32,   33,   34,   35,   36,
   37,   38,   39,    0,   40,   41,    0,   42,   43,   44,
    0,   45,   46,   47,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   61,    0,
    0,    0,   48,    0,    0,   49,   50,    0,   51,   52,
   60,   53,    0,   54,   55,   56,   57,   58,    0,    0,
    0,    0,   59,    0,    0,    0,    0,    0,    0,    4,
    5,    6,    7,    8,    0,    0,    0,    9,   10,    0,
    0,    0,   11,    0,   12,   13,   14,   15,   16,   17,
   18,    0,    0,    0,    0,   19,   20,   21,   22,   23,
   24,   25,    0,    0,   26,    0,    0,    0,    0,    0,
   27,   28,   29,   30,   31,   32,   33,   34,   35,   36,
   37,   38,   39,    0,   40,   41,    0,   42,   43,   44,
    0,   45,   46,   47,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,   48,    0,    0,   49,   50,    0,   51,   52,
    0,   53,    0,   54,   55,   56,   57,   58,    0,    4,
    5,    6,   59,    8,    0,    0,    0,    9,   10,    0,
    0,    0,   11,    0,   12,   13,   14,   98,   99,   17,
   18,    0,    0,    0,    0,  100,   20,   21,   22,   23,
   24,   25,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   28,    0,    0,   31,   32,   33,   34,   35,   36,
   37,   38,   39,  219,   40,   41,    0,   42,   43,   44,
    0,   45,   46,   47,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,  220,    0,    0,  108,   50,    0,   51,   52,
    0,  221,  222,   54,   55,   56,   57,   58,    0,    0,
    0,    0,   59,    0,    4,    5,    6,    0,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   15,   16,   17,   18,    0,    0,    0,    0,
   19,   20,   21,   22,   23,   24,   25,    0,    0,   26,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  614,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,   20,   21,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  221,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,   20,   21,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,  415,    0,   51,   52,    0,  221,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  605,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  609,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,   20,   21,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  605,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  808,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  811,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  816,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  308,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  887,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,  308,    0,    0,    0,    0,    0,    0,
  255,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  889,  222,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,  255,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  916,  222,   54,   55,
   56,   57,   58,    0,  308,  308,  308,   59,  308,    0,
    0,    0,  308,  308,    0,    0,  515,  308,  515,  308,
  308,  308,  308,  308,  308,  308,    0,    0,    0,    0,
  308,  308,  308,  308,  308,  308,  308,    0,    0,  308,
    0,    0,    0,    0,    0,    0,  308,    0,    0,  308,
  308,  308,  308,  308,  308,  308,  308,  308,    0,  308,
  308,    0,  308,  308,  308,    0,  308,  308,  308,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
   60,    0,    0,    0,    0,    0,    0,  308,    0,    0,
  308,  308,    0,  308,  308,    0,    0,    0,  308,  308,
  308,  308,  308,    0,  255,  255,  255,  308,  255,    0,
    0,    0,  255,  255,    0,    0,    0,  255,    0,  255,
  255,  255,  255,  255,  255,  255,    0,    0,    0,    0,
  255,  255,  255,  255,  255,  255,  255,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  255,    0,    0,  255,
  255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
  255,    0,  255,  255,  255,    0,  255,  255,  255,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  255,    0,    0,
  255,  255,    0,  255,  255,    0,  255,  255,  255,  255,
  255,  255,  255,    0,    4,    5,    6,  255,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,   20,   21,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  267,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   15,   16,   17,   18,    0,    0,    0,    0,
   19,   20,   21,   22,   23,   24,   25,    0,    0,   26,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,    0,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,  219,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,    0,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   15,   16,   17,   18,    0,    0,    0,    0,
   19,   20,   21,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  527,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  638,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
  223,    0,    0,    0,    0,    0,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  527,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,   61,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  517,    0,  220,    0,    0,
  108,   50,  517,   51,   52,    0,  708,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  517,    0,
    0,    0,    0,    0,    0,  223,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  770,    0,   54,   55,
   56,   57,   58,    0,    4,    5,    6,   59,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   98,   99,   17,   18,    0,    0,    0,    0,
  100,  101,  102,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,    0,   40,
   41,    0,   42,   43,   44,    0,   45,   46,   47,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   61,    0,
    0,    0,    0,    0,    0,  223,    0,  220,    0,    0,
  108,   50,    0,   51,   52,    0,  849,    0,   54,   55,
   56,   57,   58,    0,    0,    0,    0,   59,    0,  517,
  517,  517,    0,  517,    0,    0,    0,  517,  517,    0,
    0,    0,  517,    0,  517,  517,  517,  517,  517,  517,
  517,    0,    0,    0,    0,  517,  517,  517,  517,  517,
  517,  517,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  517,    0,    0,  517,  517,  517,  517,  517,  517,
  517,  517,  517,    0,  517,  517,    0,  517,  517,  517,
    0,  517,  517,  517,    0,    0,    0,    0,   61,    0,
    0,    0,    0,    0,    0,  216,    0,    0,    0,    0,
    0,    0,  517,    0,    0,  517,  517,    0,  517,  517,
    0,    0,    0,  517,  517,  517,  517,  517,    0,    4,
    5,    6,  517,    8,    0,    0,    0,    9,   10,    0,
    0,    0,   11,    0,   12,   13,   14,   98,   99,   17,
   18,    0,    0,    0,    0,  100,  101,  102,   22,   23,
   24,   25,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   28,    0,    0,   31,   32,   33,   34,   35,   36,
   37,   38,   39,    0,   40,   41,    0,   42,   43,   44,
    0,   45,   46,   47,    0,    0,    0,    0,  216,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  220,    0,    0,  108,   50,    0,   51,   52,
  897,    0,    0,   54,   55,   56,   57,   58,    0,    4,
    5,    6,   59,    8,    0,    0,    0,    9,   10,    0,
    0,    0,   11,    0,   12,   13,   14,   15,   16,   17,
   18,    0,    0,    0,    0,   19,   20,   21,   22,   23,
   24,   25,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   28,    0,    0,   31,   32,   33,   34,   35,   36,
   37,   38,   39,    0,   40,   41,    0,   42,   43,   44,
    0,   45,   46,   47,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  220,    0,    0,  108,   50,    0,   51,   52,
    0,    0,    0,   54,   55,   56,   57,   58,    0,  216,
  216,  216,   59,  216,    0,    0,    0,  216,  216,    0,
    0,    0,  216,    0,  216,  216,  216,  216,  216,  216,
  216,    0,    0,    0,    0,  216,  216,  216,  216,  216,
  216,  216,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  216,    0,    0,  216,  216,  216,  216,  216,  216,
  216,  216,  216,    0,  216,  216,    0,  216,  216,  216,
    0,  216,  216,  216,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  216,    0,    0,  216,  216,    0,  216,  216,
    0,    0,    0,  216,  216,  216,  216,  216,    0,    0,
    0,    0,  216,    4,    5,    6,    0,    8,    0,    0,
    0,    9,   10,    0,    0,    0,   11,    0,   12,   13,
   14,   98,   99,   17,   18,    0,    0,    0,    0,  100,
  101,  102,   22,   23,   24,   25,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  103,    0,    0,   31,   32,
   33,   34,   35,   36,   37,   38,   39,    0,   40,   41,
    0,   42,   43,   44,    0,    0,    0,   47,    0,    0,
    0,  185,  180,    0,    0,    0,  183,  181,    0,  182,
    0,  184,    0,    0,    0,    0,  822,    0,    0,  108,
   50,    0,   51,   52,  177,    0,  176,   54,   55,   56,
   57,   58,    0,    0,    0,    0,  109,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  179,    0,
  187,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  178,    0,
  186,    0,    0,    0,    0,    0,  185,  180,    0,    0,
    0,  183,  181,    0,  182,    0,  184,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  177,
    0,  176,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   55,  179,    0,  187,    0,    0,    0,    0,
    0,    0,   55,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  178,    0,  186,    0,    0,    0,    0,
    0,    0,    0,   55,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   55,   55,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  118,  119,  120,  121,  122,  123,  124,  125,    0,
    0,  126,  127,  128,  129,  130,    0,    0,  131,  132,
  133,  134,  135,  136,  137,   55,    0,  138,  139,  140,
  194,  195,  196,  197,  145,  146,  147,  148,  149,  150,
  151,  152,  153,  154,  155,  156,  198,  199,  200,  160,
  246,    0,  201,    0,    0,    0,    0,   55,    0,    0,
    0,    0,    0,  162,  163,    0,  164,  165,  166,  167,
    0,  168,  169,    0,    0,  170,    0,    0,    0,  171,
  172,  173,  174,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  175,    0,   54,  118,  119,  120,  121,
  122,  123,  124,  125,    0,    0,  126,  127,  128,  129,
  130,    0,    0,  131,  132,  133,  134,  135,  136,  137,
    0,    0,  138,  139,  140,  194,  195,  196,  197,  145,
  146,  147,  148,  149,  150,  151,  152,  153,  154,  155,
  156,  198,  199,  200,  160,    0,    0,  201,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  753,  162,  163,
    0,  164,  165,  166,  167,    0,  168,  169,    0,    0,
  170,    0,    0,    0,  171,  172,  173,  174,    0,  185,
  180,    0,  188,    0,  183,  181,    0,  182,  175,  184,
   54,    0,    0,    0,   55,   55,   55,    0,    0,   55,
   55,   55,  177,   55,  176,    0,    0,    0,    0,    0,
    0,    0,    0,   55,   55,    0,    0,    0,    0,    0,
    0,    0,   55,   55,    0,   55,   55,   55,   55,   55,
    0,    0,    0,    0,    0,    0,  179,    0,  187,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  178,    0,  186,    0,
    0,    0,  185,  180,    0,    0,    0,  183,  181,    0,
  182,    0,  184,    0,    0,    0,    0,    0,    0,    0,
    4,    5,    6,    0,    8,  177,    0,  176,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   98,   99,
   17,   18,    0,    0,    0,    0,  100,  101,  102,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,  179,
    0,  187,  103,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,    0,   40,   41,    0,   42,   43,
   44,    0,    0,    0,   47,    0,    0,    0,    0,  178,
    0,  186,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  277,    0,    0,  351,   50,    0,   51,
   52,    0,  751,  752,   54,   55,   56,   57,   58,    0,
    0,    0,    0,  109,    0,    0,    0,    0,    0,  118,
  119,  120,  121,  122,  123,  124,  125,    0,    0,  126,
  127,  128,  129,  130,    0,    0,  131,  132,  133,  134,
  135,  136,  137,    0,    0,  138,  139,  140,  141,  142,
  143,  144,  145,  146,  147,  148,  149,  150,  151,  152,
  153,  154,  155,  156,  157,  158,  159,  160,   35,   36,
  161,   38,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  162,  163,    0,  164,  165,  166,  167,    0,  168,
  169,    0,    0,  170,    0,    0,    0,  171,  172,  173,
  174,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  175,  118,  119,  120,  121,  122,  123,  124,  125,
    0,    0,  126,  127,  128,  129,  130,    0,    0,  131,
  132,  133,  134,  135,  136,  137,    0,    0,  138,  139,
  140,  194,  195,  196,  197,  145,  146,  147,  148,  149,
  150,  151,  152,  153,  154,  155,  156,  198,  199,  200,
  160,  282,  283,  201,  284,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  162,  163,    0,  164,  165,  166,
  167,    0,  168,  169,    0,    0,  170,    0,    0,    0,
  171,  172,  173,  174,    0,  185,  180,    0,    0,    0,
  183,  181,    0,  182,  175,  184,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  177,    0,
  176,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  185,  180,    0,    0,    0,  183,  181,    0,  182,    0,
  184,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  179,  177,  187,  176,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  185,  180,    0,    0,    0,
  183,  181,    0,  182,    0,  184,    0,    0,    0,    0,
    0,    0,  178,    0,  186,    0,    0,  179,  177,  187,
  176,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  185,  180,    0,    0,    0,  183,  181,    0,  182,    0,
  184,    0,    0,    0,    0,    0,    0,  178,    0,  186,
    0,    0,  179,  177,  187,  176,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  178,    0,  186,    0,    0,  179,    0,  187,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  178,    0,  186,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  118,  119,  120,  121,  122,
  123,  124,  125,    0,    0,  126,  127,  128,  129,  130,
    0,    0,  131,  132,  133,  134,  135,  136,  137,    0,
    0,  138,  139,  140,  194,  195,  196,  197,  145,  146,
  147,  148,  149,  150,  151,  152,  153,  154,  155,  156,
  198,  199,  200,  160,    0,    0,  201,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  162,  163,    0,
  164,  165,  166,  167,    0,  168,  169,    0,    0,  170,
    0,    0,    0,  171,  172,  173,  174,  509,  510,    0,
    0,  511,    0,    0,    0,    0,    0,  175,    0,    0,
    0,    0,  162,  163,    0,  164,  165,  166,  167,    0,
  168,  169,    0,    0,  170,    0,    0,    0,  171,  172,
  173,  174,  515,  257,    0,    0,  516,    0,    0,    0,
    0,    0,  175,    0,    0,    0,    0,  162,  163,    0,
  164,  165,  166,  167,    0,  168,  169,    0,    0,  170,
    0,    0,    0,  171,  172,  173,  174,  545,  510,    0,
    0,  546,    0,    0,    0,    0,    0,  175,    0,    0,
    0,    0,  162,  163,    0,  164,  165,  166,  167,    0,
  168,  169,    0,    0,  170,    0,    0,    0,  171,  172,
  173,  174,    0,  185,  180,    0,    0,    0,  183,  181,
    0,  182,  175,  184,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  177,    0,  176,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  185,  180,
    0,    0,    0,  183,  181,    0,  182,    0,  184,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  179,  177,  187,  176,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  185,  180,    0,    0,    0,  183,  181,
    0,  182,    0,  184,    0,    0,    0,    0,    0,    0,
  178,    0,  186,    0,    0,  179,  177,  187,  176,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  185,  180,
    0,    0,    0,  183,  181,    0,  182,    0,  184,    0,
    0,    0,    0,    0,    0,  178,    0,  186,    0,    0,
  179,  177,  187,  176,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  185,  180,    0,    0,    0,  183,  181,
    0,  182,    0,  184,    0,    0,    0,    0,    0,    0,
  178,    0,  186,    0,    0,  179,  177,  187,  176,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  185,  180,
    0,    0,    0,  183,  181,    0,  182,    0,  184,    0,
    0,    0,    0,    0,    0,  178,    0,  186,    0,    0,
  179,  177,  187,  176,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  185,  180,    0,    0,    0,  183,  181,    0,  182,
  178,  184,  186,    0,    0,  179,    0,  187,    0,    0,
    0,    0,    0,    0,  177,    0,  176,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  178,    0,  186,    0,    0,
  600,  510,    0,    0,  601,    0,    0,    0,  179,    0,
  187,    0,    0,    0,    0,  162,  163,    0,  164,  165,
  166,  167,    0,  168,  169,    0,    0,  170,    0,    0,
    0,  171,  172,  173,  174,  602,  257,    0,  178,  603,
  186,    0,    0,    0,    0,  175,    0,    0,    0,    0,
  162,  163,    0,  164,  165,  166,  167,    0,  168,  169,
    0,    0,  170,    0,    0,    0,  171,  172,  173,  174,
  640,  510,    0,    0,  641,    0,    0,    0,    0,    0,
  175,    0,    0,    0,    0,  162,  163,    0,  164,  165,
  166,  167,    0,  168,  169,    0,    0,  170,    0,    0,
    0,  171,  172,  173,  174,  642,  257,    0,    0,  643,
    0,    0,    0,    0,    0,  175,    0,    0,    0,    0,
  162,  163,    0,  164,  165,  166,  167,    0,  168,  169,
    0,    0,  170,    0,    0,    0,  171,  172,  173,  174,
  716,  510,   40,    0,  717,    0,    0,    0,    0,    0,
  175,    0,   40,    0,    0,  162,  163,    0,  164,  165,
  166,  167,    0,  168,  169,    0,    0,  170,    0,    0,
    0,  171,  172,  173,  174,  718,  257,    0,    0,  719,
    0,    0,    0,   40,    0,  175,    0,    0,    0,    0,
  162,  163,    0,  164,  165,  166,  167,    0,  168,  169,
   40,   40,  170,    0,    0,    0,  171,  172,  173,  174,
    0,    0,    0,    0,    0,    0,    0,    0,  892,  510,
  175,    0,  893,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  162,  163,    0,  164,  165,  166,  167,
    0,  168,  169,    0,    0,  170,    0,    0,    0,  171,
  172,  173,  174,    0,  185,  180,    0,    0,    0,  183,
  181,    0,  182,  175,  184,    0,    0,   40,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  177,    0,  176,
    0,    0,    0,  334,  329,    0,    0,    0,  332,  330,
    0,  331,    0,  333,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  326,    0,  325,  324,
    0,  179,    0,  187,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   42,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   42,   47,    0,    0,    0,    0,    0,
  328,  178,    0,  186,   47,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   42,    0,    0,    0,    0,    0,
  327,    0,    0,    0,    0,   47,    0,    0,    0,    0,
    0,   42,   42,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   47,   47,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   40,   40,   40,    0,    0,   40,
   40,   40,    0,   40,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   40,    0,    0,    0,    0,    0,    0,
    0,    0,   40,   40,    0,   40,   40,   40,   40,   40,
    0,    0,    0,    0,    0,    0,    0,    0,   42,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   47,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  894,  257,    0,    0,  895,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  162,  163,    0,  164,
  165,  166,  167,  674,  168,  169,    0,    0,  170,    0,
    0,    0,  171,  172,  173,  174,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  175,    0,  309,  310,
  311,  312,  313,  314,  315,  316,  317,  318,  319,  320,
  321,    0,    0,  322,  323,   42,   42,   42,    0,    0,
   42,   42,   42,    0,   42,    0,   47,   47,   47,    0,
    0,   47,   47,   47,   42,   47,    0,    0,    0,    0,
    0,    0,    0,   42,   42,   47,   42,   42,   42,   42,
   42,    0,    0,    0,   47,   47,    0,   47,   47,   47,
   47,   47,    4,    5,    6,    0,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,    0,    0,  351,   50,
    0,   51,   52,    0,  868,  869,   54,   55,   56,   57,
   58,    0,    4,    5,    6,  109,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,  104,
   34,   35,   36,  105,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  106,    0,    0,  107,    0,    0,  108,   50,
    0,   51,   52,    0,    0,    0,   54,   55,   56,   57,
   58,    0,    4,    5,    6,  109,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,    0,    0,  351,   50,
    0,   51,   52,    0,  352,    0,   54,   55,   56,   57,
   58,    0,    4,    5,    6,  109,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  390,    0,    0,   49,   50,
    0,   51,   52,    0,   53,    0,   54,   55,   56,   57,
   58,    0,    4,    5,    6,  109,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,  104,
   34,   35,   36,  105,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  107,    0,    0,  108,   50,
    0,   51,   52,    0,    0,    0,   54,   55,   56,   57,
   58,    0,    4,    5,    6,  109,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,    0,    0,  108,   50,
    0,   51,   52,    0,    0,    0,   54,   55,   56,   57,
   58,    0,    4,    5,    6,  109,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   98,   99,   17,   18,    0,    0,    0,    0,  100,  101,
  102,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  103,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,    0,   40,   41,    0,
   42,   43,   44,    0,    0,    0,   47,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  822,    0,    0,  108,   50,
    0,   51,   52,    0,    0,    0,   54,   55,   56,   57,
   58,    0,    0,    0,    0,  109,
/* start of table check */  2, 
    3,    4,    5,   81,   90,   15,   16,   27,  389,   19,
    6,  409,   15,   16,  383,  242,   19,    6,    7,   21,
    3,   47,   21,   26,  377,   38,   73,   49,   15,   16,
  107,   46,   19,   10,  112,   51,   52,   46,   27,   53,
   93,   51,  103,   10,    7,  586,   49,   50,   51,    7,
   53,  449,   44,  350,   10,  372,   11,   60,   54,    2,
    3,    4,    5,  232,   27,   54,  124,   60,   10,   27,
   10,   32,  125,    4,    5,   51,   52,   10,  568,   46,
  570,   58,   59,   40,  209,   41,   46,   90,   44,  214,
   10,  216,  217,   40,   96,   46,  123,   96,  288,  688,
  102,  291,   44,  106,   10,  108,   49,  314,   46,  308,
   53,  507,  508,  115,   10,   10,   58,   59,   73,   59,
   60,   41,   53,   40,   38,  123,   46,   44,   42,   46,
  688,   91,   92,  312,  313,  693,   10,   46,   10,   10,
   91,   92,   10,  576,   40,   10,   41,   90,   44,   44,
   46,  358,   46,   91,   92,  110,    2,  314,   40,   90,
  262,   37,   58,   59,   59,  108,   42,   43,   10,   45,
   44,   47,   46,   44,   91,   92,   44,  302,  303,  304,
  305,   37,   91,   92,   58,   59,   42,   59,   46,   10,
   41,   47,   41,   44,   59,   91,   92,   91,   92,   32,
  278,  358,   44,   49,  207,  208,  209,  268,   10,  562,
   61,  214,  304,  216,  217,  218,  308,   91,   92,   10,
   41,   10,    2,    3,    4,    5,    6,  544,  124,  239,
  399,  241,  242,   91,   92,   10,  239,   37,  241,  242,
  262,  123,   42,   43,   10,   45,  563,   47,  251,  351,
  124,  840,  239,  280,  241,  242,   58,   59,   10,  262,
  125,   46,  108,   38,  207,   40,  242,   42,   59,   49,
   59,   46,  330,   53,  707,  218,  466,   41,  575,   32,
   10,  677,  840,  659,   59,  290,  291,  218,  664,   41,
  267,   10,  369,   59,  784,  785,  786,   61,  301,  302,
  303,  304,  305,  306,  307,  308,   91,   92,  251,  850,
   90,   41,   26,  391,  340,  341,  256,  343,  301,  262,
  346,  347,   41,  306,  339,  267,  342,  725,  108,  351,
  339,  352,  342,  731,  290,  291,   50,  350,  352,  342,
  387,  710,  345,  282,  370,  348,  349,  350,  351,  352,
  342,  340,  345,  314,   10,  348,  349,  350,  301,  420,
  421,   44,  279,  306,   61,  342,  342,  312,  313,  372,
  373,  267,  339,  349,  350,  341,  402,  403,   61,  339,
  369,  442,  443,   61,  290,  291,  342,  877,  339,  405,
  304,   10,  106,  267,  621,  356,  357,  341,  280,  415,
  342,  339,   10,   59,   10,  251,  426,   44,  351,  352,
  365,  437,  415,  402,  268,  269,  262,  850,  543,  339,
  789,  352,  339,   10,  777,  806,  429,  207,  415,  372,
  339,  457,  387,  388,  389,  349,  350,  426,  218,  608,
   59,  290,  291,  339,   10,  339,  322,  436,   10,   41,
  619,   59,   59,   59,   41,   44,  773,  523,  524,  342,
  858,    2,    3,  426,   41,  339,  322,  456,  426,   61,
  308,  251,   59,  264,  263,  264,   44,  846,  269,  281,
  271,  339,  262,  271,   61,  273,  429,  275,  341,    0,
  551,   44,   41,   59,  208,  209,   58,   59,  264,   10,
  214,   10,  216,  217,  269,  351,  271,   38,   49,  907,
  512,   42,   61,  512,   54,  279,   56,  520,  517,   41,
  837,  301,  322,  341,  523,  524,  306,  520,  269,  304,
  271,  306,  307,  308,  309,  263,  264,  337,  338,   61,
  543,  544,  269,   16,  271,  547,   19,   61,   59,   58,
   59,  306,   40,  573,  339,  724,  552,  341,  727,  728,
  563,  316,  317,  552,  339,  651,  123,  108,  594,   41,
   41,  351,  352,  322,  349,  350,   93,  580,   10,  582,
  125,  358,  585,  429,  573,   10,  541,  308,  302,  303,
  304,  305,  372,  307,  308,  720,  279,  623,  624,  575,
  358,  544,   10,  341,  593,  621,  322,  583,  264,  564,
  573,  621,  268,  269,  322,  573,   41,   44,  621,   44,
  563,  264,  348,  262,  685,  628,   58,   59,   44,  632,
   46,  271,   40,  269,   59,  308,   44,  580,   46,  582,
  263,  264,  585,  812,  279,  814,  269,   44,  651,  429,
   58,   59,  341,  656,  262,  263,  264,  660,  661,  373,
   41,  269,   44,  656,   41,  125,  207,  660,  661,   41,
  795,  737,  738,   32,  735,   91,   92,  218,  341,  341,
   44,  341,   46,   91,   92,  854,  855,  279,  749,  632,
  633,  860,  341,   38,   93,   40,   44,   42,  264,  702,
   41,  415,  279,  269,  264,  267,   61,   44,  651,   44,
  251,  714,  715,  729,   44,  123,  124,  720,  304,  888,
  651,  262,  304,  269,  679,   37,   38,   91,   92,  267,
   42,   43,  264,   45,  580,   47,  582,  906,  350,  585,
  909,  341,  729,  341,  830,  341,  824,   44,  751,  752,
   44,  920,   41,  729,  757,  758,  759,  279,  267,  702,
  301,   41,   41,   41,  544,  306,  239,  125,  241,  242,
  773,  714,  715,  304,  264,  306,  307,  308,  309,   41,
  264,  342,   94,  563,  263,  788,  632,   44,  264,  792,
  264,   41,  795,  306,  307,   40,  309,  125,  801,   44,
  580,   46,  582,  316,  317,  585,  124,   44,  751,  752,
  351,  314,  124,   41,  757,  758,  759,   44,  349,  350,
  751,  752,   40,   41,  264,  264,   44,  830,   46,  543,
  773,  372,  835,  264,  837,  267,  264,   44,   40,  865,
  856,  844,   44,   61,   46,  788,   91,   92,   44,  792,
   41,  806,  632,  356,  357,  358,  702,  788,  801,  267,
  863,  308,   44,  279,  350,  868,  869,  125,  714,  715,
  264,  651,  280,   91,   92,  864,  879,  264,  123,  264,
   44,  884,   46,  350,  125,  264,   44,  830,  429,   91,
   92,  350,  835,  896,  837,   44,  264,   37,   38,  830,
  350,  844,   42,   43,   93,   45,  124,   47,  911,   10,
  125,  757,  758,  759,  628,  279,  919,   40,   93,   41,
  863,  123,  702,  339,   93,  868,  869,   91,   92,  271,
  125,  339,  863,   41,  714,  715,  879,  868,  869,   41,
   93,  884,  125,   44,  264,   46,  792,  124,   10,   63,
  124,  124,  314,  896,  124,  801,  318,   58,   59,  304,
    5,  306,  307,  308,  309,  896,  878,  124,  911,  124,
  904,  751,  752,    0,    6,  339,  919,  757,  758,  759,
  911,  342,   44,   10,   46,  777,  562,  693,  919,  835,
   91,   92,   80,  773,  356,  357,   58,   59,  844,  218,
  693,   73,   -1,  544,  349,  350,  720,   -1,  788,  314,
  322,   -1,  792,   -1,   41,   -1,   -1,   44,   -1,   -1,
   -1,  801,  563,  124,   -1,  337,  338,   -1,   -1,   91,
   92,   58,   59,  879,  279,  280,   -1,   -1,  884,  580,
   -1,  582,   -1,   10,  585,   -1,   -1,   -1,   10,   -1,
  830,  356,  357,  358,   44,  835,   46,  837,   -1,   -1,
   41,  279,  124,   44,  844,   46,   93,   -1,   -1,   -1,
  314,   38,   -1,   40,   -1,   42,  314,  279,  280,   46,
   61,  795,   44,  863,   46,   -1,   -1,   -1,  868,  869,
  314,  632,   59,   -1,  339,   -1,   58,   59,  125,  879,
   -1,   91,   92,  314,  884,   15,   16,   -1,   -1,   19,
   91,   92,  356,  357,  358,  279,  896,   -1,  356,  357,
  358,  339,  293,  294,  295,  296,  297,   -1,   -1,   91,
   92,  911,  356,  357,  358,   45,   46,  339,   -1,  919,
   -1,   51,   52,  124,   -1,  356,  357,  358,   -1,   -1,
   60,   61,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  702,  124,   -1,   -1,   -1,  267,   -1,   -1,   -1,
   -1,   10,   -1,  714,  715,  339,   41,   -1,   -1,   44,
   -1,   46,  322,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    0,   -1,   -1,   -1,   -1,   -1,   61,  337,  338,   38,
   10,   40,   -1,   42,   -1,  267,   -1,   46,   -1,   -1,
  751,  752,   -1,   -1,   -1,   -1,  757,  758,  759,   -1,
   59,   -1,   -1,   -1,   -1,   -1,   91,   92,   -1,   -1,
   -1,   41,  773,   -1,   44,  262,  263,  264,  339,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,  788,   58,   59,
   -1,  792,   10,   63,  281,  282,   -1,   -1,   -1,  124,
  801,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   10,   -1,   -1,  339,   -1,   -1,
   -1,   -1,   40,   93,   -1,   -1,   44,   -1,   46,  279,
   -1,   -1,   -1,   -1,  835,   -1,  837,   -1,  279,   -1,
   58,   59,   38,  844,   40,  267,   42,   -1,   -1,  219,
   46,  221,  222,  223,   -1,  125,   -1,   -1,   -1,   -1,
   -1,  348,  863,   59,   10,   -1,   -1,  868,  869,  239,
   -1,  241,  242,   91,   92,   -1,   -1,  304,  879,  306,
  307,  308,  309,  884,    2,    3,   -1,   -1,   -1,  339,
   -1,   -1,   38,   -1,   40,  896,   42,  267,  339,   -1,
   46,   -1,   -1,   -1,   -1,  123,  124,   -1,   -1,   -1,
  911,   -1,  339,   59,   -1,   -1,   -1,  339,  919,   -1,
   -1,   -1,  349,  350,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   49,   -1,   -1,   -1,   -1,   -1,   10,   -1,  309,
  310,  311,  312,  313,  314,  315,  316,  317,  318,  319,
  320,  321,  322,  323,  279,  325,  326,  327,  328,  329,
  330,  331,  332,  333,  334,   -1,   -1,   -1,   41,   -1,
   -1,   44,  342,   -1,   -1,  345,   -1,   -1,  348,  349,
  350,   -1,   -1,   -1,   -1,   58,   59,   -1,   -1,   -1,
  108,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   -1,  304,   -1,  306,  307,  308,
  309,  281,  282,   -1,  339,   -1,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
    0,  401,   -1,   -1,   -1,  405,   -1,   -1,  408,  409,
  339,  411,  412,   -1,   -1,  415,   -1,   -1,   -1,  267,
  349,  350,  125,   -1,   -1,   -1,   -1,   -1,  428,  329,
  330,   -1,  280,  333,  334,   -1,   51,   52,  438,    2,
    3,   41,  342,   -1,   -1,  445,   -1,   -1,  348,  449,
   -1,  451,   15,   16,   -1,   -1,   19,   -1,   -1,  207,
   -1,   -1,   10,   26,   -1,   -1,   -1,   -1,   -1,   -1,
  470,  471,   -1,   -1,   -1,   -1,   -1,   -1,  304,   -1,
  306,  307,  308,  309,   -1,   -1,   49,   50,   51,   -1,
   38,  339,   40,   -1,   42,  495,   -1,   60,   46,   -1,
   -1,   -1,   -1,  251,   -1,   -1,   10,   -1,   -1,   -1,
   -1,   59,   -1,  339,  262,   -1,   -1,   -1,   -1,   -1,
  520,   -1,   -1,  349,  350,  125,   -1,  527,  304,   -1,
  306,  307,  308,  309,   -1,   -1,   40,   -1,   -1,   -1,
   44,   -1,   46,  106,   -1,  108,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  301,   58,   59,   -1,   -1,  306,  262,
  263,  264,   -1,  339,  267,  268,  269,   -1,  271,   -1,
   -1,   -1,   -1,  349,  350,  575,   -1,   -1,   -1,   -1,
   -1,   -1,    0,  583,   -1,   -1,   -1,   91,   92,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
   -1,   10,   -1,  351,  604,  605,   -1,   -1,   -1,  609,
   -1,   -1,   -1,  228,  614,   -1,   -1,  232,   -1,  123,
  124,  621,   -1,   41,  372,   -1,   -1,  242,   -1,   38,
   -1,   40,   -1,   42,   -1,   -1,   -1,   46,  638,  342,
   -1,   -1,   -1,   -1,  207,  208,  209,   -1,   -1,   -1,
   59,  214,   -1,  216,  217,   -1,  656,  657,   -1,   -1,
  660,  661,  262,  263,  264,   -1,   40,   -1,  268,  269,
   44,  271,   46,   -1,  674,  675,  239,   -1,  241,  242,
   -1,  429,   -1,   -1,  684,   40,   41,   -1,  251,   44,
   -1,   46,   -1,   -1,   -1,   -1,   -1,   -1,   10,  262,
   -1,   -1,   -1,   -1,   -1,   -1,   61,  125,  708,   -1,
   -1,   -1,   -1,   -1,    0,   -1,   -1,   91,   92,   -1,
   -1,  721,  722,  723,   10,  725,   38,  342,   40,  729,
   42,  731,   -1,   -1,   46,   -1,   91,   92,  301,  302,
  303,  304,  305,  306,  307,  308,   -1,   59,   -1,  123,
  750,   -1,   -1,   -1,   -1,   41,  304,   -1,  306,  307,
  308,  309,   -1,  267,   -1,  380,   -1,   -1,  123,  124,
  770,   -1,   -1,   59,   -1,   -1,  280,   -1,   -1,  342,
   -1,   -1,  345,   -1,  399,  348,  349,  350,  351,   -1,
   -1,  339,   -1,   -1,   -1,   -1,  544,   -1,   -1,   -1,
  800,  349,  350,   -1,  419,   -1,   -1,   -1,  808,  372,
  373,  811,   -1,   -1,   -1,  563,  816,   37,   38,   -1,
   -1,   -1,   42,   43,   -1,   45,   -1,   47,   41,   -1,
   -1,   44,  580,   46,  582,  339,   -1,  585,   -1,  125,
   60,   -1,   62,   -1,  262,  263,  264,   -1,   61,  849,
  268,  269,  415,  271,   -1,   -1,  856,   -1,  858,   -1,
   -1,   40,   41,   -1,   -1,   44,  429,   46,   -1,   -1,
   -1,   -1,   -1,   -1,   94,   -1,   -1,   -1,   91,   92,
   -1,   -1,   61,   -1,  632,   -1,   -1,  887,   -1,  889,
   -1,  506,  507,  508,   -1,  304,   -1,  306,  307,  308,
  309,   -1,  517,   -1,  124,  279,  280,  907,  523,  524,
    0,  124,   91,   92,   -1,   -1,  916,   -1,   -1,   -1,
   10,   -1,   -1,   -1,  279,  280,   -1,   -1,   -1,  219,
  339,  221,  222,   -1,  549,   -1,   -1,   -1,   -1,   -1,
  349,  350,   -1,   -1,  123,  124,   -1,   -1,   -1,   -1,
  565,   41,   -1,  568,  702,  570,   -1,  520,   -1,   -1,
  575,  576,   -1,   -1,   -1,  339,  714,  715,   58,   59,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
  543,  544,  268,  269,  339,  271,   -1,   -1,   -1,   -1,
   -1,   -1,  304,  608,  306,  307,  308,  309,   -1,   -1,
  563,   -1,   -1,   93,  619,   -1,   -1,   -1,   -1,  757,
  758,  759,   -1,   -1,   -1,   -1,   -1,  580,   -1,  582,
   -1,   -1,  585,   -1,   -1,  773,   -1,  339,   -1,   -1,
   -1,   41,   -1,   -1,   44,  125,   46,  349,  350,   -1,
   -1,   -1,   -1,   -1,  792,   -1,   -1,   -1,   -1,   -1,
   -1,   61,   -1,  801,   -1,   -1,   -1,   -1,  621,  349,
  350,   -1,  677,   -1,   -1,  628,  279,   -1,   -1,  632,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   91,   92,   -1,  699,   -1,   -1,  835,  703,  837,
  705,   -1,  707,  656,   -1,   -1,  844,  660,  661,   -1,
  279,  280,  322,   -1,   -1,   -1,    0,  327,  328,  724,
   -1,  401,  727,  728,  124,  405,   10,  337,  338,  409,
   -1,   -1,  737,  738,   -1,  415,  339,   -1,   -1,   -1,
   -1,  879,   -1,   -1,   -1,   -1,  884,   -1,   -1,  702,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,
   -1,  714,  715,   -1,   -1,  445,   -1,  720,   -1,  449,
  339,  451,   -1,   -1,   -1,   59,   -1,   -1,   -1,  784,
  785,  786,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  281,  282,   -1,  757,  758,  759,  812,   -1,  814,
  290,  291,   -1,  293,  294,  295,  296,  297,    0,   -1,
  773,   -1,   -1,   40,   41,   -1,   -1,   44,   10,   46,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  792,
   -1,  125,  795,   -1,   61,  850,   -1,  527,  801,  854,
  855,   -1,   -1,   -1,   -1,  860,   -1,   -1,   -1,   41,
   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,  348,   -1,
   -1,   -1,  877,  878,   91,   92,   -1,   59,   -1,  279,
   -1,   -1,  835,  888,  837,   -1,   -1,   -1,   -1,   -1,
   -1,  844,   -1,   -1,   -1,  575,   -1,   -1,   -1,  904,
   -1,  906,   -1,  583,  909,   -1,  123,  124,   -1,   -1,
   -1,   -1,   37,   38,   -1,  920,   -1,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,  605,  879,   -1,   -1,  609,
   -1,  884,   -1,   -1,  614,   60,   -1,   62,   -1,  339,
   -1,  621,   -1,  125,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   94,
   -1,   96,   -1,   -1,   -1,   -1,    0,   -1,  262,  263,
  264,   -1,   -1,   -1,  268,  269,   10,  271,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  675,   -1,   -1,   -1,  124,
   -1,  126,   -1,   -1,  684,   -1,   -1,   -1,   -1,   33,
   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,   43,
   44,   45,   46,   47,   -1,   -1,   -1,   -1,  708,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   61,   62,   63,
   -1,   -1,   -1,   -1,   -1,  725,   -1,   -1,   -1,   -1,
   -1,  731,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  279,  280,   -1,   -1,   -1,   91,   92,   93,
   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  262,  263,  264,   -1,   -1,   -1,  268,  269,   -1,  271,
  770,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,  123,
  124,  125,  126,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
  800,   -1,  339,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   33,  811,   -1,   -1,   37,   38,  816,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  849,
   -1,   -1,   -1,   -1,   -1,   -1,  856,   -1,  858,  304,
  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,   -1,  319,  320,   -1,  322,  323,  324,
  325,   -1,  327,  328,   -1,   -1,  331,  887,   -1,  889,
  335,  336,  337,  338,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,  349,   -1,   -1,  907,   -1,   -1,
   -1,   -1,   -1,  257,  258,  259,  916,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,  271,  272,  273,
  274,  275,  276,  277,  278,  279,  280,  281,  282,  283,
  284,  285,  286,  287,  288,  289,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,  303,
  304,  305,  306,  307,  308,  309,  310,  311,  312,  313,
   -1,  315,  316,  317,   -1,  319,  320,  321,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,  339,  340,  341,  342,  343,
  344,   -1,  346,  347,  348,  349,  350,  351,  352,  353,
  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,  279,  280,  281,  282,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,    0,   -1,  337,  338,  339,  340,  341,  342,
  343,  344,   10,  346,  347,  348,  349,  350,  351,  352,
  353,  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,   -1,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,  339,  340,   -1,  342,  343,  344,   -1,  346,  347,
  348,  349,  350,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,    0,   -1,  337,
  338,  339,  340,   -1,  342,  343,  344,   10,  346,  347,
  348,  349,  350,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,  280,  281,  282,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,  339,  340,   -1,  342,
  343,  344,   -1,  346,  347,  348,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,  280,  281,  282,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,    0,   -1,  337,  338,  339,  340,   -1,  342,
  343,  344,   10,  346,  347,  348,  349,  350,  351,  352,
  353,  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,   -1,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,  339,  340,   -1,  342,  343,  344,   -1,  346,  347,
  348,  349,  350,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,    0,   -1,  337,
  338,  339,  340,   -1,  342,  343,  344,   10,  346,  347,
  348,  349,  350,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,  280,  281,  282,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,  339,  340,   -1,  342,
  343,  344,   -1,  346,  347,  348,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,  280,  281,  282,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,    0,   -1,  337,  338,  339,  340,   -1,  342,
  343,  344,   10,  346,  347,  348,  349,  350,  351,  352,
  353,  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   -1,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,   -1,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,  339,  340,   -1,  342,  343,  344,   -1,  346,  347,
  348,  349,  350,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,    0,   -1,  337,
  338,  339,  340,   -1,  342,  343,  344,   10,  346,  347,
  348,  349,  350,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   -1,   94,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  123,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,   -1,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,  280,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,  339,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,  262,
  263,  264,  265,  266,   -1,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,  280,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,    0,   -1,  337,  338,  339,  340,   -1,   -1,
  343,  344,   10,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   -1,   94,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
   38,   -1,   40,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   -1,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   44,  257,
  258,  259,   -1,  261,  262,  263,  264,  265,  266,   -1,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,  339,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,  349,  350,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,  262,  263,  264,  265,  266,   -1,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,    0,   -1,  337,
  338,  339,  340,   -1,   -1,  343,  344,   10,  346,  347,
   -1,  349,  350,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   33,  257,  258,  259,   -1,  261,   -1,   -1,   41,  265,
  266,   -1,   -1,   46,  270,   -1,  272,  273,  274,  275,
  276,  277,  278,   -1,   -1,   58,   59,  283,  284,  285,
  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,
  306,  307,  308,  309,  310,   -1,  312,  313,   -1,  315,
  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,  340,   -1,   -1,  343,  344,   -1,
  346,  347,  125,  126,   -1,  351,  352,  353,  354,  355,
   -1,   -1,   -1,   -1,  360,   33,   -1,   -1,   -1,   -1,
   -1,   -1,   40,   -1,   -1,   -1,   -1,   -1,   46,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   59,   60,   -1,    0,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   59,   -1,  123,   -1,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,  281,   -1,
  283,  284,  285,  286,  287,  288,  289,  290,  291,  125,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  339,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,   -1,   -1,   -1,  360,  256,  257,
  258,  259,  260,  261,  262,  263,  264,  265,  266,   -1,
   -1,  269,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,
  298,  299,  300,  301,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,  262,  263,  264,   10,
   -1,   -1,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,
   -1,  339,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,  349,   33,  351,  352,  353,  354,  355,   -1,   40,
   -1,   -1,  360,   -1,   -1,   46,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   91,   92,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  123,   -1,   -1,  126,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,
   -1,   -1,   -1,   -1,   40,   -1,   -1,   -1,   -1,   -1,
   46,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   59,   60,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,   -1,   47,   -1,   91,   92,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   62,   37,
   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,   -1,   -1,
  126,   -1,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,
   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  256,  257,  258,  259,  260,
  261,  262,  263,  264,  265,  266,   94,   -1,  269,  270,
  124,  272,  273,  274,  275,  276,  277,  278,   -1,  280,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,  292,   -1,   -1,   -1,   -1,  124,  298,  299,  300,
  301,  302,  303,  304,  305,  306,  307,  308,  309,  310,
   -1,  312,  313,   -1,  315,  316,  317,   -1,  319,  320,
  321,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  339,  340,
   -1,   -1,  343,  344,   -1,  346,  347,   -1,  349,   -1,
  351,  352,  353,  354,  355,   -1,   -1,   -1,   -1,  360,
  256,  257,  258,  259,  260,  261,  262,  263,  264,  265,
  266,   -1,   -1,  269,  270,   -1,  272,  273,  274,  275,
  276,  277,  278,   -1,  280,   -1,   -1,  283,  284,  285,
  286,  287,  288,  289,   -1,   -1,  292,   -1,   -1,   -1,
   -1,   -1,  298,  299,  300,  301,  302,  303,  304,  305,
  306,  307,  308,  309,  310,   -1,  312,  313,   -1,  315,
  316,  317,   -1,  319,  320,  321,   -1,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  339,  340,   -1,   -1,  343,  344,   -1,
  346,  347,   -1,  349,   33,  351,  352,  353,  354,  355,
   -1,   40,   -1,   -1,  360,   -1,   -1,   46,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,   -1,
   59,   60,   -1,  337,  338,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,   -1,   91,   92,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   33,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   46,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   59,   60,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,  258,
  259,  260,  261,  262,  263,  264,  265,  266,   -1,   -1,
  269,  270,   59,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,  298,
  299,  300,  301,  302,  303,  304,  305,  306,  307,  308,
  309,  310,   -1,  312,  313,   -1,  315,  316,  317,   -1,
  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,
  339,  340,   -1,   -1,  343,  344,   -1,  346,  347,   -1,
  349,   33,  351,  352,  353,  354,  355,   -1,   -1,   -1,
   -1,  360,  256,  257,  258,  259,  260,  261,  262,  263,
  264,  265,  266,   -1,   -1,  269,  270,   59,  272,  273,
  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,
  284,  285,  286,  287,  288,  289,   -1,   -1,  292,   -1,
   -1,   -1,   -1,   -1,  298,  299,  300,  301,  302,  303,
  304,  305,  306,  307,  308,  309,  310,   -1,  312,  313,
   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  126,  339,  340,   -1,   -1,  343,
  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,  353,
  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,  256,
  257,  258,  259,  260,  261,  262,  263,  264,  265,  266,
   -1,  268,  269,  270,  271,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,
   -1,  298,  299,  300,  301,  302,  303,  304,  305,  306,
  307,  308,  309,  310,   -1,  312,  313,   -1,  315,  316,
  317,   10,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  340,   33,   -1,  343,  344,   -1,  346,
  347,   -1,  349,   -1,  351,  352,  353,  354,  355,   -1,
   -1,   -1,   -1,  360,  256,  257,  258,  259,  260,  261,
   59,   -1,  264,  265,  266,   -1,   -1,   -1,  270,   -1,
  272,  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,
   -1,  283,  284,  285,  286,  287,  288,  289,   -1,   -1,
  292,   -1,   -1,   -1,   -1,   -1,  298,  299,  300,  301,
  302,  303,  304,  305,  306,  307,  308,  309,  310,   -1,
  312,  313,   -1,  315,  316,  317,   -1,  319,  320,  321,
   -1,   -1,    0,   -1,   -1,   -1,  125,  126,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,  340,   -1,
   -1,  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,
  352,  353,  354,  355,   -1,   -1,   -1,   -1,  360,   37,
   38,   -1,   -1,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,   -1,   10,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,  258,
  259,  260,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   41,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,  292,   -1,   -1,   -1,   58,   59,  298,
  299,  300,  301,  302,  303,  304,  305,  306,  307,  308,
  309,  310,   -1,  312,  313,   -1,  315,  316,  317,   -1,
  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  340,   -1,   10,  343,  344,   -1,  346,  347,   -1,
  349,   -1,  351,  352,  353,  354,  355,   -1,   -1,   -1,
   -1,  360,   -1,   -1,  125,   -1,   -1,   -1,   -1,    0,
   -1,   -1,   -1,   -1,   41,   -1,   -1,   -1,   -1,   10,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   59,  271,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  280,  281,  282,   -1,   -1,   -1,   -1,   -1,
   41,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   -1,
    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   10,   -1,   -1,   -1,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,  125,  337,
  338,  339,   -1,   -1,  342,   -1,   -1,   37,   38,   -1,
  348,   41,   42,   43,   44,   45,   46,   47,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,
   60,   61,   62,   63,  125,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,
  281,   91,   92,   93,   94,   -1,   -1,   -1,   -1,   10,
   -1,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  124,  125,   37,   38,   -1,   -1,
   41,   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,
   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
   -1,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,
   91,   92,   93,   94,    0,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,  293,  294,  295,  296,
  297,  262,  263,  264,   -1,   -1,   -1,  268,  269,   -1,
  271,   -1,  123,  124,  125,   -1,   -1,   -1,   -1,   -1,
   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,   45,
   46,   47,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   61,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   -1,   91,   92,   93,   94,   37,
   38,  281,  282,   -1,   42,   43,   -1,   45,   -1,   47,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   58,   -1,   60,   -1,   62,   63,   -1,   -1,  124,  125,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,  333,  334,   -1,   94,  337,  338,  339,
   -1,  341,  342,   -1,   -1,   -1,   -1,   -1,  348,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   -1,   -1,   -1,   -1,  124,   -1,   -1,  280,
  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   10,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,  333,  334,   -1,   -1,  337,  338,  339,   -1,
   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,   -1,   -1,
   -1,   41,   -1,   -1,   44,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   58,   59,
   -1,   -1,   -1,   63,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,    0,   93,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,  333,  334,   -1,
   -1,  337,  338,  339,   -1,  125,  342,   -1,   -1,   37,
   38,   -1,  348,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,    0,   -1,   91,   92,   93,   94,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,   37,   38,
   -1,   -1,   41,   42,   43,   44,   45,   46,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   91,   92,   93,   94,    0,   -1,   -1,   -1,
   -1,  281,  282,   -1,   -1,   -1,   10,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,   -1,   -1,
   -1,   -1,   -1,   37,   38,   -1,   -1,   41,   42,   43,
   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   -1,   91,   92,   93,
   94,   37,   38,  281,  282,   -1,   42,   43,   44,   45,
   -1,   47,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   60,   -1,   62,   63,   -1,   -1,
  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   94,  337,
  338,  339,   -1,   -1,  342,   -1,   -1,   -1,   -1,   -1,
  348,   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,  124,   -1,
   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,  322,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,  333,  334,   -1,   -1,  337,  338,
  339,   -1,   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,
   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   58,   59,   -1,   -1,   -1,   63,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,    0,   93,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,  339,   -1,  125,  342,   -1,
   -1,   37,   38,   -1,  348,   41,   42,   43,   44,   45,
   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,  333,  334,   -1,
   -1,  337,  338,    0,   -1,   91,   92,   93,   94,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,
   37,   38,   -1,   -1,   41,   42,   43,   44,   45,   46,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   91,   92,   93,   94,    0,   -1,
   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   10,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,
   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   41,
   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,  342,   -1,   -1,   -1,   -1,   -1,
  348,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,   91,
   92,   93,   94,   37,   38,  281,  282,   -1,   42,   43,
   -1,   45,   -1,   47,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   60,   -1,   62,   63,
   -1,   -1,  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,  333,  334,   -1,
   94,  337,  338,  339,   -1,   -1,  342,   -1,   -1,   -1,
   -1,   -1,  348,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,
  124,   37,   38,   -1,  281,  282,   42,   43,   -1,   45,
   -1,   47,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   60,   -1,   62,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,  333,  334,   94,   -1,
  337,  338,  339,   -1,   -1,  342,    0,   -1,   -1,   -1,
   -1,  348,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,  124,  271,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   41,  290,  291,
   44,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   59,    0,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,  333,  334,   -1,   -1,  337,  338,  339,   -1,   -1,
  342,   -1,   -1,   37,   38,   -1,  348,   41,   42,   43,
   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,  125,   -1,   -1,   -1,   -1,   -1,   -1,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,    0,   -1,   91,   92,   93,
   94,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  124,  125,   37,   38,   -1,   -1,   41,   42,   43,   44,
   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  322,  323,  324,  325,
  326,  327,  328,  329,   -1,  331,  332,   -1,   -1,   -1,
   -1,  337,  338,   -1,   -1,    0,   91,   92,   93,   94,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,   -1,  268,  269,   -1,  271,   -1,  124,
  125,   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,
   45,   -1,   47,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   93,   94,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,  124,
  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,  339,   -1,   -1,  342,   -1,
   -1,   -1,   -1,   -1,  348,   -1,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
   -1,   -1,   -1,   37,   38,   -1,  281,  282,   42,   43,
   -1,   45,   -1,   47,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   60,   -1,   62,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,  333,  334,
   94,   -1,  337,  338,  339,   -1,   -1,  342,   -1,   -1,
   -1,   -1,   -1,  348,   -1,   -1,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
  124,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,  333,  334,
   -1,   -1,  337,  338,   37,   38,   -1,  342,   41,   42,
   43,   44,   45,  348,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,   -1,
   93,   94,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  124,  125,   37,   38,   -1,   -1,   41,   42,   43,
   44,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  322,  323,
  324,  325,  326,  327,  328,   -1,    0,  331,  332,   93,
   94,   -1,   -1,  337,  338,   -1,   10,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  124,  125,   -1,   37,   38,   -1,   -1,   41,   42,   43,
   44,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   93,
   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,   -1,   -1,   -1,  342,
   -1,   -1,   -1,   -1,   -1,  348,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,   -1,   -1,   -1,  342,   -1,
   -1,   -1,   -1,   -1,  348,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,   37,   38,   -1,  342,   41,
   42,   43,   44,   45,  348,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   93,   94,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,  125,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   93,   94,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  124,  125,   -1,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,  124,  125,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,  333,  334,   -1,   -1,  337,  338,   -1,   -1,   -1,
  342,   38,   -1,   -1,   41,   -1,  348,   44,   -1,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,  281,  282,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,    0,   -1,   -1,   93,   94,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,   -1,  124,  125,  342,
   -1,   38,   -1,   -1,   41,  348,   43,   44,   45,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,  281,  282,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,    0,   -1,   -1,   93,   94,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,   -1,  124,  125,  342,
   -1,   38,   -1,   -1,   41,  348,   43,   44,   45,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   93,   94,   -1,   -1,
   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,    0,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,
   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,  333,  334,   -1,   -1,
  337,  338,   -1,   -1,   -1,  342,   -1,   38,   -1,   -1,
   41,  348,   -1,   44,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   58,   59,   60,
   -1,   62,   63,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   93,   94,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,  333,  334,   -1,   -1,
  337,  338,   -1,  124,  125,  342,   38,   -1,   -1,   41,
   -1,  348,   44,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   93,   94,   -1,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,  333,  334,   -1,   -1,
  337,  338,  124,  125,   -1,  342,   -1,   -1,   -1,   41,
   -1,  348,   44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,  124,  125,    0,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,  333,  334,   -1,   -1,  337,  338,   -1,   -1,
   -1,  342,   -1,   -1,   -1,   41,   -1,  348,   44,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,    0,   -1,   -1,   -1,   93,   94,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,  333,  334,   -1,   -1,   -1,   -1,   -1,  124,  125,
  342,   -1,   -1,   -1,   41,   -1,  348,   44,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,    0,   -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,  333,  334,   -1,   -1,   -1,   -1,   -1,  125,   -1,
  342,   -1,   41,   -1,   -1,   44,  348,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   93,  271,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,  125,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,  333,  334,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  342,   -1,   41,   -1,
   -1,   44,  348,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,
   93,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,  333,  334,   -1,   -1,
   -1,   -1,  125,   -1,   -1,  342,   41,   -1,   -1,   44,
   -1,  348,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,    0,   -1,   -1,   -1,   93,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,  333,  334,   -1,   -1,   -1,   -1,
  125,   -1,   -1,  342,   -1,   41,   -1,   -1,   44,  348,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   -1,   -1,   -1,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,
  263,  264,   -1,   -1,  267,  268,  269,   93,  271,   -1,
   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,  281,  282,
   -1,   -1,   -1,   -1,   -1,   10,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,  125,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   58,   59,   -1,   -1,   -1,   63,  342,
   -1,   -1,   -1,   -1,   -1,  348,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   93,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,
  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,  333,  334,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  342,   -1,   41,
   -1,   -1,   44,  348,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   58,   59,   -1,   -1,
   -1,   63,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   93,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  329,  330,   -1,   -1,  333,  334,   -1,
   -1,   -1,   -1,  125,   -1,   -1,  342,   -1,   -1,   -1,
   -1,   -1,  348,   41,   -1,   -1,   44,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,
   58,   59,  267,  268,  269,   63,  271,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,    0,   93,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  329,  330,   -1,  125,  333,  334,
   -1,   -1,   -1,   -1,   -1,   41,   -1,  342,   44,   -1,
   -1,   -1,   -1,  348,   -1,   -1,   -1,   -1,   -1,   -1,
    0,   -1,   58,   59,   -1,   -1,   -1,   63,   -1,   -1,
   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,  281,
  282,   41,   -1,   -1,   44,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   58,   59,
   -1,   -1,   -1,   63,   -1,   -1,   -1,   -1,   -1,  125,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  329,  330,   -1,
   -1,  333,  334,   93,    0,   -1,   -1,   -1,   -1,   -1,
  342,   -1,   -1,   -1,   10,   -1,  348,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,  125,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  281,  282,   41,   -1,   -1,   44,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   58,   59,   -1,   -1,   -1,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  329,  330,   -1,   -1,  333,  334,   93,   -1,   -1,
   -1,   -1,    0,   -1,  342,   -1,   -1,   -1,   -1,   -1,
  348,   -1,   10,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,  125,
   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   41,  290,  291,   44,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,  329,  330,   -1,   -1,  333,  334,    0,
   -1,  281,  282,   -1,   -1,   93,  342,   -1,   -1,   10,
  290,  291,  348,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,
   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,  329,
  330,   -1,   -1,  333,  334,   -1,   -1,   58,   59,   -1,
   -1,   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,
   -1,    0,   93,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   10,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  125,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   41,   -1,   -1,   44,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,  330,   -1,   -1,  333,  334,   58,
   59,   -1,   -1,   -1,   -1,   -1,  342,   -1,   -1,   -1,
   -1,   -1,  348,   -1,   -1,   -1,   -1,   -1,   41,   -1,
   -1,   44,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   93,   58,   59,   -1,   -1,    0,
   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   10,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,
   93,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   -1,
   -1,   -1,  125,   -1,  342,   -1,   -1,   -1,   -1,   -1,
  348,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,   -1,
  281,  282,   93,   -1,   -1,   -1,   10,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  125,   -1,   -1,   41,   -1,   -1,
   44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   -1,   -1,   -1,   -1,
   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,   -1,   -1,
   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,   93,
    0,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,
   10,  290,  291,   -1,  293,  294,  295,  296,  297,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,
   -1,  125,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,
   -1,   41,   -1,   -1,   44,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   58,   59,
   -1,   -1,   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,    0,   93,   -1,   -1,   -1,   -1,   -1,  342,
  281,  282,   10,   -1,   -1,  348,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   41,   -1,    0,   44,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   58,   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  342,   -1,   -1,   -1,   -1,   -1,  348,  262,  263,
  264,   -1,   -1,  267,  268,  269,   41,  271,   -1,   44,
    0,   -1,   -1,   -1,   -1,   93,   -1,  281,  282,   -1,
   10,   -1,   -1,   58,   59,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,
   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,   93,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,  342,   -1,
   -1,   -1,   10,   -1,  348,   -1,   -1,   -1,   -1,   -1,
  125,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  281,  282,   41,   -1,   -1,   44,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   58,   59,   -1,   -1,   -1,  125,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,    0,   93,   -1,   -1,   -1,   -1,
   -1,   -1,  342,   -1,   10,   -1,   -1,   -1,  348,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   -1,  125,   -1,   -1,
   -1,   -1,   -1,  281,  282,   41,   -1,   -1,   44,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   58,   59,   -1,   -1,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,    0,   -1,  290,  291,   93,  293,  294,
  295,  296,  297,   10,  342,   -1,   -1,   -1,   -1,   -1,
  348,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,
   -1,  281,  282,   -1,   41,   -1,   -1,   44,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  342,   -1,   -1,
   -1,   58,   59,  348,   -1,    0,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   93,   -1,   -1,   -1,
   -1,   -1,  342,  281,  282,   -1,   41,   -1,  348,   44,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   58,   59,   -1,   -1,   -1,  125,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   93,   -1,
   -1,   -1,   -1,   -1,  342,   -1,   10,   -1,   -1,   -1,
  348,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,
  125,   -1,   -1,   37,   38,  281,  282,   41,   42,   43,
   44,   45,   46,   47,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   -1,
   94,   -1,   -1,   -1,   -1,   -1,  342,   -1,   -1,   -1,
   -1,   -1,  348,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,    0,
  124,  125,   -1,   -1,  281,  282,   -1,   -1,   -1,   10,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,
   41,   42,   43,   44,   45,   46,   47,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   58,   59,   60,
   -1,   62,   63,   -1,   -1,  342,  281,  282,   -1,   -1,
   -1,  348,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   91,   92,   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   -1,  124,  125,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,  348,   -1,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,   -1,   41,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   58,   59,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   93,   -1,  337,  338,  339,   -1,   -1,  342,   37,
   38,   -1,   -1,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,  125,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   -1,   91,   92,   -1,   94,   -1,   -1,   -1,
  281,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,  124,  125,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,  333,  334,   -1,   -1,  337,  338,  339,   37,
   38,  342,   -1,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   -1,   91,   92,   -1,   94,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,    0,   -1,  124,  125,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  281,   -1,   41,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   58,   59,   -1,    0,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   93,   -1,  337,
  338,  339,   37,   38,  342,   -1,   41,   42,   43,   44,
   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   59,   60,   61,   62,   63,  125,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,   -1,
  268,  269,   -1,  271,   -1,   -1,   91,   92,   -1,   94,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,  124,
  125,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,  333,  334,   -1,   -1,  337,
  338,  339,   -1,  341,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   91,   92,
   -1,   94,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,
   -1,  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,
   -1,   -1,   -1,  268,  269,   -1,  271,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,  333,  334,
  126,   -1,  337,  338,  339,   37,   38,   -1,   -1,   41,
   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,   61,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,
  263,  264,   -1,   -1,   -1,  268,  269,   -1,  271,   91,
   92,   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,  125,   -1,   33,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
  333,  334,   -1,   -1,  337,  338,  339,   -1,   -1,   -1,
  256,  257,  258,  259,  260,  261,   -1,   -1,   -1,  265,
  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,
  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,
  286,  287,  288,  289,   -1,   -1,  292,   -1,   -1,   -1,
   -1,   -1,  298,  299,  300,  301,  302,  303,  304,  305,
  306,  307,  308,  309,  310,   -1,  312,  313,  126,  315,
  316,  317,   -1,  319,  320,  321,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  340,   33,   -1,  343,  344,   -1,
  346,  347,   -1,  349,   -1,  351,  352,  353,  354,  355,
  262,  263,  264,   -1,  360,   -1,  268,  269,   -1,  271,
   -1,   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,  333,  334,   -1,   -1,  337,  338,  339,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,
  258,  259,  260,  261,   -1,   33,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,
  298,  299,  300,  301,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,  349,   -1,  351,  352,  353,  354,  355,  126,   -1,
   -1,   -1,  360,   -1,   -1,   33,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   41,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,  260,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,
  298,  299,  300,  301,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,
   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   33,  349,   -1,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,   -1,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,  260,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,
  298,  299,  300,  301,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,  349,   -1,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,  349,  350,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,   -1,  257,  258,  259,   -1,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,  292,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,  292,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,   -1,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,  350,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,  292,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,   -1,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,   -1,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,  340,   -1,   -1,
  343,  344,   40,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,  257,  258,  259,  360,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,   -1,  312,
  313,   -1,  315,  316,  317,   -1,  319,  320,  321,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,  340,   -1,   -1,
  343,  344,   -1,  346,  347,   -1,  349,   -1,  351,  352,
  353,  354,  355,   -1,   -1,   -1,   -1,  360,   -1,  257,
  258,  259,   -1,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   -1,
   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,   -1,   -1,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   44,   -1,   -1,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,   -1,   -1,  351,  352,  353,  354,  355,   -1,  257,
  258,  259,  360,  261,   -1,   -1,   -1,  265,  266,   -1,
   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,  277,
  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,   -1,  312,  313,   -1,  315,  316,  317,
   -1,  319,  320,  321,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,  347,
   -1,   -1,   -1,  351,  352,  353,  354,  355,   -1,   -1,
   -1,   -1,  360,  257,  258,  259,   -1,  261,   -1,   -1,
   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,
  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,
  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,
  304,  305,  306,  307,  308,  309,  310,   -1,  312,  313,
   -1,  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,
   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,
   -1,   47,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,
  344,   -1,  346,  347,   60,   -1,   62,  351,  352,  353,
  354,  355,   -1,   -1,   -1,   -1,  360,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   94,   -1,
   96,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,
  126,   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,
   -1,   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   60,
   -1,   62,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,    0,   94,   -1,   96,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  124,   -1,  126,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   41,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  257,  258,  259,  260,  261,  262,  263,  264,   -1,
   -1,  267,  268,  269,  270,  271,   -1,   -1,  274,  275,
  276,  277,  278,  279,  280,   93,   -1,  283,  284,  285,
  286,  287,  288,  289,  290,  291,  292,  293,  294,  295,
  296,  297,  298,  299,  300,  301,  302,  303,  304,  305,
  306,   -1,  308,   -1,   -1,   -1,   -1,  125,   -1,   -1,
   -1,   -1,   -1,  319,  320,   -1,  322,  323,  324,  325,
   -1,  327,  328,   -1,   -1,  331,   -1,   -1,   -1,  335,
  336,  337,  338,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  349,   -1,  351,  257,  258,  259,  260,
  261,  262,  263,  264,   -1,   -1,  267,  268,  269,  270,
  271,   -1,   -1,  274,  275,  276,  277,  278,  279,  280,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,  290,
  291,  292,  293,  294,  295,  296,  297,  298,  299,  300,
  301,  302,  303,  304,  305,   -1,   -1,  308,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,  319,  320,
   -1,  322,  323,  324,  325,   -1,  327,  328,   -1,   -1,
  331,   -1,   -1,   -1,  335,  336,  337,  338,   -1,   37,
   38,   -1,   40,   -1,   42,   43,   -1,   45,  349,   47,
  351,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   60,  271,   62,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   94,   -1,   96,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,   -1,
   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  257,  258,  259,   -1,  261,   60,   -1,   62,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   94,
   -1,   96,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,   -1,  312,  313,   -1,  315,  316,
  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,   -1,  124,
   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  340,   -1,   -1,  343,  344,   -1,  346,
  347,   -1,  349,  350,  351,  352,  353,  354,  355,   -1,
   -1,   -1,   -1,  360,   -1,   -1,   -1,   -1,   -1,  257,
  258,  259,  260,  261,  262,  263,  264,   -1,   -1,  267,
  268,  269,  270,  271,   -1,   -1,  274,  275,  276,  277,
  278,  279,  280,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,  290,  291,  292,  293,  294,  295,  296,  297,
  298,  299,  300,  301,  302,  303,  304,  305,  306,  307,
  308,  309,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  319,  320,   -1,  322,  323,  324,  325,   -1,  327,
  328,   -1,   -1,  331,   -1,   -1,   -1,  335,  336,  337,
  338,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  349,  257,  258,  259,  260,  261,  262,  263,  264,
   -1,   -1,  267,  268,  269,  270,  271,   -1,   -1,  274,
  275,  276,  277,  278,  279,  280,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,  290,  291,  292,  293,  294,
  295,  296,  297,  298,  299,  300,  301,  302,  303,  304,
  305,  306,  307,  308,  309,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  319,  320,   -1,  322,  323,  324,
  325,   -1,  327,  328,   -1,   -1,  331,   -1,   -1,   -1,
  335,  336,  337,  338,   -1,   37,   38,   -1,   -1,   -1,
   42,   43,   -1,   45,  349,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,
   62,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   94,   60,   96,   62,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,
   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,  124,   -1,  126,   -1,   -1,   94,   60,   96,
   62,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,
   -1,   -1,   94,   60,   96,   62,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,   -1,  126,   -1,   -1,   94,   -1,   96,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  257,  258,  259,  260,  261,
  262,  263,  264,   -1,   -1,  267,  268,  269,  270,  271,
   -1,   -1,  274,  275,  276,  277,  278,  279,  280,   -1,
   -1,  283,  284,  285,  286,  287,  288,  289,  290,  291,
  292,  293,  294,  295,  296,  297,  298,  299,  300,  301,
  302,  303,  304,  305,   -1,   -1,  308,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  319,  320,   -1,
  322,  323,  324,  325,   -1,  327,  328,   -1,   -1,  331,
   -1,   -1,   -1,  335,  336,  337,  338,  304,  305,   -1,
   -1,  308,   -1,   -1,   -1,   -1,   -1,  349,   -1,   -1,
   -1,   -1,  319,  320,   -1,  322,  323,  324,  325,   -1,
  327,  328,   -1,   -1,  331,   -1,   -1,   -1,  335,  336,
  337,  338,  304,  305,   -1,   -1,  308,   -1,   -1,   -1,
   -1,   -1,  349,   -1,   -1,   -1,   -1,  319,  320,   -1,
  322,  323,  324,  325,   -1,  327,  328,   -1,   -1,  331,
   -1,   -1,   -1,  335,  336,  337,  338,  304,  305,   -1,
   -1,  308,   -1,   -1,   -1,   -1,   -1,  349,   -1,   -1,
   -1,   -1,  319,  320,   -1,  322,  323,  324,  325,   -1,
  327,  328,   -1,   -1,  331,   -1,   -1,   -1,  335,  336,
  337,  338,   -1,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,  349,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   62,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,
   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   94,   60,   96,   62,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
  124,   -1,  126,   -1,   -1,   94,   60,   96,   62,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,
   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,   -1,   -1,
   94,   60,   96,   62,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
  124,   -1,  126,   -1,   -1,   94,   60,   96,   62,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,
   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,   -1,   -1,
   94,   60,   96,   62,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,
  124,   47,  126,   -1,   -1,   94,   -1,   96,   -1,   -1,
   -1,   -1,   -1,   -1,   60,   -1,   62,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,   -1,   -1,
  304,  305,   -1,   -1,  308,   -1,   -1,   -1,   94,   -1,
   96,   -1,   -1,   -1,   -1,  319,  320,   -1,  322,  323,
  324,  325,   -1,  327,  328,   -1,   -1,  331,   -1,   -1,
   -1,  335,  336,  337,  338,  304,  305,   -1,  124,  308,
  126,   -1,   -1,   -1,   -1,  349,   -1,   -1,   -1,   -1,
  319,  320,   -1,  322,  323,  324,  325,   -1,  327,  328,
   -1,   -1,  331,   -1,   -1,   -1,  335,  336,  337,  338,
  304,  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,   -1,
  349,   -1,   -1,   -1,   -1,  319,  320,   -1,  322,  323,
  324,  325,   -1,  327,  328,   -1,   -1,  331,   -1,   -1,
   -1,  335,  336,  337,  338,  304,  305,   -1,   -1,  308,
   -1,   -1,   -1,   -1,   -1,  349,   -1,   -1,   -1,   -1,
  319,  320,   -1,  322,  323,  324,  325,   -1,  327,  328,
   -1,   -1,  331,   -1,   -1,   -1,  335,  336,  337,  338,
  304,  305,    0,   -1,  308,   -1,   -1,   -1,   -1,   -1,
  349,   -1,   10,   -1,   -1,  319,  320,   -1,  322,  323,
  324,  325,   -1,  327,  328,   -1,   -1,  331,   -1,   -1,
   -1,  335,  336,  337,  338,  304,  305,   -1,   -1,  308,
   -1,   -1,   -1,   41,   -1,  349,   -1,   -1,   -1,   -1,
  319,  320,   -1,  322,  323,  324,  325,   -1,  327,  328,
   58,   59,  331,   -1,   -1,   -1,  335,  336,  337,  338,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  304,  305,
  349,   -1,  308,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  319,  320,   -1,  322,  323,  324,  325,
   -1,  327,  328,   -1,   -1,  331,   -1,   -1,   -1,  335,
  336,  337,  338,   -1,   37,   38,   -1,   -1,   -1,   42,
   43,   -1,   45,  349,   47,   -1,   -1,  125,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   62,
   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   62,   63,
   -1,   94,   -1,   96,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,    0,   -1,   -1,   -1,   -1,   -1,
   94,  124,   -1,  126,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   41,   -1,   -1,   -1,   -1,   -1,
  124,   -1,   -1,   -1,   -1,   41,   -1,   -1,   -1,   -1,
   -1,   58,   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  281,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  304,  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  319,  320,   -1,  322,
  323,  324,  325,  297,  327,  328,   -1,   -1,  331,   -1,
   -1,   -1,  335,  336,  337,  338,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  349,   -1,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,
  334,   -1,   -1,  337,  338,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,  281,  271,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,  281,  293,  294,  295,  296,
  297,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,  257,  258,  259,   -1,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,  349,  350,  351,  352,  353,  354,
  355,   -1,  257,  258,  259,  360,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  337,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,   -1,   -1,  351,  352,  353,  354,
  355,   -1,  257,  258,  259,  360,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,  349,   -1,  351,  352,  353,  354,
  355,   -1,  257,  258,  259,  360,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,  349,   -1,  351,  352,  353,  354,
  355,   -1,  257,  258,  259,  360,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,   -1,   -1,  351,  352,  353,  354,
  355,   -1,  257,  258,  259,  360,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,   -1,   -1,  351,  352,  353,  354,
  355,   -1,  257,  258,  259,  360,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,   -1,  312,  313,   -1,
  315,  316,  317,   -1,   -1,   -1,  321,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  343,  344,
   -1,  346,  347,   -1,   -1,   -1,  351,  352,  353,  354,
  355,   -1,   -1,   -1,   -1,  360,
};
enum { YYTABLESIZE = 20546 };
enum { YYFINAL = 1 };
#ifndef YYDEBUG
#define YYDEBUG 1
#endif
enum { YYMAXTOKEN = 361 };
#if YYDEBUG
static const char *yyname[] = {

"end-of-file",0,0,0,0,0,0,0,0,0,"'\\n'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,"' '","'!'",0,0,0,"'%'","'&'",0,"'('","')'","'*'","'+'","','","'-'","'.'",
"'/'",0,0,0,0,0,0,0,0,0,0,"':'","';'","'<'","'='","'>'","'?'",0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'['","'\\\\'","']'","'^'",0,"'`'",0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"'{'","'|'","'}'","'~'",0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,"kCLASS","kMODULE","kDEF","kUNDEF","kBEGIN","kRESCUE","kENSURE","kEND",
"kIF","kUNLESS","kTHEN","kELSIF","kELSE","kCASE","kWHEN","kWHILE","kUNTIL",
"kFOR","kBREAK","kNEXT","kREDO","kRETRY","kIN","kDO","kDO_COND","kDO_BLOCK",
"kRETURN","kYIELD","kSUPER","kSELF","kNIL","kTRUE","kFALSE","kAND","kOR","kNOT",
"kIF_MOD","kUNLESS_MOD","kWHILE_MOD","kUNTIL_MOD","kRESCUE_MOD","kALIAS",
"kDEFINED","klBEGIN","klEND","k__LINE__","k__FILE__","tIDENTIFIER","tFID",
"tGVAR","tIVAR","tCONSTANT","tCVAR","tXSTRING_BEG","tLABEL","tINTEGER","tFLOAT",
"tSTRING_CONTENT","tCHAR","tNTH_REF","tBACK_REF","tREGEXP_END","tUPLUS",
"tUMINUS","tUBS","tPOW","tCMP","tEQ","tEQQ","tNEQ","tGEQ","tLEQ","tANDOP",
"tOROP","tMATCH","tNMATCH","tDOT2","tDOT3","tAREF","tASET","tLSHFT","tRSHFT",
"tCOLON2","tCOLON3","tOP_ASGN","tASSOC","tLPAREN","tLPAREN_ARG","tRPAREN",
"tLBRACK","tLBRACE","tLBRACE_ARG","tSTAR","tAMPER","tSYMBEG","tSTRING_BEG",
"tREGEXP_BEG","tWORDS_BEG","tQWORDS_BEG","tSTRING_DBEG","tSTRING_DVAR",
"tSTRING_END","tLOWEST","tUMINUS_NUM","tLAST_TOKEN",
};
static const char *yyrule[] = {
"$accept : program",
"$$1 :",
"program : $$1 compstmt",
"bodystmt : compstmt opt_rescue opt_else opt_ensure",
"compstmt : stmts opt_terms",
"stmts : none",
"stmts : stmt",
"stmts : stmts terms stmt",
"stmts : error stmt",
"$$2 :",
"stmt : kALIAS fitem $$2 fitem",
"stmt : kALIAS tGVAR tGVAR",
"stmt : kALIAS tGVAR tBACK_REF",
"stmt : kALIAS tGVAR tNTH_REF",
"stmt : kUNDEF undef_list",
"stmt : stmt kIF_MOD expr_value",
"stmt : stmt kUNLESS_MOD expr_value",
"stmt : stmt kWHILE_MOD expr_value",
"stmt : stmt kUNTIL_MOD expr_value",
"stmt : stmt kRESCUE_MOD stmt",
"$$3 :",
"stmt : klBEGIN $$3 '{' compstmt '}'",
"stmt : klEND '{' compstmt '}'",
"stmt : lhs '=' command_call",
"stmt : mlhs '=' command_call",
"stmt : var_lhs tOP_ASGN command_call",
"stmt : primary_value ary_ref tOP_ASGN command_call",
"stmt : primary_value '.' tIDENTIFIER tOP_ASGN command_call",
"stmt : primary_value '.' tCONSTANT tOP_ASGN command_call",
"stmt : primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call",
"stmt : backref tOP_ASGN command_call",
"stmt : lhs '=' mrhs",
"stmt : mlhs '=' arg_value",
"stmt : mlhs '=' mrhs",
"stmt : expr",
"expr : command_call",
"expr : expr kAND expr",
"expr : expr kOR expr",
"expr : kNOT expr",
"expr : '!' command_call",
"expr : arg",
"expr_value : expr",
"command_call : command",
"command_call : block_command",
"command_call : kRETURN call_args",
"command_call : kBREAK call_args",
"command_call : kNEXT call_args",
"block_command : block_call",
"block_command : block_call '.' operation2 command_args",
"block_command : block_call tCOLON2 operation2 command_args",
"$$4 :",
"$$5 :",
"cmd_brace_block : tLBRACE_ARG $$4 opt_block_var $$5 compstmt '}'",
"command : operation command_args",
"command : operation command_args cmd_brace_block",
"command : primary_value '.' operation2 command_args",
"command : primary_value '.' operation2 command_args cmd_brace_block",
"command : primary_value tCOLON2 operation2 command_args",
"command : primary_value tCOLON2 operation2 command_args cmd_brace_block",
"command : kSUPER command_args",
"command : kYIELD command_args",
"mlhs : mlhs_basic",
"mlhs : tLPAREN mlhs_entry ')'",
"mlhs_entry : mlhs_basic",
"mlhs_entry : tLPAREN mlhs_entry ')'",
"mlhs_basic : mlhs_head",
"mlhs_basic : mlhs_head mlhs_item",
"mlhs_basic : mlhs_head tSTAR mlhs_node",
"mlhs_basic : mlhs_head tSTAR",
"mlhs_basic : tSTAR mlhs_node",
"mlhs_basic : tSTAR",
"mlhs_item : mlhs_node",
"mlhs_item : tLPAREN mlhs_entry ')'",
"mlhs_head : mlhs_item ','",
"mlhs_head : mlhs_head mlhs_item ','",
"ary_ref : '[' aref_args ']'",
"mlhs_node : variable",
"mlhs_node : primary_value ary_ref",
"mlhs_node : primary_value '.' tIDENTIFIER",
"mlhs_node : primary_value tCOLON2 tIDENTIFIER",
"mlhs_node : primary_value '.' tCONSTANT",
"mlhs_node : primary_value tCOLON2 tCONSTANT",
"mlhs_node : tCOLON3 tCONSTANT",
"mlhs_node : backref",
"lhs : variable",
"lhs : primary_value ary_ref",
"lhs : primary_value '.' tIDENTIFIER",
"lhs : primary_value tCOLON2 tIDENTIFIER",
"lhs : primary_value '.' tCONSTANT",
"lhs : primary_value tCOLON2 tCONSTANT",
"lhs : tCOLON3 tCONSTANT",
"lhs : backref",
"cname : tIDENTIFIER",
"cname : tCONSTANT",
"cpath : tCOLON3 cname",
"cpath : cname",
"cpath : primary_value tCOLON2 cname",
"fname : tIDENTIFIER",
"fname : tCONSTANT",
"fname : tFID",
"fname : op",
"fname : reswords",
"fitem : fname",
"fitem : symbol",
"fitem : dsym",
"undef_list : fitem",
"$$6 :",
"undef_list : undef_list ',' $$6 fitem",
"op : '|'",
"op : '^'",
"op : '&'",
"op : tCMP",
"op : tEQ",
"op : tEQQ",
"op : tMATCH",
"op : '>'",
"op : tGEQ",
"op : '<'",
"op : tLEQ",
"op : tLSHFT",
"op : tRSHFT",
"op : '+'",
"op : '-'",
"op : '*'",
"op : tSTAR",
"op : '/'",
"op : '%'",
"op : tPOW",
"op : '~'",
"op : tUPLUS",
"op : tUMINUS",
"op : tAREF",
"op : tASET",
"op : '`'",
"reswords : k__LINE__",
"reswords : k__FILE__",
"reswords : klBEGIN",
"reswords : klEND",
"reswords : kALIAS",
"reswords : kAND",
"reswords : kBEGIN",
"reswords : kBREAK",
"reswords : kCASE",
"reswords : kCLASS",
"reswords : kDEF",
"reswords : kDEFINED",
"reswords : kDO",
"reswords : kELSE",
"reswords : kELSIF",
"reswords : kEND",
"reswords : kENSURE",
"reswords : kFALSE",
"reswords : kFOR",
"reswords : kIN",
"reswords : kMODULE",
"reswords : kNEXT",
"reswords : kNIL",
"reswords : kNOT",
"reswords : kOR",
"reswords : kREDO",
"reswords : kRESCUE",
"reswords : kRETRY",
"reswords : kRETURN",
"reswords : kSELF",
"reswords : kSUPER",
"reswords : kTHEN",
"reswords : kTRUE",
"reswords : kUNDEF",
"reswords : kWHEN",
"reswords : kYIELD",
"reswords : kIF_MOD",
"reswords : kUNLESS_MOD",
"reswords : kWHILE_MOD",
"reswords : kUNTIL_MOD",
"reswords : kRESCUE_MOD",
"arg : lhs '=' arg",
"arg : lhs '=' arg kRESCUE_MOD arg",
"arg : var_lhs tOP_ASGN arg",
"arg : primary_value ary_ref tOP_ASGN arg",
"arg : primary_value '.' tIDENTIFIER tOP_ASGN arg",
"arg : primary_value '.' tCONSTANT tOP_ASGN arg",
"arg : primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg",
"arg : primary_value tCOLON2 tCONSTANT tOP_ASGN arg",
"arg : tCOLON3 tCONSTANT tOP_ASGN arg",
"arg : backref tOP_ASGN arg",
"arg : arg tDOT2 arg",
"arg : arg tDOT3 arg",
"arg : arg '+' arg",
"arg : arg '-' arg",
"arg : arg '*' arg",
"arg : arg '/' arg",
"arg : arg '%' arg",
"arg : arg tPOW arg",
"arg : tUMINUS_NUM tINTEGER tPOW arg",
"arg : tUMINUS_NUM tFLOAT tPOW arg",
"arg : tUPLUS arg",
"arg : tUMINUS arg",
"arg : arg '|' arg",
"arg : arg '^' arg",
"arg : arg '&' arg",
"arg : arg tCMP arg",
"arg : arg '>' arg",
"arg : arg tGEQ arg",
"arg : arg '<' arg",
"arg : arg tLEQ arg",
"arg : arg tEQ arg",
"arg : arg tEQQ arg",
"arg : arg tNEQ arg",
"arg : arg tMATCH arg",
"arg : arg tNMATCH arg",
"arg : '!' arg",
"arg : '~' arg",
"arg : arg tLSHFT arg",
"arg : arg tRSHFT arg",
"arg : arg tANDOP arg",
"arg : arg tOROP arg",
"$$7 :",
"arg : kDEFINED opt_nl $$7 arg",
"$$8 :",
"arg : arg '?' $$8 arg ':' arg",
"arg : primary",
"arg_value : arg",
"aref_args : none",
"aref_args : command opt_nl",
"aref_args : args trailer",
"aref_args : args ',' tSTAR arg opt_nl",
"aref_args : assocs trailer",
"aref_args : tSTAR arg opt_nl",
"paren_args : '(' none ')'",
"paren_args : '(' call_args opt_nl ')'",
"paren_args : '(' block_call opt_nl ')'",
"paren_args : '(' args ',' block_call opt_nl ')'",
"opt_paren_args : none",
"opt_paren_args : paren_args",
"call_args : command",
"call_args : args opt_block_arg",
"call_args : args ',' tSTAR arg_value opt_block_arg",
"call_args : assocs opt_block_arg",
"call_args : assocs ',' tSTAR arg_value opt_block_arg",
"call_args : args ',' assocs opt_block_arg",
"call_args : args ',' assocs ',' tSTAR arg opt_block_arg",
"call_args : tSTAR arg_value opt_block_arg",
"call_args : block_arg",
"call_args2 : arg_value ',' args opt_block_arg",
"call_args2 : arg_value ',' block_arg",
"call_args2 : arg_value ',' tSTAR arg_value opt_block_arg",
"call_args2 : arg_value ',' args ',' tSTAR arg_value opt_block_arg",
"call_args2 : assocs opt_block_arg",
"call_args2 : assocs ',' tSTAR arg_value opt_block_arg",
"call_args2 : arg_value ',' assocs opt_block_arg",
"call_args2 : arg_value ',' args ',' assocs opt_block_arg",
"call_args2 : arg_value ',' assocs ',' tSTAR arg_value opt_block_arg",
"call_args2 : arg_value ',' args ',' assocs ',' tSTAR arg_value opt_block_arg",
"call_args2 : tSTAR arg_value opt_block_arg",
"call_args2 : block_arg",
"$$9 :",
"command_args : $$9 open_args",
"open_args : call_args",
"$$10 :",
"open_args : tLPAREN_ARG $$10 ')'",
"$$11 :",
"open_args : tLPAREN_ARG call_args2 $$11 ')'",
"block_arg : tAMPER arg_value",
"opt_block_arg : ',' block_arg",
"opt_block_arg : none",
"args : arg_value",
"args : args ',' arg_value",
"mrhs : args ',' arg_value",
"mrhs : args ',' tSTAR arg_value",
"mrhs : tSTAR arg_value",
"primary : literal",
"primary : strings",
"primary : xstring",
"primary : regexp",
"primary : words",
"primary : qwords",
"primary : var_ref",
"primary : backref",
"primary : tFID",
"$$12 :",
"primary : kBEGIN $$12 bodystmt kEND",
"$$13 :",
"primary : tLPAREN_ARG expr $$13 opt_nl ')'",
"primary : tLPAREN compstmt ')'",
"primary : primary_value tCOLON2 tCONSTANT",
"primary : tCOLON3 tCONSTANT",
"primary : primary_value ary_ref",
"primary : tLBRACK aref_args ']'",
"primary : tLBRACE assoc_list '}'",
"primary : kRETURN",
"primary : kYIELD '(' call_args ')'",
"primary : kYIELD '(' ')'",
"primary : kYIELD",
"$$14 :",
"primary : kDEFINED opt_nl '(' $$14 expr ')'",
"primary : operation brace_block",
"primary : method_call",
"primary : method_call brace_block",
"$$15 :",
"primary : kIF $$15 expr_value then compstmt if_tail kEND",
"$$16 :",
"primary : kUNLESS $$16 expr_value then compstmt opt_else kEND",
"$$17 :",
"$$18 :",
"primary : kWHILE $$17 expr_value do $$18 compstmt kEND",
"$$19 :",
"$$20 :",
"primary : kUNTIL $$19 expr_value do $$20 compstmt kEND",
"$$21 :",
"primary : kCASE $$21 expr_value opt_terms case_body kEND",
"$$22 :",
"primary : kCASE opt_terms $$22 case_body kEND",
"$$23 :",
"primary : kCASE opt_terms $$23 kELSE compstmt kEND",
"$$24 :",
"$$25 :",
"$$26 :",
"primary : kFOR $$24 for_var kIN $$25 expr_value do $$26 compstmt kEND",
"$$27 :",
"primary : kCLASS cpath superclass $$27 bodystmt kEND",
"$$28 :",
"$$29 :",
"primary : kCLASS tLSHFT expr $$28 term $$29 bodystmt kEND",
"$$30 :",
"primary : kMODULE cpath $$30 bodystmt kEND",
"$$31 :",
"primary : kDEF fname $$31 f_arglist bodystmt kEND",
"$$32 :",
"$$33 :",
"primary : kDEF singleton dot_or_colon $$32 fname $$33 f_arglist bodystmt kEND",
"primary : kBREAK",
"primary : kNEXT",
"primary : kREDO",
"primary : kRETRY",
"primary_value : primary",
"then : term",
"then : ':'",
"then : kTHEN",
"then : term kTHEN",
"do : term",
"do : ':'",
"do : kDO_COND",
"if_tail : opt_else",
"if_tail : kELSIF expr_value then compstmt if_tail",
"opt_else : none",
"opt_else : kELSE compstmt",
"for_var : lhs",
"for_var : mlhs",
"block_par : mlhs_item",
"block_par : block_par ',' mlhs_item",
"blck_var : block_par",
"blck_var : block_par ','",
"blck_var : block_par ',' tAMPER lhs",
"blck_var : block_par ',' tSTAR lhs ',' tAMPER lhs",
"blck_var : block_par ',' tSTAR ',' tAMPER lhs",
"blck_var : block_par ',' tSTAR lhs",
"blck_var : block_par ',' tSTAR",
"blck_var : tSTAR lhs ',' tAMPER lhs",
"blck_var : tSTAR ',' tAMPER lhs",
"blck_var : tSTAR lhs",
"blck_var : tSTAR",
"blck_var : tAMPER lhs",
"opt_block_var : none",
"opt_block_var : '|' '|'",
"opt_block_var : tOROP",
"opt_block_var : '|' blck_var '|'",
"$$34 :",
"$$35 :",
"do_block : kDO_BLOCK $$34 opt_block_var $$35 compstmt kEND",
"block_call : command do_block",
"block_call : block_call '.' operation2 opt_paren_args",
"block_call : block_call tCOLON2 operation2 opt_paren_args",
"method_call : operation paren_args",
"method_call : primary_value '.' operation2 opt_paren_args",
"method_call : primary_value tCOLON2 operation2 paren_args",
"method_call : primary_value tCOLON2 operation3",
"method_call : primary_value '\\\\' operation2",
"method_call : tUBS operation2",
"method_call : kSUPER paren_args",
"method_call : kSUPER",
"$$36 :",
"$$37 :",
"brace_block : '{' $$36 opt_block_var $$37 compstmt '}'",
"$$38 :",
"$$39 :",
"brace_block : kDO $$38 opt_block_var $$39 compstmt kEND",
"case_body : kWHEN when_args then compstmt cases",
"when_args : args",
"when_args : args ',' tSTAR arg_value",
"when_args : tSTAR arg_value",
"cases : opt_else",
"cases : case_body",
"opt_rescue : kRESCUE exc_list exc_var then compstmt opt_rescue",
"opt_rescue : none",
"exc_list : arg_value",
"exc_list : mrhs",
"exc_list : none",
"exc_var : tASSOC lhs",
"exc_var : none",
"opt_ensure : kENSURE compstmt",
"opt_ensure : none",
"literal : numeric",
"literal : symbol",
"literal : dsym",
"strings : string",
"string : tCHAR",
"string : string1",
"string : string string1",
"string1 : tSTRING_BEG string_contents tSTRING_END",
"xstring : tXSTRING_BEG xstring_contents tSTRING_END",
"regexp : tREGEXP_BEG xstring_contents tREGEXP_END",
"words : tWORDS_BEG ' ' tSTRING_END",
"words : tWORDS_BEG word_list tSTRING_END",
"word_list :",
"word_list : word_list word ' '",
"word : string_content",
"word : word string_content",
"qwords : tQWORDS_BEG ' ' tSTRING_END",
"qwords : tQWORDS_BEG qword_list tSTRING_END",
"qword_list :",
"qword_list : qword_list tSTRING_CONTENT ' '",
"string_contents :",
"string_contents : string_contents string_content",
"xstring_contents :",
"xstring_contents : xstring_contents string_content",
"string_content : tSTRING_CONTENT",
"$$40 :",
"string_content : tSTRING_DVAR $$40 string_dvar",
"$$41 :",
"string_content : tSTRING_DBEG $$41 compstmt '}'",
"string_dvar : tGVAR",
"string_dvar : tIVAR",
"string_dvar : tCVAR",
"string_dvar : backref",
"symbol : tSYMBEG sym",
"sym : fname",
"sym : tIVAR",
"sym : tGVAR",
"sym : tCVAR",
"dsym : tSYMBEG xstring_contents tSTRING_END",
"numeric : tINTEGER",
"numeric : tFLOAT",
"numeric : tUMINUS_NUM tINTEGER",
"numeric : tUMINUS_NUM tFLOAT",
"variable : tIDENTIFIER",
"variable : tIVAR",
"variable : tGVAR",
"variable : tCONSTANT",
"variable : tCVAR",
"variable : kNIL",
"variable : kSELF",
"variable : kTRUE",
"variable : kFALSE",
"variable : k__FILE__",
"variable : k__LINE__",
"var_ref : variable",
"var_lhs : variable",
"backref : tNTH_REF",
"backref : tBACK_REF",
"superclass : term",
"$$42 :",
"superclass : '<' $$42 expr_value term",
"superclass : error term",
"f_arglist : '(' f_args opt_nl ')'",
"f_arglist : f_args term",
"f_args : f_arg ',' f_optarg ',' f_rest_arg opt_f_block_arg",
"f_args : f_arg ',' f_optarg opt_f_block_arg",
"f_args : f_arg ',' f_rest_arg opt_f_block_arg",
"f_args : f_arg opt_f_block_arg",
"f_args : f_optarg ',' f_rest_arg opt_f_block_arg",
"f_args : f_optarg opt_f_block_arg",
"f_args : f_rest_arg opt_f_block_arg",
"f_args : f_block_arg",
"f_args :",
"f_norm_arg : tCONSTANT",
"f_norm_arg : tIVAR",
"f_norm_arg : tGVAR",
"f_norm_arg : tCVAR",
"f_norm_arg : tIDENTIFIER",
"f_arg : f_norm_arg",
"f_arg : f_arg ',' f_norm_arg",
"f_opt : tIDENTIFIER '=' arg_value",
"f_optarg : f_opt",
"f_optarg : f_optarg ',' f_opt",
"restarg_mark : '*'",
"restarg_mark : tSTAR",
"f_rest_arg : restarg_mark tIDENTIFIER",
"f_rest_arg : restarg_mark",
"blkarg_mark : '&'",
"blkarg_mark : tAMPER",
"f_block_arg : blkarg_mark tIDENTIFIER",
"opt_f_block_arg : ',' f_block_arg",
"opt_f_block_arg : none",
"singleton : var_ref",
"$$43 :",
"singleton : '(' $$43 expr opt_nl ')'",
"assoc_list : none",
"assoc_list : assocs trailer",
"assoc_list : args trailer",
"assocs : assoc",
"assocs : assocs ',' assoc",
"assoc : arg_value tASSOC arg_value",
"assoc : tLABEL arg_value",
"operation : tIDENTIFIER",
"operation : tCONSTANT",
"operation : tFID",
"operation2 : tIDENTIFIER",
"operation2 : tCONSTANT",
"operation2 : tFID",
"operation2 : op",
"operation3 : tIDENTIFIER",
"operation3 : tFID",
"operation3 : op",
"dot_or_colon : '.'",
"dot_or_colon : tCOLON2",
"opt_terms :",
"opt_terms : terms",
"opt_nl :",
"opt_nl : '\\n'",
"trailer :",
"trailer : '\\n'",
"trailer : ','",
"term : ';'",
"term : '\\n'",
"terms : term",
"terms : terms ';'",
"none :",

};
#endif
enum { 
  lhsBASE = 0, 
  lenBASE = 527, 
  defredBASE = 1054, 
  dgotoBASE = 1977, 
  sindexBASE = 2129, 
  rindexBASE = 3052, 
  gindexBASE = 3975, 
  tableBASE = 4127, 
  checkBASE = 24674, 
  unifiedSIZE = 45221 
 }; 
static short yylhs(uint64 v) { 
  UTL_ASSERT(v + lhsBASE < lenBASE); 
  return  yyUnifiedTable[v + lhsBASE]; 
}; 
static short yylen(uint64 v) { 
  UTL_ASSERT(v + lenBASE < defredBASE); 
  return  yyUnifiedTable[v + lenBASE]; 
}; 
static short yydefred(uint64 v) { 
  UTL_ASSERT(v + defredBASE < dgotoBASE); 
  return  yyUnifiedTable[v + defredBASE]; 
}; 
static short yydgoto(uint64 v) { 
  UTL_ASSERT(v + dgotoBASE < sindexBASE); 
  return  yyUnifiedTable[v + dgotoBASE]; 
}; 
static short yysindex(uint64 v) { 
  UTL_ASSERT(v + sindexBASE < rindexBASE); 
  return  yyUnifiedTable[v + sindexBASE]; 
}; 
static short yyrindex(uint64 v) { 
  UTL_ASSERT(v + rindexBASE < gindexBASE); 
  return  yyUnifiedTable[v + rindexBASE]; 
}; 
static short yygindex(uint64 v) { 
  UTL_ASSERT(v + gindexBASE < tableBASE); 
  return  yyUnifiedTable[v + gindexBASE]; 
}; 
static short yytable(uint64 v) { 
  UTL_ASSERT(v + tableBASE < checkBASE); 
  return  yyUnifiedTable[v + tableBASE]; 
}; 
static short yycheck(uint64 v) { 
  UTL_ASSERT(v + checkBASE < unifiedSIZE); 
  return  yyUnifiedTable[v + checkBASE]; 
}; 

/* yydebug defined in .y file now */
static int  yynerrs = 0;

/* # line 3237 "grammar.y" */ 


#undef ISALPHA
#undef ISSPACE
#undef ISALNUM
#undef ISDIGIT
#undef ISXDIGIT
#undef ISUPPER

enum {
  alpha_MASK =     0x1, // bits in ps->charTypes array
  digit_MASK =     0x2,
  ALNUM_MASK =     0x3,
  identchar_MASK = 0x4, 
  upper_MASK     = 0x8,
  xdigit_MASK                = 0x10,
  space_MASK                 = 0x20,
  tokadd_string_special_MASK = 0x40
};

static void initCharTypes(rb_parse_state *ps)
{
  UTL_ASSERT(ismbchar(258) == 0);
  memset(ps->charTypes, 0, sizeof(ps->charTypes));

  for (int c = 'a'; c <= 'z'; c++) {
    ps->charTypes[c] = alpha_MASK | identchar_MASK;
  }
  for (int c = 'A'; c <= 'Z'; c++) {
    ps->charTypes[c] = alpha_MASK | identchar_MASK | upper_MASK;
  }
  for (int c = '0'; c <= '9'; c++) {
    ps->charTypes[c] = digit_MASK | identchar_MASK | xdigit_MASK;
  }
  ps->charTypes[(int)'_'] = identchar_MASK;
  for (int c = 'A'; c <= 'F'; c++) {
    ps->charTypes[c] |= xdigit_MASK;
  }
  for (int c = 'a'; c <= 'f'; c++) {
    ps->charTypes[c] |= xdigit_MASK;
  }
  ps->charTypes[(int)' ' ] = space_MASK | tokadd_string_special_MASK;
  ps->charTypes[(int)'\f'] = space_MASK | tokadd_string_special_MASK;
  ps->charTypes[(int)'\n'] = space_MASK | tokadd_string_special_MASK;
  ps->charTypes[(int)'\r'] = space_MASK | tokadd_string_special_MASK;
  ps->charTypes[(int)'\t'] = space_MASK | tokadd_string_special_MASK;
  ps->charTypes[(int)'\v'] = space_MASK | tokadd_string_special_MASK;

  ps->charTypes[(int)'#'] |= tokadd_string_special_MASK;
  ps->charTypes[(int)'\\'] |= tokadd_string_special_MASK;
  ps->charTypes[(int)'/'] |= tokadd_string_special_MASK;
  ps->charTypes[0]        |= tokadd_string_special_MASK;
}

static inline int isAlpha(ByteType c, rb_parse_state *ps) 
{
  return ps->charTypes[c] & alpha_MASK ;
}

static inline int isAlphaNumeric(ByteType c, rb_parse_state *ps) 
{
  return ps->charTypes[c] & ALNUM_MASK ;
}

static inline int isUpper(ByteType c, rb_parse_state *ps)
{
  return ps->charTypes[c] & upper_MASK;
}
static inline int isDigit(ByteType c, rb_parse_state *ps)
{
  return ps->charTypes[c] & digit_MASK;
}
static inline int isXdigit(ByteType c, rb_parse_state *ps)
{
  return ps->charTypes[c] & xdigit_MASK;
}
static inline int isSpace(ByteType c, rb_parse_state *ps)
{
  return ps->charTypes[c] & space_MASK;
}

static inline int is_identchar(ByteType c, rb_parse_state *ps)
{
  return ps->charTypes[c] & identchar_MASK ;
}

static inline int tokadd_string_isSpecial(ByteType c, rb_parse_state *ps)
{
  return ps->charTypes[c] & tokadd_string_special_MASK;
}

static bool lex_getline(rb_parse_state *ps)
{
  // inline lex_gets
  char *ptr = ps->sourcePtr;
  char *limit = ps->sourceLimit;
  if (ptr >= limit) {
     return FALSE; // EOF
  }
  ps->lineStartOffset = ptr - ps->sourceBytes ;
  char *lineStart= ptr;
  while (ptr < limit) {
    char ch = *ptr;
    ptr += 1;
    if (ch == '\n') 
      break;
  }
  ps->sourcePtr = ptr;

  int64 lineLen = ptr - lineStart;
  bstring::btrunc(&ps->line_buffer, 0);
  bstring::bcatcstr(&ps->line_buffer, lineStart, lineLen, ps);

  return TRUE;
}


int64 RpNameToken::tLastToken() {
  return tLAST_TOKEN;
}

omObjSType* RubyArgsNode::add_arg(omObjSType **instH, omObjSType *arg, rb_parse_state *ps)
{
    if (OOP_IS_SMALL_INT(arg)) {
      if (ps->errorCount == 0) {
        rb_compile_error(ps, "illegal formal argument");
      } 
      return *instH;
    }
    omObjSType *res = RubyNode::call(*instH, sel_add_arg, arg, ps);
    if (! OOP_IS_SMALL_INT(res)) {
      rb_compile_error(ps, "illegal result in RubyArgsNode::add_arg");
    }
    int64 nArgs = OOP_TO_I64(res);
    if (nArgs > GEN_MAX_RubyFixedArgs) {
      char msg[128];
      snprintf(msg, sizeof(msg),
	   "more than %d formal arguments", GEN_MAX_RubyFixedArgs);
      rb_compile_error(ps, msg);
    }
    return *instH;
}

omObjSType* RubyParser::node_assign(omObjSType **lhsH, omObjSType* srcOfs, omObjSType *rhs,
			rb_parse_state *ps) 
{
  if (OOP_IS_SMALL_INT(rhs)) {
    if (ps->errorCount == 0) {
      rb_compile_error(ps, "illegal rhs for assignment ");
    }
    return *lhsH;
  }
  return RubyNode::call((AstClassEType)my_cls, sel_node_assign, *lhsH, srcOfs, rhs, ps);
}

static BoolType is_notop_id(NODE* id) {
  int64 val = QUID_to_id(id);
  return (val & ID_TOK_MASK) > tLAST_TOKEN ;
}

static BoolType v_is_notop_id(int64 val) 
{
  return (val & ID_TOK_MASK) > tLAST_TOKEN ;
}

static void resolveAstClass(om *omPtr, NODE **astClassesH, AstClassEType e_cls)
{
  const char* nam = "___badClass";
  switch (e_cls) {
    case  cls_RubyAbstractLiteralNode: nam = "RubyAbstractLiteralNode"; break;
    case  cls_RubyAbstractNumberNode: nam = "RubyAbstractNumberNode"; break;
    case  cls_RubyAliasNode: 		nam = "RubyAliasNode"; break;
    case  cls_RubyAndNode: nam = "RubyAndNode"; break;
    case  cls_RubyArgsNode: nam = "RubyArgsNode"; break;
    case  cls_RubyArrayNode: nam = "RubyArrayNode"; break;
    case  cls_RubyAttrAssignNode: nam = "RubyAttrAssignNode"; break;
    case  cls_RubyBackRefNode: nam = "RubyBackRefNode"; break;
    case  cls_RubyBeginNode: nam = "RubyBeginNode"; break;
    case  cls_RubyBlockArgNode: nam = "RubyBlockArgNode"; break;
    case  cls_RubyBlockNode: nam = "RubyBlockNode"; break;
    case  cls_RubyBlockPassNode: nam = "RubyBlockPassNode"; break;
    case  cls_RubyBreakNode: nam = "RubyBreakNode"; break;
    case  cls_RubyCaseNode: nam = "RubyCaseNode"; break;
    case  cls_RubyClassNode: nam = "RubyClassNode"; break;
    case  cls_RubyClassVarDeclNode: nam = "RubyClassVarDeclNode"; break;
    case  cls_RubyClassVarNode: nam = "RubyClassVarNode"; break;
    case  cls_RubyColon2Node: nam = "RubyColon2Node"; break;
    case  cls_RubyColon3Node: nam = "RubyColon3Node"; break;
    case  cls_RubyConstDeclNode: nam = "RubyConstDeclNode"; break;
    case  cls_RubyConstNode: nam = "RubyConstNode"; break;
    case  cls_RubyDefinedNode: nam = "RubyDefinedNode"; break;
    case  cls_RubyDotNode: nam = "RubyDotNode"; break;
    case  cls_RubyEnsureNode: nam = "RubyEnsureNode"; break;
    case  cls_RubyEvStrNode: nam = "RubyEvStrNode"; break;
    case  cls_RubyFalseNode: nam = "RubyFalseNode"; break;
    case  cls_RubyForNode: nam = "RubyForNode"; break;
    case  cls_RubyGlobalAsgnNode: nam = "RubyGlobalAsgnNode"; break;
    case  cls_RubyGlobalVarAliasNode: nam = "RubyGlobalVarAliasNode"; break;
    case  cls_RubyGlobalVarNode: nam = "RubyGlobalVarNode"; break;
    case  cls_RubyHashNode: nam = "RubyHashNode"; break;
    case  cls_RubyIfNode: nam = "RubyIfNode"; break;
    case  cls_RubyInstAsgnNode: nam = "RubyInstAsgnNode"; break;
    case  cls_RubyInstVarNode: nam = "RubyInstVarNode"; break;
    case  cls_RubyIterRpNode:   nam = "RubyIterRpNode"; break;
    case  cls_RubyLocalAsgnNode: nam = "RubyLocalAsgnNode"; break;
    case  cls_RubyLocalVarNode: nam = "RubyLocalVarNode"; break;
    case  cls_RubyModuleNode: nam = "RubyModuleNode"; break;
    case  cls_RubyNextNode: nam = "RubyNextNode"; break;
    case  cls_RubyNilNode: nam = "RubyNilNode"; break;
    case  cls_RubyNotNode: nam = "RubyNotNode"; break;
    case  cls_RubyNthRefNode: nam = "RubyNthRefNode"; break;
    case  cls_RubyOpAsgnNode: nam = "RubyOpAsgnNode"; break;
    case  cls_RubyOpElementAsgnNode: nam = "RubyOpElementAsgnNode"; break;
    case  cls_RubyOrNode: nam = "RubyOrNode"; break;
    case  cls_RubyParser: nam = "RubyParserM"; break;
    case  cls_RubyRedoNode: nam = "RubyRedoNode"; break;
    case  cls_RubyRescueBodyNode: nam = "RubyRescueBodyNode"; break;
    case  cls_RubyRescueNode: nam = "RubyRescueNode"; break;
    case  cls_RubyRetryNode: nam = "RubyRetryNode"; break;
    case  cls_RubyReturnNode: nam = "RubyReturnNode"; break;
    case  cls_RubyRpCallArgs: nam = "RubyRpCallArgs" ; break;
    case  cls_RpNameToken:    nam = "RpNameToken" ; break;
    case  cls_RubySClassNode: nam = "RubySClassNode"; break;
    case  cls_RubySValueNode: nam = "RubySValueNode"; break;
    case  cls_RubySelfNode: nam = "RubySelfNode"; break;
    case  cls_RubySplatNode: nam = "RubySplatNode"; break;
    case  cls_RubyStrNode: nam = "RubyStrNode"; break;
    case  cls_RubySymbolNode: nam = "RubySymbolNode"; break;
    case  cls_RubyTrueNode: nam = "RubyTrueNode"; break;
    case  cls_RubyWhenNode: nam = "RubyWhenNode"; break;
    case  cls_RubyZSuperNode : nam = "RubyZSuperNode"; break;
    case  cls_RubyLexStrTerm : nam = "RubyLexStrTerm"; break;
#if !defined(FLG_LINT_SWITCHES)
    default:
#endif
    case NUM_AST_CLASSES:
      GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "invalid enum value in resolveAstClass");
  }
  OmScopeType aScope(omPtr);
  NODE **symH = aScope.add( ObjNewSym(omPtr, nam));
  NODE **symListH = aScope.add( GemDoSessionSymList(omPtr));
  NODE *assoc = GemSupSearchSymList(omPtr, symListH, symH );
  if (assoc == ram_OOP_NIL) {
     GemErrAnsi(omPtr, ERR_ArgumentError, "resolveAstClass class not found: ", nam);
  }
  NODE **clsH = aScope.add( om::FetchOop(assoc, OC_ASSOCIATION_VALUE)); 
  om::StoreOop(omPtr, astClassesH, e_cls, clsH); 
}


static BoolType initAstSelector(om *omPtr, OopType *selectorIds, AstSelectorEType e_sel)
{
  const char *str = NULL;
  switch (e_sel) {
    case sel_add_arg: 		str = "add_arg:"; 	break;
    case sel_add_block_arg: 	str = "add_block_arg:"; break;
    case sel_add_optional_arg: 	str = "add_optional_arg:"; 	break;
    case sel_add_star_arg: 	str = "add_star_arg:"; 	break;
    case sel__append: 		str = "_append:"; 	break;
    case sel__appendAll: 	str = "_appendAll:"; break;
    case sel__append_amperLhs:  str = "_appendAmperLhs:";  break;
    case sel_append_arg: 	str = "append_arg:"; break;
    case sel_append_arg_blkArg: str = "append_arg:blkArg:"; break;
    case sel_append_arg_splatArg_blkArg: str = "append_arg:splatArg:blkArg:"; break;
    case sel_append_blkArg: 	str = "append_blk_arg:"; break;
    case sel_append_splatArg: 	str = "append_splatArg:"; break;
    case sel_append_splatArg_blk:  str = "append_splatArg:blk:"; break;
    case sel_append_to_block: 	str = "append_to_block:"; break;
    case sel_appendTo_evstr2dstr: str = "appendTo:evstr2dstr:";  break;
    case sel_arrayLength: 	str = "arrayLength"; break;
    case sel_block_append: 	str = "block_append:tail:"; break;
    case sel_colon2_name:	str = "colon2:name:"; break;
    case sel_colon3:		str = "colon3:"; break;
    case sel_callNode_:         str = "callNode:"; break;
    case sel_backref_error: 	str = "backref_error:" ; break;
    case sel_bodyNode_:         str = "bodyNode:"; break;
    case sel_includesTemp_:   str = "includesTemp:"; break;
    case sel_get_match_node:   str = "get_match_node:rhs:ofs:"; break;
    case sel_list_append: 	str = "list_append:item:"; break;
    case sel_list_prepend: 	str = "list_prepend:item:"; break;
    case sel_literal_concat:   str = "literal_concat:tail:"; break;
    case sel_logop: 		str = "logop:left:right:"; break;
    case sel_masgn_append_arg: str = "masgn_append_arg:right:"; break;
    case sel_masgn_append_mrhs: str = "masgn_append_mrhs:right:"; break;
    case sel__new: 		str = "_new"; break;
    case sel__new_: 		str = "_new:"; break;
    case sel__new_with: 	str = "_new:with:"; break;
    case sel_new_aref: 		str = "new_aref:args:ofs:"; break;
    case sel_new_call: 		str = "new_call:sel:arg:"; break;
    case sel_new_call_1: 	str = "new_call_1:sel:arg:"; break;
    case sel_new_call_braceBlock: str = "new_call_braceBlock:sel:args:blkArg:"; break;
    case sel_new_defn: 	str = "new_defn:args:body:ofs:startLine:endOfs:"; break;
    case sel_new_defs: 	str = "new_defs:name:args:body:ofs:startLine:endOfs:"; break;
    case sel_new_dsym:  	str = "new_dsym:"; break;
    case sel_new_evstr: 	str = "new_evstr:"; break;
    case sel_new_fcall: 	str = "new_fcall:arg:"; break;
    case sel_new_fcall_braceBlock: str = "new_fcall_braceBlock:args:blkArg:"; break;
    case sel_new_if: 		str = "new_if:t:f:ofs:"; break;
    case sel_new_op_asgn: 	str = "new_op_asgn:sel:arg:"; break;
    case sel_new_parasgn: 	str = "new_parasgn:ofs:comma:"; break;
    case sel_new_regexp: 	str = "new_regexp:options:"; break;
    case sel_new_string: 	str = "new_string:"; break;
    case sel_new_super: 	str = "new_super:ofs:"; break;
    case sel_new_undef: 	str = "new_undef:ofs:"; break;
    case sel_new_until: 	str = "new_until:expr:ofs:"; break;
    case sel_new_vcall: 	str = "new_vcall:sel:"; break;
    case sel_new_while: 	str = "new_while:expr:ofs:"; break;
    case sel_new_xstring: 	str = "new_xstring:"; break;
    case sel_new_yield: 	str = "new_yield:ofs:"; break;
    case sel_node_assign: 	str = "node_assign:ofs:rhs:"; break;
    case sel_opt_rescue: 	str = "opt_rescue:var:body:rescue:ofs:"; break;
    case sel_ret_args: 		str = "ret_args:"; break;
    case sel_s_a: 		str = "s_a:"; break;
    case sel_s_a_b: 		str = "s_a:b:"; break;
    case sel_s_a_b_c: 		str = "s_a:b:c:"; break;
    case sel_s_a_b_c_d: 	str = "s_a:b:c:d:"; break;
    case sel_s_a_b_c_d_e: 	str = "s_a:b:c:d:e:"; break;
    case sel_s_splat_blk:	str = "s_splat:blk:"; break;
    case sel_s_a_blk:		str = "s_a:blk:"; break;
    case sel_s_a_splat_blk:	str = "s_a:splat:blk:"; break;
    case sel_s_a_b_blk:		str = "s_a:b:blk:"; break;
    case sel_s_a_b_splat_blk:	str = "s_a:b:splat:blk:"; break;
    case sel_a_all_b_blk:	str = "s_a:all:b:blk:"; break;
    case sel_a_all_b_splat_blk:	str = "s_a:all:b:splat:blk:"; break;
    case sel_sym_srcOffset:     str = "sym:srcOffset:"; break;
    case sel_setParen:     	str = "setParen"; break;
    case sel_sym_ofs_val: 	str = "sym:ofs:val:"; break;
    case sel_uplus_production : str = "uplus_production:ofs:"; break;
    case sel_value_expr: 	str = "value_expr:"; break;
#if !defined(FLG_LINT_SWITCHES)
    default:
#endif
    case NUM_AST_SELECTORS:
      GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "invalid enum value in initAstSelector");
  }
  OmScopeType aScope(omPtr);
  NODE **strH = aScope.add( om::NewString_(omPtr, str));
  NODE* symO = ObjExistingCanonicalSym(omPtr, strH);
  if (symO == NULL) {
    printf( "non-existant symbol %s in initAstSelector\n", str);
    return FALSE;
  }
  OopType selObjId = om::objIdOfObj( symO);
  selectorIds[e_sel] = OOP_makeSelectorId(0, selObjId);
  return TRUE;
}

static void initAstSymbol(om *omPtr, NODE** symbolsH, AstSymbolEType e_sym)
{
  const char* str = NULL;
  switch (e_sym) {
    case a_sym_or: 	str = "or"; 	break; 
    case a_sym_orOp: str = "|"; break;
    case a_sym_OOR: str = "||"; break;
    case a_sym_upArrow: str = "^"; break;
    case a_sym_andOp: str = "&"; break;
    case a_sym_and:   str = "and"; 	break; 
    case a_sym_AAND: str = "&&"; break;
    case a_sym_tCMP : str = "<=>"; break;
    case a_sym_tEQ  : str = "=="; break;
    case a_sym_gt: str = ">"; break;
    case a_sym_tGEQ: str = ">="; break;
    case a_sym_lt: str = "<"; break;
    case a_sym_tLEQ: str = "<="; break;
    case a_sym_tLSHFT: str = "<<"; break;
    case a_sym_tRSHFT: str = ">>"; break;
    case a_sym_plus: str = "+"; break;
    case a_sym_minus: str = "-"; break;
    case a_sym_star: str = "*"; break;
    case a_sym_div: str = "/"; break;; break;
    case a_sym_percent: str = "%"; break;
    case a_sym_tPOW: str = "**"; break;
    case a_sym_tilde: str = "~" ; break;  // also for tMATCH
    case a_sym_tripleEq: str = "==="; break;
    case a_sym_tUPLUS: str = "+@"; break;
    case a_sym_tUMINUS: str = "-@"; break;
    case a_sym_tAREF: str = "[]"; break;
    case a_sym_tASET: str = "[]="; break;
    case a_sym_backtick: str = "`"; break;
    case a_sym_tNEQ: str = "!="; break;
    case a_sym_tEQQ: str = "==="; break;
    case a_sym_bang: str = "!"; break;
    case a_sym_dot2: str = ".."; break;
    case a_sym_dot3: str = "..."; break;
    case a_sym_tMATCH: str = "=~"; break;
    case a_sym_tNMATCH: str = "!~"; break;
    case a_sym_colon2: str = "::"; break;

    case a_sym_alias:   str = "alias";          break;
    case a_sym_break:   str = "break";          break;
    case a_sym_case:    str = "case";           break;
    case a_sym_class:   str = "class";          break;
    case a_sym_definedQ: str = "defined?";      break;
    case a_sym_ensure:  str = "ensure";         break;
    case a_sym_false:   str = "false";          break;
    case a_sym_for:     str = "for";            break;
    case a_sym_in:      str = "in";             break;
    case a_sym_next:    str = "next";           break;
    case a_sym_not:     str = "not";            break;
    case a_sym_redo:    str = "redo";           break;
    case a_sym_retry:   str = "retry";          break;
    case a_sym_return:  str = "return";         break;
    case a_sym_super:   str = "super";          break;
    case a_sym_true:    str = "true";           break;
    case a_sym_undef:   str = "undef";          break;
    case a_sym_when:    str = "when";           break;
    case a_sym_yield:   str = "yield";          break;

    case  a_sym_end: 	str = "end"; 	break; 
    case  a_sym_else: 	str = "else"; 	break; 
    case  a_sym_module: str = "module"; 	break; 
    case  a_sym_elsif: 	str = "elsif"; 	break; 
    case  a_sym_def: 	str = "def"; 	break; 
    case  a_sym_rescue: str = "rescue"; 	break; 
    case  a_sym_then: 	str = "then"; 	break; 
    case  a_sym_self: 	str = "self"; 	break; 
    case  a_sym_if: 	str = "if"; 	break; 
    case  a_sym_do: 	str = "do"; 	break; 
    case  a_sym_nil: 	str = "nil"; 	break; 
    case  a_sym_until: 	str = "until"; 	break; 
    case  a_sym_unless: str = "unless"; 	break; 
    case  a_sym_begin: 	str = "begin"; 	break; 
    case  a_sym__LINE_: str = "__LINE__"; 	break; 
    case  a_sym__FILE_: str = "__FILE__"; 	break; 
    case  a_sym_END: 	str = "END"; 	break; 
    case  a_sym_BEGIN: 	str = "BEGIN"; 	break; 
    case  a_sym_while: 	str = "while"; 	break; 
    case  a_sym_rest_args: str = "rest_args"; 	break; 

    case a_sym_INVALID:  return; // leave entry in symbolsH as NIL

#if !defined(FLG_LINT_SWITCHES)
    default:
#endif
    case NUM_AST_SYMBOLS:
      GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "invalid enum value in initAstSymbol");
      str = "badSym"; //lint
      break;
  }
  OmScopeType aScope(omPtr);
  NODE **symH = aScope.add( ObjNewSym(omPtr, str));
  om::StoreOop(omPtr, symbolsH, e_sym, symH);
}


static void sessionInit(om *omPtr, rb_parse_state *ps)
{
  omPtr->rubyParseState = ps;
  ps->omPtr = omPtr;

  ps->yystack.initialize();
  yygrowstack(ps, NULL);
  omPtr->rubyParseStack = &ps->yystack ;

  ps->astClassesH = omPtr->NewGlobalHandle();
  *ps->astClassesH = om::NewArray(omPtr, NUM_AST_CLASSES);

  ps->astSymbolsH = omPtr->NewGlobalHandle();
  *ps->astSymbolsH = om::NewArray(omPtr, NUM_AST_SYMBOLS);
 
  int id = 0;
  while (id < NUM_AST_CLASSES) {
    resolveAstClass(omPtr, ps->astClassesH, (AstClassEType)id);
    id += 1;
  } 
  id = 0;
  BoolType ok = TRUE;
  while (id < NUM_AST_SELECTORS) {
    ok &= initAstSelector(omPtr, ps->astSelectorIds, (AstSelectorEType)id);
    id += 1;
  }
  if (! ok) {
    GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "non-existant symbol(s) in initAstSelector");
  }
  id = 0;
  while (id < NUM_AST_SYMBOLS) {
    initAstSymbol(omPtr, ps->astSymbolsH, (AstSymbolEType)id);
    id += 1;
  }
  initCharTypes(ps);
}

omObjSType *MagCompileError902(om *omPtr, omObjSType **ARStackPtr)
{
  DOPRIM_ARGS(omPtr, 3);
  // omObjSType **recH = DOPRIM_STACK_ADDR(3);
  omObjSType **strH = DOPRIM_STACK_ADDR(2);
  omObjSType *isWarningOop = DOPRIM_STACK(1);

  rb_parse_state *ps = (rb_parse_state*) omPtr->rubyParseState;
  if (ps == NULL || ! ps->parserActive) 
    return ram_OOP_FALSE; // caller should signal an Exception
  
  omObjSType *strO = *strH;
  if (! OOP_IS_RAM_OOP(strO) || strO->classPtr()->strCharSize() != 1)
    return NULL;

  int64 strSize = om::FetchSize_(strO);
  char *cStr = ComHeapMalloc(ps->cst, strSize + 1);
  om::FetchCString_(strO, cStr, strSize + 1);  
  if (isWarningOop == ram_OOP_TRUE) {
    rb_warning(ps, cStr);
  } else if (isWarningOop == ram_OOP_FALSE) {
    rb_compile_error(cStr, ps);
  } else {
    return NULL;
  }
  return ram_OOP_TRUE; // error string was saved in parser state
}

omObjSType *MagParse903(om *omPtr, omObjSType **ARStackPtr)
{

  DOPRIM_ARGS(omPtr, 8);
  // omObjSType **recH = DOPRIM_STACK_ADDR(8);
  omObjSType **sourceH = DOPRIM_STACK_ADDR(7);
  omObjSType **cbytesH = DOPRIM_STACK_ADDR(6); // a CByteArray
  omObjSType *lineOop =  DOPRIM_STACK(5);
  omObjSType **fileNameH =  DOPRIM_STACK_ADDR(4);
  omObjSType *traceOop =    DOPRIM_STACK(3);
  omObjSType *warnOop =     DOPRIM_STACK(2);
  omObjSType **evalScopeH = DOPRIM_STACK_ADDR(1);

  if (! OOP_IS_SMALL_INT(lineOop))
    return NULL;
  if (! OOP_IS_SMALL_INT(traceOop))
    return NULL;

  BoolType printWarnings = warnOop == ram_OOP_TRUE;
  if (! printWarnings && warnOop != ram_OOP_FALSE)
     return NULL;

  int64 trace = OOP_TO_I64(traceOop);
  if (trace < 0) trace = 0;
  if (trace > 5) trace = 5;
#if defined(FLG_DEBUG)
  yTraceLevel = trace;
  yydebug = trace > 1;
#endif

  int64 lineBias = OOP_TO_I64(lineOop);
  if ((uint64)lineBias > INT_MAX) {
    GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "Parser lineNumber arg must be in range 0..0x7FFFFFFF");
  }
  { omObjSType *cbytesO = *cbytesH;
    if (! OOP_IS_RAM_OOP(cbytesO) || !  cbytesO->classPtr()->isCByteArray())
      return NULL;
  }
  { omObjSType *srcO = *sourceH;
    if (! OOP_IS_RAM_OOP(srcO) || srcO->classPtr()->strCharSize() != 1) 
      return NULL;
  } 
  { omObjSType *fileO = *fileNameH;
    if (! OOP_IS_RAM_OOP(fileO) || fileO->classPtr()->strCharSize() != 1) 
      return NULL;
  } 
  rb_parse_state *ps = (rb_parse_state*) omPtr->rubyParseState;
  if (ps == NULL) {
    // this path executed on first parse during session only
    ps = (rb_parse_state*)UtlMalloc( sizeof(*ps), "MagParseInitialize");
    sessionInit(omPtr, ps);
    omPtr->rubyParseState = ps;
    omPtr->rubyParseStack = &ps->yystack ;
  } else if (ps->parserActive) {
    GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "reentrant invocation of parser not supported");
  }
  if (lineBias > 0) {  // assume arg is one-based
    ps->lineNumber = lineBias - 1;
  } else {
    ps->lineNumber = 0;
  }
  ps->printWarnings = printWarnings;

  /* Setup an initial empty scope. */
  OmScopeType oScope(ps->omPtr);

  ps->cst = &omPtr->workspace()->compilerState;
  ComHeapInit(ps->cst);

  // initialize handles
  ps->lexvalH = oScope.newHandle();
  ps->yyvalH = oScope.newHandle();
  ps->_nilH = oScope.newHandle();
  ps->lex_strtermH = oScope.newHandle();
  ps->magicCommentsH = oScope.newHandle();
  ps->fileNameH = oScope.add(*fileNameH);
  ps->sourceStrH = oScope.add(*sourceH);
  ps->warningsH = oScope.newHandle();
  if (*evalScopeH == ram_OOP_NIL) {
    ps->evalScopeH = NULL;
  } else {
    omObjSType *evScope = *evalScopeH; // expect a RubyEvalScope
    if (! OOP_IS_RAM_OOP(evScope)) { // class RubyEvalScope in mcz only
      return NULL;
    }
    ps->evalScopeH = oScope.add(evScope); 
  }
  ps->lex_pbeg = NULL;
  ps->lex_p = NULL;
  ps->lex_pend = NULL;
  { NODE *cbytesO = *cbytesH;
    UTL_ASSERT(OOP_IS_RAM_OOP(cbytesO) && cbytesO->classPtr()->isCByteArray());
    int64 info = om::FetchSmallInt_(cbytesH, OC_CByteArray_info);
    cbytesO = *cbytesH;
    int64 srcSize = H_CByteArray::sizeBytes(info);
    if ((uint64)srcSize > INT_MAX) {
      GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "Parser maximum source string size is 2G bytes");
    }
    ps->sourceBytes = (char*)om::FetchCData(cbytesO);
    ps->sourcePtr = ps->sourceBytes;
    // -1 to exclude the null byte added by FetchCString
    ps->sourceLimit = ps->sourceBytes + srcSize - 1;
    UTL_ASSERT(ps->sourceLimit[0] == '\0');
  }
  ps->lineStartOffset = 0;
  ps->line_buffer.initialize();
  ps->lex_lastline = NULL;
  enum { initial_str_size = 256 };
  bstring::balloc(&ps->line_buffer, initial_str_size , ps);

  ps->tokenbuf = ComHeapMalloc(ps->cst, initial_str_size);
  ps->toksiz = initial_str_size;
  ps->tokidx = 0;

  ps->cond_stack.initialize();
  ps->cmdarg_stack.initialize();
  ps->yystack.setEmpty();

  ps->start_lines.initialize();

  ps->heredoc_end = 0;
  ps->end_seen = 0;
  { NODE *nameO = *fileNameH;
    int64 nameSiz = om::FetchSize_(nameO);
    ps->sourceFileName = ComHeapMalloc(ps->cst, nameSiz + 1);
    om::FetchCString_(nameO, ps->sourceFileName, nameSiz + 1);
#if defined(FLG_DEBUG)
    if (yTraceLevel > 0) {
      printf("--------------- begin yyparse\n");
      printf("           file %s\n", ps->sourceFileName);
    }
#endif
  };
  ps->command_start = TRUE;

  // debug_lines = 0;
  ps->compile_for_eval = 0;
  ps->command_start = TRUE;
  ps->class_nest = 0;
  ps->in_single = 0;
  ps->in_def = 0;
  ps->inStrTerm = 0;
  ps->errorCount = 0;
  // ps->cur_mid = 0;
  ps->eofReason = NULL;
  ps->firstErrorReason[0] = '\0';
  ps->atEof = 0;
  ps->firstErrorLine = -1;
  ps->parserActive = TRUE;

  int status = yyparse(ps);

  ps->parserActive = FALSE;

  omObjSType **resH = oScope.add(ps->yystack.mark->obj); // the AST
  ps->yystack.setEmpty();  // help gc
  if (status != 0 || ps->errorCount > 0) {
    *resH = *ps->warningsH;
    if (*resH == ram_OOP_NIL) {
      *resH = om::NewString(omPtr, 0);
    }
    char buf[512];
    const char* errStr;
    int lineNum = ps->firstErrorLine;
    if (lineNum < 0) {
      lineNum = ps->lineNumber;
    }
    if (ps->firstErrorReason[0] != '\0') {
      errStr = ps->firstErrorReason;
    } else {
      errStr = "syntax error"; 
    }
    snprintf(buf, sizeof(buf), "%s:%d: %s", ps->sourceFileName, lineNum, errStr);
    om::AppendToString(omPtr, resH, buf); 
    if (ps->atEof) {
      StartPosition *strt = ps->start_lines.back();
      if (strt != NULL) {
        snprintf(buf, sizeof(buf), 
          "\nunexpected EOF at line %d, missing 'end' for %s on line %d",
          	ps->lineNumber, strt->kind, strt->line );
      } else {
        snprintf(buf, sizeof(buf), "\nunexpected EOF at line %d",
          	ps->lineNumber );
      }
      om::AppendToString(omPtr, resH, buf); 
    }
  }
  // destroy ComHeaps (AST all in object memory now)
  ComHeapInit(ps->cst);
  return *resH; 
}

// --------------- begin lexer  implementation

static int nextc(rb_parse_state *parse_state)
{
    int c;

    if (parse_state->lex_p == parse_state->lex_pend) {
        if (! lex_getline(parse_state)) {
          return -1;  // EOF 
        }
        if (parse_state->heredoc_end > 0) {
            parse_state->lineNumber = parse_state->heredoc_end;
            parse_state->heredoc_end = 0;
        }
        parse_state->lineNumber += 1; // count lines

        // This code is setup so that lex_pend can be compared to
        // the data in lex_lastline. Thats important, otherwise
        // the heredoc code breaks. 
   
        if (parse_state->lex_lastline != &parse_state->line_buffer) {
          parse_state->lex_lastline = &parse_state->line_buffer;
        }
        bstring *v = parse_state->lex_lastline;

        parse_state->lex_pbeg = parse_state->lex_p = bstring::bdata(v);
        parse_state->lex_pend = parse_state->lex_p + bstring::blength(v);
    }
    c = (unsigned char)*(parse_state->lex_p++);
    if (c == '\r' && parse_state->lex_p < parse_state->lex_pend 
                  && *(parse_state->lex_p) == '\n') {
        parse_state->lex_p++;
        c = '\n';
        // parse_state->column = 0;
    } else if (c == '\n') {
        // lines already counted above
        // parse_state->column = 0;
    } else {
        // parse_state->column++;
    }

    return c;
}

static void pushback(int c, rb_parse_state *parse_state)
{
    if (c == -1) {
      return;
    }
    parse_state->lex_p--;
}

static BoolType was_bol(rb_parse_state *parse_state) 
{
  // Indicates if we're currently at the beginning of a line. 
  return parse_state->lex_p == (parse_state->lex_pbeg + 1);
}

static BoolType peek(int c, rb_parse_state *parse_state) 
{
  return parse_state->lex_p != parse_state->lex_pend 
            && c == *(parse_state->lex_p);
}

static BoolType ch_equals(int expected_c, int c)
{
  return c == expected_c ;
}

/* The token buffer. It's just a global string that has
   functions to build up the string easily. */

static inline void tokfix(rb_parse_state *ps) 
{ 
  ps->tokenbuf[ps->tokidx] = '\0';
}

static inline char* tok(rb_parse_state *ps) { return ps->tokenbuf; }

static inline intptr_t toklen(rb_parse_state *ps) { return ps->tokidx; }

static inline char toklast(rb_parse_state *ps) 
{
  intptr_t idx = ps->tokidx;
  return idx > 0 ? ps->tokenbuf[idx-1] : 0 ;
}

static void startToken(rb_parse_state *ps)  // was named newtok()
{
    ps->tokidx = 0;
    ps->tokStartDelta =  ps->lex_p - 1 - ps->lex_pbeg ;
    UTL_ASSERT( ps->tokenbuf != NULL);
}


static char * tokspace(int n, rb_parse_state *ps)
{
  ps->tokidx += n;

  if(ps->tokidx >= ps->toksiz) {
    do {
      ps->toksiz *= 2;
    } while(ps->toksiz < ps->tokidx);
    ps->tokenbuf = (char*)realloc(ps->tokenbuf, sizeof(char) * ps->toksiz);
  }
  return &(ps->tokenbuf[ps->tokidx - n]);
}

static void tokadd(char c, rb_parse_state *ps)
{
  UTL_ASSERT(ps->tokidx < ps->toksiz && ps->tokidx >= 0);

  int64 idx = ps->tokidx ;
  ps->tokenbuf[idx] = c;
  idx += 1;
  if (idx >= ps->toksiz) {
    int64 newSize = ps->toksiz * 2;
    char *buf = ComHeapMalloc(ps->cst, newSize);
    memcpy(buf, ps->tokenbuf, ps->toksiz);
    ps->tokenbuf = buf;
    ps->toksiz = newSize;
  }
  ps->tokidx = idx; 
}

#define tokcopy(n, ps) memcpy(tokspace(n, ps), ps->lex_p - (n), (n))

static int tokadd_utf8(rb_parse_state *ps, int string_literal, int symbol_literal, int regexp_literal)
{
  /*
   * If string_literal is true, then we allow multiple codepoints
   * in \u{}, and add the codepoints to the current token.
   * Otherwise we're parsing a character literal and return a single
   * codepoint without adding it
   */

  int codepoint;
  int numlen;

  if(regexp_literal) {
    tokadd('\\', ps); tokadd('u', ps);
  }

  if(peek('{', ps)) {  /* handle \u{...} form */
    do {
      if(regexp_literal) tokadd(*ps->lex_p, ps);
      nextc(ps);
      codepoint = scan_hex(ps->lex_p, 6, &numlen);

      if(numlen == 0)  {
        rb_compile_error("invalid Unicode escape", ps);
        return 0;
      }
      if(codepoint > 0x10ffff) {
        rb_compile_error("invalid Unicode codepoint (too large)", ps);
        return 0;
      }

      ps->lex_p += numlen;
      if(regexp_literal) {
        tokcopy(numlen, ps);
      } else if(codepoint >= 0x80) {
        char* dst = (char*)calloc(sizeof(char), 8);
        int offset_counter = 0;
        U8_APPEND_UNSAFE(dst, offset_counter, codepoint);
        for (unsigned int i = 0; i < strlen(dst); i++) {
          tokadd(*(dst + i), ps);
        }
        free(dst);
      } else if(string_literal) {
        tokadd(codepoint, ps);
      }
    } while(string_literal && (peek(' ', ps) || peek('\t', ps)));

    if(!peek('}', ps)) {
      rb_compile_error("unterminated Unicode escape", ps);
      return 0;
    }

    if(regexp_literal) tokadd('}', ps);
    nextc(ps);
  } else {			/* handle \uxxxx form */
    codepoint = scan_hex(ps->lex_p, 4, &numlen);
    if(numlen < 4) {
      rb_compile_error("invalid Unicode escape", ps);
      return 0;
    }
    ps->lex_p += 4;
    if(regexp_literal) {
      tokcopy(4, ps);
    } else if(codepoint >= 0x80) {
      if (string_literal) {
        char* dst = (char*)calloc(sizeof(char), 8);
        int offset_counter = 0;
        U8_APPEND_UNSAFE(dst, offset_counter, codepoint);
        for (unsigned int i = 0; i < strlen(dst); i++) {
          tokadd(*(dst + i), ps);
        }
        free(dst);
      }
    } else if(string_literal) {
      tokadd(codepoint, ps);
    }
  }

  return codepoint;
}
static int read_escape(rb_parse_state *ps)
{
    int c;

    switch (c = nextc(ps)) {
      case '\\':        /* Backslash */
        return c;

      case 'n': /* newline */
        return '\n';

      case 't': /* horizontal tab */
        return '\t';

      case 'r': /* carriage-return */
        return '\r';

      case 'f': /* form-feed */
        return '\f';

      case 'v': /* vertical tab */
        return '\13';

      case 'a': /* alarm(bell) */
        return '\007';

      case 'e': /* escape */
        return 033;

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
        {
            int numlen;

            pushback(c, ps);
            c = scan_oct(ps->lex_p, 3, &numlen);
            ps->lex_p += numlen;
        }
        return c;

      case 'x': /* hex constant */
        {
            int numlen;

            c = scan_hex(ps->lex_p, 2, &numlen);
            if (numlen == 0) {
                rb_compile_error("Invalid escape character syntax", ps);
                return 0;
            }
            ps->lex_p += numlen;
        }
        return c;

      case 'b': /* backspace */
        return '\010';

      case 's': /* space */
        return ' ';

      case 'M':
        if ((c = nextc(ps)) != '-') {
            rb_compile_error("Invalid escape character syntax", ps);
            pushback(c, ps);
            return '\0';
        }
        if ((c = nextc(ps)) == '\\') {
            return read_escape(ps) | 0x80;
        }
        else if (c == -1) goto eof;
        else {
            return ((c & 0xff) | 0x80);
        }

      case 'C':
        if ((c = nextc(ps)) != '-') {
            rb_compile_error("Invalid escape character syntax", ps);
            pushback(c, ps);
            return '\0';
        }
      case 'c':
        if ((c = nextc(ps))== '\\') {
            c = read_escape(ps);
        }
        else if (c == '?')
            return 0177;
        else if (c == -1) goto eof;
        return c & 0x9f;

      eof:
      case -1:
        rb_compile_error("Invalid escape character syntax", ps);
        return '\0';

      default:
        return c;
    }
}

static int
tokadd_escape(int term, rb_parse_state *ps)
{
    int c;

    switch (c = nextc(ps)) {
      case '\n':
        return 0;               /* just ignore */

      case '0': case '1': case '2': case '3': /* octal constant */
      case '4': case '5': case '6': case '7':
        {
            int i;

            tokadd((char)'\\', ps);
            tokadd((char)c, ps);
            for (i=0; i<2; i++) {
                c = nextc(ps);
                if (c == -1) goto eof;
                if (c < '0' || '7' < c) {
                    pushback(c, ps);
                    break;
                }
                tokadd((char)c, ps);
            }
        }
        return 0;

      case 'x': /* hex constant */
        {
            int numlen;

            tokadd('\\', ps);
            tokadd((char)c, ps);
            scan_hex(ps->lex_p, 2, &numlen);
            if (numlen == 0) {
                rb_compile_error("Invalid escape character syntax", ps);
                return -1;
            }
            while (numlen--)
                tokadd((char)nextc(ps), ps);
        }
        return 0;

      case 'M':
        if ((c = nextc(ps)) != '-') {
            rb_compile_error("Invalid escape character syntax", ps);
            pushback(c, ps);
            return 0;
        }
        tokadd('\\',ps);
        tokadd('M', ps);
        tokadd('-', ps);
        goto escaped;

      case 'C':
        if ((c = nextc(ps)) != '-') {
            rb_compile_error("Invalid escape character syntax", ps);
            pushback(c, ps);
            return 0;
        }
        tokadd('\\', ps);
        tokadd('C', ps);
        tokadd('-', ps);
        goto escaped;

      case 'c':
        tokadd('\\', ps);
        tokadd('c', ps);
      escaped:
        if ((c = nextc(ps)) == '\\') {
            return tokadd_escape(term, ps);
        }
        else if (c == -1) goto eof;
        tokadd((char)c, ps);
        return 0;

      eof:
      case -1:
        rb_compile_error("Invalid escape character syntax", ps);
        return -1;

      default:
        if (c != '\\' || c != term)
            tokadd('\\', ps);
        tokadd((char)c, ps);
    }
    return 0;
}

static omObjSType* regx_options(rb_parse_state *ps)
{
    int64 kcode = 0;
    int64 options = 0;
    int c;

    startToken(ps);
    while (c = nextc(ps), isAlpha(c, ps)) {
        switch (c) {
          case 'i':
            options |= RE_OPTION_IGNORECASE;
            break;
          case 'x':
            options |= RE_OPTION_EXTENDED;
            break;
          case 'm':
            options |= RE_OPTION_MULTILINE;
            break;
          case 'o':
            options |= RE_OPTION_ONCE;
            break;
          case 'G':
            options |= RE_OPTION_CAPTURE_GROUP;
            break;
          case 'g':
            options |= RE_OPTION_DONT_CAPTURE_GROUP;
            break;
          case 'n':
            kcode = 0x100000000; // Maglev ENC_NONE
            break;
          case 'e':
            kcode = 0x200000000; // Maglev ENC_EUC
            break;
          case 's':
            kcode = 0x300000000; // maglev ENC_SJIS
            break;
          case 'u':
            kcode = 0x400000000; // maglev ENC_UTF8
            break;
          default:
            tokadd((char)c, ps);
            break;
        }
    }
    pushback(c, ps);
    if (toklen(ps)) {
        tokfix(ps);
        char msg[1024];
        snprintf(msg, sizeof(msg), "unknown regexp option%s - %s",
                         toklen(ps) > 1 ? "s" : "", tok(ps));
        rb_compile_error(ps, msg);
    }
    return OOP_OF_SMALL_LONG_(options | kcode);
}

enum { STR_FUNC_ESCAPE = 0x01 ,
       STR_FUNC_EXPAND = 0x02 ,
       STR_FUNC_REGEXP = 0x04 ,
       STR_FUNC_QWORDS = 0x08 ,
       STR_FUNC_SYMBOL = 0x10 ,
       STR_FUNC_INDENT = 0x20
};

typedef enum {
    str_squote = 0,
    str_dquote = STR_FUNC_EXPAND,
    str_xquote = STR_FUNC_EXPAND,
    str_regexp = STR_FUNC_REGEXP|STR_FUNC_ESCAPE|STR_FUNC_EXPAND,
    str_sword  = STR_FUNC_QWORDS,
    str_dword  = STR_FUNC_QWORDS|STR_FUNC_EXPAND,
    str_ssym   = STR_FUNC_SYMBOL,
    str_dsym   = STR_FUNC_SYMBOL|STR_FUNC_EXPAND
} string_type ;

static int tokadd_string(int func, int term, int paren, NODE **strTermH,
				rb_parse_state *ps)
{
    int c;

    while ((c = nextc(ps)) != -1) {
        if (paren && c == paren) {
          RubyLexStrTerm::incrementNest(strTermH, 1, ps);
          tokadd((char)c, ps);
        } else if (c == term) {
          if ( strTermH == NULL || RubyLexStrTerm::nest(*strTermH) == 0 ) {
                pushback(c, ps);
                break;
          }
          RubyLexStrTerm::incrementNest(strTermH, -1, ps);
          tokadd((char)c, ps);
        } else if (ismbchar(c)) {
           int i, len = mbclen(c)-1;

           for (i = 0; i < len; i++) {
                tokadd((char)c, ps);
                c = nextc(ps);
           }
        } else if (! tokadd_string_isSpecial(c, ps)) {
          // c is none of isSpace , # , \\ , / , 0
          tokadd((char)c, ps);
        } else {
          if ((func & STR_FUNC_EXPAND) && c == '#' && ps->lex_p < ps->lex_pend) {
            int c2 = *(ps->lex_p);
            if (c2 == '$' || c2 == '@' || c2 == '{') {
                pushback(c, ps);
                break;
            }
          } else if (c == '\\') {
            c = nextc(ps);
            switch (c) {
              case '\n':
                if (func & STR_FUNC_QWORDS) break;
                if (func & STR_FUNC_EXPAND) continue;
                tokadd('\\', ps);
                break;

              case '\\':
                if (func & STR_FUNC_ESCAPE) tokadd((char)c, ps);
                break;

              case 'u':
                  if((func & STR_FUNC_EXPAND) == 0) {
                    tokadd('\\', ps);
                    break;
                  }
                  tokadd_utf8(ps, 1, func & STR_FUNC_SYMBOL, 
                                     func & STR_FUNC_REGEXP);
                  continue;

              default:
                if (func & STR_FUNC_REGEXP) {
                    pushback(c, ps);
                    if (tokadd_escape(term, ps) < 0)
                        return -1;
                    continue;
                }
                else if (func & STR_FUNC_EXPAND) {
                    pushback(c, ps);
                    if (func & STR_FUNC_ESCAPE) tokadd('\\', ps);
                    c = read_escape(ps);
                }
                else if ((func & STR_FUNC_QWORDS) && isSpace(c, ps)) {
                    /* ignore backslashed spaces in %w */
                }
                else if (c != term && !(paren && c == paren)) {
                    tokadd('\\', ps);
                }
            }
          } else if ((func & STR_FUNC_QWORDS) && isSpace(c, ps)) {
            pushback(c, ps);
            break;
          } else if ((func & STR_FUNC_REGEXP) && c == '/' && term != '/') {
            // added for Maglev, this path not in Rubinius .y file
            tokadd('\\', ps);
          }
          if (c == 0 && (func & STR_FUNC_SYMBOL)) {
            func &= ~STR_FUNC_SYMBOL;
            rb_compile_error(ps, "symbol cannot contain '\\0'");
            continue;
          }
          tokadd((char)c, ps);
        }
    }
    return c;
}

static int parse_string(NODE** quoteH/* a RubyLexStrTerm*/ , rb_parse_state *ps)
{
    int func = RubyLexStrTerm::func(*quoteH);
    if (func == -1) return tSTRING_END;

    int term = RubyLexStrTerm::term(*quoteH);

    int space = 0;
    int c = nextc(ps);
    if ((func & STR_FUNC_QWORDS) && isSpace(c, ps)) {
        do {
          c = nextc(ps);
        } while ( isSpace(c, ps));
        space = 1;
    }
    if (c == term &&  RubyLexStrTerm::nest(*quoteH) == 0 ) {
        if (func & STR_FUNC_QWORDS) {
            RubyLexStrTerm::set_func(quoteH, -1, ps);
            return ' ' ;
        }
        if (!(func & STR_FUNC_REGEXP)) return tSTRING_END;
        *ps->lexvalH = regx_options(ps);
        return tREGEXP_END;
    }
    if (space) {
        pushback(c, ps);
        return ' ';
    }
    startToken(ps);
    if ((func & STR_FUNC_EXPAND) && c == '#') {
        c = nextc(ps);
        switch (c) {
          case '$':
          case '@':
            pushback(c, ps);
            return tSTRING_DVAR;
          case '{':
            return tSTRING_DBEG;
        }
        tokadd('#', ps);
    }
    pushback(c, ps);
    int paren = RubyLexStrTerm::paren(*quoteH);
    if (tokadd_string(func, term, paren, quoteH , ps) == -1) {
        ps->lineNumber = RubyLexStrTerm::lineNum(*quoteH);
        rb_compile_error_override(ps, "unterminated string meets end of file");
        return tSTRING_END;
    }

    tokfix(ps);
    *ps->lexvalH = NEW_STR(tok(ps), toklen(ps), ps);
    return tSTRING_CONTENT;
}


static int heredoc_identifier(rb_parse_state *ps)
{
  // Called when the lexer detects a heredoc is beginning. This pulls
  // in more characters and detects what kind of heredoc it is. 

    int c = nextc(ps);
    int term = 0;
    int func = 0;
    size_t len;

    if (c == '-') {
        c = nextc(ps);
        func = STR_FUNC_INDENT;
    }
    switch (c) {
      case '\'':
        func |= str_squote; goto quoted;
      case '"':
        func |= str_dquote; goto quoted;
      case '`':
        func |= str_xquote;
      quoted:
        /* The heredoc indent is quoted, so its easy to find, we just
           continue to consume characters into the token buffer until
           we hit the terminating character. */

        startToken(ps);
        tokadd((char)func, ps);
        term = c;

        /* Where of where has the term gone.. */
        while ((c = nextc(ps)) != -1 && c != term) {
            len = mbclen(c);
            do {
              tokadd((char)c, ps);
            } while (--len > 0 && (c = nextc(ps)) != -1);
        }
        /* Ack! end of file or end of string. */
        if (c == -1) {
            rb_compile_error_override(ps, "unterminated here document identifier");
            return 0;
        }

        break;

      default:
        /* Ok, this is an unquoted heredoc ident. We just consume
           until we hit a non-ident character. */

        /* Do a quick check that first character is actually valid.
           if it's not, then this isn't actually a heredoc at all!
           It sucks that it's way down here in this function that in
           finally bails with this not being a heredoc.*/

        if (!is_identchar(c, ps)) {
            pushback(c, ps);
            if (func & STR_FUNC_INDENT) {
                pushback('-', ps);
            }
            return 0;
        }

        /* Finally, setup the token buffer and begin to fill it. */
        startToken(ps);
        term = '"';
        tokadd((char)(func |= str_dquote), ps);
        do {
            len = mbclen(c);
            do { tokadd((char)c, ps); } while (--len > 0 && (c = nextc(ps)) != -1);
        } while ((c = nextc(ps)) != -1 && is_identchar(c, ps));
        pushback(c, ps);
        break;
    }


    /* Fixup the token buffer, ie set the last character to null. */
    tokfix(ps);
    len = ps->lex_p - ps->lex_pbeg;
    ps->lex_p = ps->lex_pend;
    *ps->lexvalH = int64ToSi( 0 );

    /* Tell the lexer that we're inside a string now. nd_lit is
       the heredoc identifier that we watch the stream for to
       detect the end of the heredoc. */

    ps->set_lex_strterm(  RubyLexStrTerm::newHereDoc(ps, 
                               tok(ps), toklen(ps), /* nd_lit*/
                               len /* nd_nth */ ,  ps->lex_lastline/*nd_orig*/));
    return term == '`' ? tXSTRING_BEG : tSTRING_BEG;
}


static int
whole_match_p(const char *eos, int len, int indent, rb_parse_state *parse_state)
{
    char *p = parse_state->lex_pbeg;
    int n;

    if (indent) {
        while (*p && isSpace(*p, parse_state)) p++;
    }
    n = parse_state->lex_pend - (p + len);
    if (n < 0 || (n > 0 && p[len] != '\n' && p[len] != '\r')) return FALSE;
    if (strncmp(eos, p, len) == 0) return TRUE;
    return FALSE;
}


static int here_document(NODE **hereH, rb_parse_state *ps)
{
  // Called when the lexer knows it's inside a heredoc. This function
  // is responsible for detecting an expandions (ie #{}) in the heredoc
  //  and emitting a lex token and also detecting the end of the heredoc. 

    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    NODE **ndLitH = scp.add(RubyLexStrTerm::ndLit(*hereH));
    int64 len = om::FetchSize_(*ndLitH);

    /* eos == the heredoc ident that we found when the heredoc started */
    char *eos = ComHeapMalloc(ps->cst, len + 1);
    om::FetchCString_(*ndLitH, eos, len + 1);

    int func = eos[0];  // first byte of eos is  func
    eos +=1 ;
    len -= 1;

    /* indicates if we should search for expansions. */
    int indent = func & STR_FUNC_INDENT;
    if (yTraceLevel > 0) {
      printf("here_document line %d lineStartOffset %ld eos %s len %ld indent %d \n", 
	ps->lineNumber, ps->lineStartOffset, eos, len, indent);
    }
    NODE **strValH = scp.newHandle();

    /* Ack! EOF or end of input string! */
    int c = nextc(ps);
    if (c == -1) {
      error:
        char msg[1024];
        snprintf(msg, sizeof(msg), "can't find string \"%s\" anywhere before EOF", eos);
        rb_compile_error(ps, msg);
        heredoc_restore(ps);
        ps->clear_lex_strterm();
        return 0;
    }
    /* Gr. not yet sure what was_bol() means other than it seems like
       it means only 1 character has been consumed. */

    if (was_bol(ps) && whole_match_p(eos, len, indent, ps)) {
        if (yTraceLevel > 0) { 
          printf("here_document returns tSTRING_END\n");
        }
        heredoc_restore(ps);
        return tSTRING_END;
    }

    /* If aren't doing expansions, we can just scan until
       we find the identifier. */

    if ((func & STR_FUNC_EXPAND) == 0) {
        *strValH = om::NewString(omPtr, 0);
        do {
            char *p = bstring::bdata(ps->lex_lastline);
            char *pend = ps->lex_pend;
            if (pend > p) {
                switch (pend[-1]) {
                  case '\n':
                    if (--pend == p || pend[-1] != '\r') {
                        pend++;
                        break;
                    }
                  case '\r':
                    --pend;
                }
            }
            om::AppendToString(omPtr, strValH, p, pend - p);
            if (pend < ps->lex_pend) {
              om::AppendToString(omPtr, strValH, "\n", 1);
            }
            ps->lex_p = ps->lex_pend;
            if (nextc(ps) == -1) {
                *strValH = ram_OOP_NIL;
                goto error;
            }
        } while (! whole_match_p(eos, len, indent, ps));
    } else {
        startToken(ps);
        if (c == '#') {
            switch (c = nextc(ps)) {
              case '$':
              case '@':
                pushback(c, ps);
                if (yTraceLevel > 0) { 
                  printf("here_document returns tSTRING_DVAR\n");
                }
                return tSTRING_DVAR;
              case '{':
                if (yTraceLevel > 0) { 
                  printf("here_document returns tSTRING_DBEG\n");
                }
                return tSTRING_DBEG;
            }
            tokadd('#', ps);
        }

        /* Loop while we haven't found a the heredoc ident. */
        do {
            pushback(c, ps);
            /* Scan up until a \n and fill in the token buffer. */
            c = tokadd_string(func, '\n', 0, NULL, ps);
            if (c == -1) {
              goto error;
            }

            /* We finished scanning, but didn't find a \n, so we setup the node
               and have the lexer file in more. */
            if (c != '\n') {
                *ps->lexvalH = NEW_STR(tok(ps), toklen(ps), ps);
                if (yTraceLevel > 0) {
                  char buf[1024];
                  om::FetchCString_(*ps->lexvalH, buf, sizeof(buf)); 
                  printf("here_document returns tSTRING_CONTENT, %s\n", buf);
                }
                return tSTRING_CONTENT;
            }

            c = nextc(ps); /* I think this consumes the \n */
            tokadd(c, ps);
            c = nextc(ps);
            if (c == -1) {
              goto error;
            }
        } while (! whole_match_p(eos, len, indent, ps));
        *strValH = NEW_STR(tok(ps), toklen(ps), ps);
    }
    UTL_ASSERT(*strValH != ram_OOP_NIL);
    if (yTraceLevel > 0) {
      char buf[1024];
      om::FetchCString_(*strValH, buf, sizeof(buf));
      printf("here_document returns tSTRING_CONTENT, %s\n", buf);
    }
    heredoc_restore(ps);
    ps->set_lex_strterm( NEW_STRTERM(-1, 0, 0, ps));
    *ps->lexvalH = *strValH;
    return tSTRING_CONTENT;
}

#include "rubylex_tab.hc"

static void arg_ambiguous(rb_parse_state *ps)
{
  rb_warning(ps, "ambiguous first argument; put parentheses or even spaces");
}

static int IS_ARG(int lex_state)
{
  return lex_state & (EXPR_ARG | EXPR_CMDARG);
}

static int IS_ARG_or_END(int lex_state)
{
  return lex_state & (EXPR_ARG | EXPR_CMDARG|EXPR_END);
}

static int IS_EXPR_FNAME_or_DOT(int lex_state)
{
  return lex_state & (EXPR_FNAME | EXPR_DOT);
}

static int IS_EXPR_BEG_or_FNAME_DOT_CLASS(int lex_state)
{
  return lex_state & (EXPR_BEG| EXPR_FNAME | EXPR_DOT | EXPR_CLASS);
}

static int IS_EXPR_BEG_or_MID(int lex_state)
{
  return lex_state & (EXPR_BEG | EXPR_MID);
} 

static int IS_EXPR_BEG_or_MID_or_CLASS(int lex_state)
{
  return lex_state & (EXPR_BEG | EXPR_MID | EXPR_CLASS);
} 

static int IS_EXPR_END_or_ENDARG(int lex_state)
{
   return lex_state & (EXPR_END | EXPR_ENDARG);
}

static int IS_EXPR_BEG_or_MID_DOT_ARG_CMDARG(int lex_state)
{
   return lex_state & (EXPR_BEG | EXPR_MID | EXPR_DOT | EXPR_ARG | EXPR_CMDARG);
}

static int IS_noneOf_EXPR_END_or_DOT_ENDARG_CLASS(int lex_state)
{  
  return (lex_state & (EXPR_END | EXPR_DOT | EXPR_ENDARG | EXPR_CLASS)) == 0;
}

static char* parse_comment(struct rb_parse_state* parse_state) 
{
  // return NULL or start of a magic comment( prefixed by "-*-" after the # )
  int len = parse_state->lex_pend - parse_state->lex_p;

  char* str = parse_state->lex_p;
  while (len-- > 0 && isSpace(str[0], parse_state)) {
    // skip white space after the # 
    str++;
  }

  if (len <= 2) return NULL;

  if (str[0] == '-' && str[1] == '*' && str[2] == '-') return str;

  return NULL;
}

static NODE* newInteger(rb_parse_state *ps, int radix)
{
  int64 len = toklen(ps);
  if (len < 10) { // within a SmallInteger
    tokfix(ps); 
    errno = 0;
    int64 val = strtol(tok(ps), NULL, radix); 
    if (errno != 0) {
      rb_compile_error(ps, "invalid integer literal format");
    }
    return OOP_OF_SMALL_LONG_(val);
  } else {
    om *omPtr = ps->omPtr;
    OmScopeType aScope(omPtr);
    NODE **strH = aScope.add(om::NewString__(omPtr, (ByteType*)tok(ps), len));
    NODE *intO = LrgRubyStringToInteger(omPtr, strH, radix, 1);
    if (intO == NULL) {
      rb_compile_error(ps, "invalid large integer literal format");
      return ram_OOP_Zero;
    }
    return intO;
  }
}

static int lexPlusMinus(rb_parse_state* ps, int space_seen, int aResult, int unaryResult);

static int yylex(rb_parse_state* ps)
{
    int c;
    int space_seen = 0;
    int cmd_state;
    LexStateKind last_state;

    // c = nextc();			// Rubinius has commented out (uncomment for debug?)
    // printf("lex char: %c\n", c);
    // pushback(c, parse_state);

    // cache lex_state in variable for better code generation by C compiler
    LexStateKind lex_state = ps->lex_state;

#define SET_lexState(v) {\
  ps->lex_state = v; \
  lex_state = v; \
}

    if (ps->inStrTerm) {
      NODE **lex_strtermH = ps->lex_strtermH; 
      NODE *lex_strtermO = *lex_strtermH; 
      int token;
      if ( RubyLexStrTerm::kind(lex_strtermO) == NODE_HEREDOC) {
        token = here_document(lex_strtermH, ps);
        if (token == tSTRING_END) {
          ps->clear_lex_strterm();
          SET_lexState(EXPR_END);
        }
      } else {
	token = parse_string(lex_strtermH, ps);
	if (token == tSTRING_END || token == tREGEXP_END) {
          ps->clear_lex_strterm();
          SET_lexState( EXPR_END);
        }
      }
      return token;
    }

    cmd_state = ps->command_start;
    ps->command_start = FALSE;
  retry:
    c = nextc(ps);
    switch (c) {
      case '\0':                /* NUL */
        ps->eofReason = "NUL byte in source file interpreted as EOF";
        goto at_eof ;
      case '\004':              /* ^D */
        ps->eofReason = "ctl-D in source file interpreted as EOF";
        goto at_eof ;
      case '\032':              /* ^Z */
        ps->eofReason = "ctl-Z in source file interpreted as EOF";
        goto at_eof ;
      case -1:                  /* end of script. */
        ps->eofReason = NULL; // normal EOF
   at_eof: ;
        ps->atEof = TRUE;
        return 0;

        /* white spaces */
      case ' ': case '\t': case '\f': case '\r':
      case '\13': /* '\v' */
        space_seen++;
        goto retry;

      case '#':         /* it's a comment */
        if (char* str = parse_comment(ps)) {
            // it is a magic comment
            int len = ps->lex_pend - str - 1; // - 1 for the \n
            NODE **magicCommentsH = ps->magicCommentsH;
            om *omPtr = ps->omPtr;
            if (*magicCommentsH == ram_OOP_NIL) {
              *magicCommentsH = om::NewArray(omPtr, 0);
            }  
            OmScopeType scp(omPtr);
            NODE **lineH = scp.add(om::NewString__(omPtr, (ByteType*)str, len));
            om::AppendToArray(omPtr, magicCommentsH, lineH);
        }
        ps->lex_p = ps->lex_pend;
        /* fall through */
      case '\n':
        if (IS_EXPR_BEG_or_FNAME_DOT_CLASS(lex_state)) {
            goto retry;
        }
        ps->command_start = TRUE;
        SET_lexState( EXPR_BEG);
        return '\n';

      case '*':
        if ((c = nextc(ps)) == '*') {
            if ((c = nextc(ps)) == '=') {
                startToken(ps);
                *ps->lexvalH = RpNameToken::s( a_sym_tPOW, ps);
                SET_lexState( EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(c, ps);
            c = tPOW;
        }
        else {
            if (c == '=') {
                startToken(ps);
                *ps->lexvalH = RpNameToken::s( a_sym_star, ps);
                SET_lexState( EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(c, ps);
            if (IS_ARG(lex_state) && space_seen && ! isSpace(c, ps)){
                rb_warning(ps, "`*' interpreted as argument prefix");
                c = tSTAR;
            }
            else if (IS_EXPR_BEG_or_MID(lex_state)) {
                c = tSTAR;
            }
            else {
                c = '*';
            }
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) { 
          SET_lexState( EXPR_ARG);
        } else {
          SET_lexState( EXPR_BEG); 
        } 
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi of tPOW, tSTAR or '*'
        return c;

      case '!':
        SET_lexState( EXPR_BEG);
        if ((c = nextc(ps)) == '=') {
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return tNEQ;
        }
        if (c == '~') {
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return tNMATCH;
        }
        pushback(c, ps);
        return '!';

      case '=':
        if (was_bol(ps)) {
            /* skip embedded rd document */
            if (strncmp(ps->lex_p, "begin", 5) == 0 
                 && isSpace(ps->lex_p[5], ps)) {
                for (;;) {
                    ps->lex_p = ps->lex_pend;
                    c = nextc(ps);
                    if (c == -1) {
                        rb_compile_error_override(ps, "embedded document meets end of file");
                        return 0;
                    }
                    if (c != '=') continue;
                    if (strncmp(ps->lex_p, "end", 3) == 0 &&
                        (ps->lex_p + 3 == ps->lex_pend || 
			   isSpace(ps->lex_p[3], ps))) {
                        break;
                    }
                }
                ps->lex_p = ps->lex_pend;
                goto retry;
            }
        }

        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        c = nextc(ps);
        if (c == '=') {
           c = nextc(ps);
           if (c == '=') {
              *ps->lexvalH = RpNameToken::s( a_sym_tripleEq, ps);
              return tEQQ;
            }
            pushback(c, ps);
            *ps->lexvalH = RpNameToken::s( a_sym_tEQ, ps);
            return tEQ;
        }
        if (c == '~') {
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); 
            return tMATCH;
        }
        else if (c == '>') {
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); 
            return tASSOC;
        }
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); 
        return '=';

      case '<':
        c = nextc(ps);
        if (c == '<' &&
            IS_noneOf_EXPR_END_or_DOT_ENDARG_CLASS(lex_state) &&
            (! IS_ARG(lex_state) || space_seen)) {
	  int token = heredoc_identifier(ps);
	  if (token) return token;
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
	    SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        if (c == '=') {
            if ((c = nextc(ps)) == '>') {
                *ps->lexvalH = RpNameToken::s( a_sym_tCMP , ps);
                return tCMP;
            }
            pushback(c, ps);
            *ps->lexvalH = RpNameToken::s( a_sym_tLEQ,  ps);
            return tLEQ;
        }
        if (c == '<') {
            if ((c = nextc(ps)) == '=') {
                startToken(ps);
                *ps->lexvalH = RpNameToken::s( a_sym_tLSHFT, ps);
                SET_lexState( EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(c, ps);
            *ps->lexvalH = RpNameToken::s( a_sym_tLSHFT, ps);
            return tLSHFT;
        }
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); 
        return '<';

      case '>':
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        if ((c = nextc(ps)) == '=') {
            *ps->lexvalH = RpNameToken::s( a_sym_tGEQ, ps);
            return tGEQ;
        }
        if (c == '>') {
            if ((c = nextc(ps)) == '=') {
                startToken(ps);
                *ps->lexvalH = RpNameToken::s( a_sym_tRSHFT, ps);
                SET_lexState( EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(c, ps);
            *ps->lexvalH = RpNameToken::s( a_sym_tRSHFT, ps);
            return tRSHFT;
        }
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi 
        return '>';

      case '"':
        ps->set_lex_strterm( NEW_STRTERM(str_dquote, '"', 0, ps));
        return tSTRING_BEG;

      case '`':
        if (lex_state == EXPR_FNAME) {
            SET_lexState( EXPR_END);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return c;
        }
        if (lex_state == EXPR_DOT) {
            if (cmd_state) {
                SET_lexState( EXPR_CMDARG);
            } else {
                SET_lexState( EXPR_ARG);
            }
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return c;
        }
        ps->set_lex_strterm(NEW_STRTERM(str_xquote, '`', 0, ps));
        *ps->lexvalH = int64ToSi( 0 ); /* so that xstring gets used normally */
        return tXSTRING_BEG;

      case '\'':
        ps->set_lex_strterm(NEW_STRTERM(str_squote, '\'', 0, ps));
        *ps->lexvalH = int64ToSi( 0 ); /* so that xstring gets used normally */
        return tSTRING_BEG;

      case '?':
        if (IS_EXPR_END_or_ENDARG(lex_state)) {
            SET_lexState( EXPR_BEG);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return '?';
        }
        c = nextc(ps);
        if (c == -1) {
            rb_compile_error(ps, "incomplete character syntax");
            return 0;
        }
        if (isSpace(c, ps)){
            if (! IS_ARG(lex_state)){
                int c2 = 0;
                switch (c) {
                  case ' ':
                    c2 = 's';
                    break;
                  case '\n':
                    c2 = 'n';
                    break;
                  case '\t':
                    c2 = 't';
                    break;
                  case '\v':
                    c2 = 'v';
                    break;
                  case '\r':
                    c2 = 'r';
                    break;
                  case '\f':
                    c2 = 'f';
                    break;
                }
                if (c2) {
                   char msg[128];
                   snprintf(msg, sizeof(msg), "invalid character syntax; use ?\\%c", c2);
                   rb_warning(ps, msg);
                }
            }
          ternary:
            pushback(c, ps);
            SET_lexState( EXPR_BEG);
            ps->ternary_colon = 1;
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return '?';
        }
        else if (ismbchar(c)) {
            char msg[128];
            snprintf(msg, sizeof(msg), "multibyte character literal not supported yet; use ?\\%.3o", c);
            rb_warning(ps, msg);
            goto ternary;
        }
        else if ( is_identchar(c, ps) /* was (ISALNUM(c) || c == '_')  */
                   && ps->lex_p < ps->lex_pend 
                   && is_identchar(*(ps->lex_p),  ps)) {
            goto ternary;
        }
        else if (c == '\\') {
            c = read_escape(ps);
        }
        startToken(ps);
        c &= 0xff;
        tokadd((char)c,ps);
        tokfix(ps);
        SET_lexState( EXPR_END);
        *ps->lexvalH = RubyStrNode::s( NEW_STR(tok(ps), toklen(ps), ps) , ps);
        SET_lexState( EXPR_END);
        return tCHAR;
      case '&':
        if ((c = nextc(ps)) == '&') {
            SET_lexState( EXPR_BEG);
            if ((c = nextc(ps)) == '=') {
                startToken(ps);
                *ps->lexvalH = RpNameToken::s( a_sym_AAND, ps);
                SET_lexState( EXPR_BEG);
                return tOP_ASGN;
            }
            pushback(c, ps);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return tANDOP;
        }
        else if (c == '=') {
            startToken(ps);
            *ps->lexvalH = RpNameToken::s( a_sym_andOp, ps);
            SET_lexState( EXPR_BEG);
            return tOP_ASGN;
        }
        pushback(c, ps);
        if (IS_ARG(lex_state) && space_seen && ! isSpace(c, ps)){
            rb_warning(ps, "`&' interpreted as argument prefix");
            c = tAMPER;
        }
        else if (IS_EXPR_BEG_or_MID(lex_state)) {
            c = tAMPER;
        }
        else {
            c = '&';
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return c;

      case '|':
        if ((c = nextc(ps)) == '|') {
            SET_lexState( EXPR_BEG);
            if ((c = nextc(ps)) == '=') {
               startToken(ps);
               *ps->lexvalH = RpNameToken::s( a_sym_OOR, ps);
               SET_lexState( EXPR_BEG);
               return tOP_ASGN;
            }
            pushback(c, ps);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return tOROP;
        }
        if (c == '=') {
            startToken(ps);
            *ps->lexvalH = RpNameToken::s( a_sym_orOp, ps);
            SET_lexState( EXPR_BEG);
            return tOP_ASGN;
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return '|';

      case '+': {
        int aResult = lexPlusMinus(ps, space_seen, '+', tUPLUS); 
        if (aResult < 0) {
          UTL_ASSERT(aResult == -1);
          goto start_num;
          }
        return aResult;
        }

      case '-': {
        int aResult = lexPlusMinus(ps, space_seen, '-', tUMINUS);
        if (aResult < 0) {
          UTL_ASSERT(aResult == -1);
          goto start_num;
          }
        return aResult;
        }


      case '.':
        SET_lexState( EXPR_BEG);
        if ((c = nextc(ps)) == '.') {
            if ((c = nextc(ps)) == '.') {
                *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
                return tDOT3;
            }
            pushback(c, ps);
             *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return tDOT2;
        }
        pushback(c, ps);
        if ( isDigit(c, ps)) {
            rb_compile_error("no .<digit> floating literal anymore; put 0 before dot", ps);
        }
        SET_lexState( EXPR_DOT);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
        return '.';

      start_num:
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        {
            int is_float, seen_point, seen_e, nondigit;

            is_float = seen_point = seen_e = nondigit = 0;
            SET_lexState( EXPR_END);
            startToken(ps);
            if (c == '-' || c == '+') {
                tokadd((char)c,ps);
                c = nextc(ps);
            }
            if (c == '0') {
                int start = toklen(ps);
                c = nextc(ps);
                if (c == 'x' || c == 'X') {
                    /* hexadecimal */
                    c = nextc(ps);
                    if (isXdigit(c, ps)) {
                        do {
                            if (c == '_') {
                                if (nondigit) break;
                                nondigit = c;
                                continue;
                            }
                            if (! isXdigit(c, ps)) break;
                            nondigit = 0;
                            tokadd((char)c,ps);
                        } while ((c = nextc(ps)) != -1);
                    }
                    pushback(c, ps);
                    tokfix(ps);
                    if (toklen(ps) == start) {
                        rb_compile_error("numeric literal without digits", ps);
                    }
                    else if (nondigit) goto trailing_uc;
                    *ps->lexvalH = newInteger(ps, 16);
                    return tINTEGER;
                }
                if (c == 'b' || c == 'B') {
                    /* binary */
                    c = nextc(ps);
                    if (c == '0' || c == '1') {
                        do {
                            if (c == '_') {
                                if (nondigit) break;
                                nondigit = c;
                                continue;
                            }
                            if (c != '0' && c != '1') break;
                            nondigit = 0;
                            tokadd((char)c, ps);
                        } while ((c = nextc(ps)) != -1);
                    }
                    pushback(c, ps);
                    tokfix(ps);
                    if (toklen(ps) == start) {
                        rb_compile_error("numeric literal without digits", ps);
                    }
                    else if (nondigit) goto trailing_uc;

                    *ps->lexvalH = newInteger(ps, 2);
                    return tINTEGER;
                }
                if (c == 'd' || c == 'D') {
                    /* decimal */
                    c = nextc(ps);
                    if ( isDigit(c, ps)) {
                        do {
                            if (c == '_') {
                                if (nondigit) break;
                                nondigit = c;
                                continue;
                            }
                            if (! isDigit(c, ps)) break;
                            nondigit = 0;
                            tokadd((char)c, ps);
                        } while ((c = nextc(ps)) != -1);
                    }
                    pushback(c, ps);
                    tokfix(ps);
                    if (toklen(ps) == start) {
                        rb_compile_error("numeric literal without digits", ps);
                    }
                    else if (nondigit) goto trailing_uc;
                    *ps->lexvalH = newInteger(ps, 10);
                    return tINTEGER;
                }
                if (c == '_') {
                    /* 0_0 */
                    goto octal_number;
                }
                if (c == 'o' || c == 'O') {
                    /* prefixed octal */
                    c = nextc(ps);
                    if (c == '_') {
                        rb_compile_error("numeric literal without digits", ps);
                    }
                }
                if (c >= '0' && c <= '7') {
                    /* octal */
                  octal_number:
                    do {
                        if (c == '_') {
                            if (nondigit) break;
                            nondigit = c;
                            continue;
                        }
                        if (c < '0' || c > '7') break;
                        nondigit = 0;
                        tokadd((char)c, ps);
                    } while ((c = nextc(ps)) != -1);
                    if (toklen(ps) > start) {
                        pushback(c, ps);
                        tokfix(ps);
                        if (nondigit) goto trailing_uc;
                        *ps->lexvalH = newInteger(ps, 8);
                        return tINTEGER;
                    }
                    if (nondigit) {
                        pushback(c, ps);
                        goto trailing_uc;
                    }
                }
                if (c > '7' && c <= '9') {
                    rb_compile_error("Illegal octal digit", ps);
                }
                else if (c == '.' || c == 'e' || c == 'E') {
                    tokadd('0', ps);
                }
                else {
                    pushback(c, ps);
                    *ps->lexvalH = ram_OOP_Zero ; 
                    return tINTEGER;
                }
            }

            for (;;) {
                switch (c) {
                  case '0': case '1': case '2': case '3': case '4':
                  case '5': case '6': case '7': case '8': case '9':
                    nondigit = 0;
                    tokadd((char)c, ps);
                    break;

                  case '.':
                    if (nondigit) goto trailing_uc;
                    if (seen_point || seen_e) {
                        goto decode_num;
                    }
                    else {
                        int c0 = nextc(ps);
                        if (! isDigit(c0, ps)) {
                            pushback(c0, ps);
                            goto decode_num;
                        }
                        c = c0;
                    }
                    tokadd('.', ps);
                    tokadd((char)c, ps);
                    is_float++;
                    seen_point++;
                    nondigit = 0;
                    break;

                  case 'e':
                  case 'E':
                    if (nondigit) {
                        pushback(c, ps);
                        c = nondigit;
                        goto decode_num;
                    }
                    if (seen_e) {
                        goto decode_num;
                    }
                    tokadd((char)c, ps);
                    seen_e++;
                    is_float++;
                    nondigit = c;
                    c = nextc(ps);
                    if (c != '-' && c != '+') continue;
                    tokadd((char)c, ps);
                    nondigit = c;
                    break;

                  case '_':     /* `_' in number just ignored */
                    if (nondigit) goto decode_num;
                    nondigit = c;
                    break;

                  default:
                    goto decode_num;
                }
                c = nextc(ps);
            }

          decode_num:
            pushback(c, ps);
            tokfix(ps);
            if (nondigit) {
                char tmp[30];
              trailing_uc:
                snprintf(tmp, sizeof(tmp), "trailing `%c' in number", nondigit);
                rb_compile_error(tmp, ps);
            }
            if (is_float) {
               char *str = tok(ps);
               char *sResultPtr;
               double d = strtod(str, &sResultPtr);
               if (sResultPtr == str) {
                 d = GCI_plusQuietNan;
               }
               *ps->lexvalH = FloatPrimDoubleToOop(ps->omPtr, d);
               return tFLOAT;
            }
            *ps->lexvalH = newInteger(ps, 10);
            return tINTEGER;
        }

      case ']':
      case '}':
      case ')':
        // deleted COND_LEXPOP, CMDARG_LEXPOP , replaced with rParenLexPop
        //  in grammar action blocks, because lexing of a closing '}'
        //  can trigger other grammar actions such as closing a non-parenthesized
        //  list of args for a method call. doing the POP here  can result
        //  in POP re-ordering that disagrees with the grammar.
        SET_lexState( EXPR_END);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return c;

      case ':':
        c = nextc(ps);
        if (c == ':') {
            if (IS_EXPR_BEG_or_MID_or_CLASS(lex_state) || 
                (IS_ARG(lex_state) && space_seen)) {
               SET_lexState( EXPR_BEG);
               *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
               return tCOLON3;
            }
            SET_lexState( EXPR_DOT);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return tCOLON2;
        }
        if (IS_EXPR_END_or_ENDARG(lex_state)
		||  isSpace(c, ps)) {
            pushback(c, ps);
            SET_lexState( EXPR_BEG);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
            return ':';
        }
        switch (c) {
          case '\'':
            ps->set_lex_strterm(NEW_STRTERM(str_ssym, c, 0, ps));
            break;
          case '"':
            ps->set_lex_strterm(NEW_STRTERM(str_dsym, c, 0, ps));
            break;
          default:
            pushback(c, ps);
            break;
        }
        SET_lexState( EXPR_FNAME);
        return tSYMBEG;

      case '/':
        if (IS_EXPR_BEG_or_MID(lex_state)) {
            ps->set_lex_strterm(NEW_STRTERM(str_regexp, '/', 0, ps));
            return tREGEXP_BEG;
        }
        if ((c = nextc(ps)) == '=') {
            startToken(ps);
            *ps->lexvalH = RpNameToken::s( a_sym_div, ps);
            SET_lexState( EXPR_BEG);
            return tOP_ASGN;
        }
        pushback(c, ps);
        if (IS_ARG(lex_state) && space_seen) {
            if (! isSpace(c, ps)) {
                arg_ambiguous(ps);
                ps->set_lex_strterm(NEW_STRTERM(str_regexp, '/', 0, ps));
                return tREGEXP_BEG;
            }
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return '/';

      case '^':
        if ((c = nextc(ps)) == '=') {
            startToken(ps);
            *ps->lexvalH = RpNameToken::s( a_sym_upArrow, ps);
            SET_lexState( EXPR_BEG);
            return tOP_ASGN;
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return '^';

      case ';':
        ps->command_start = TRUE;
      case ',':
        SET_lexState( EXPR_BEG);
        return c;

      case '~':
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
          if ((c = nextc(ps)) != '@') {
             pushback(c, ps);
          }
          UTL_ASSERT(IS_EXPR_FNAME_or_DOT(lex_state));
          SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return '~';

      case '(':
        ps->command_start = TRUE;
        if (IS_EXPR_BEG_or_MID(lex_state)) {
            c = tLPAREN;
        }
        else if (space_seen) {
            if (lex_state == EXPR_CMDARG) {
                c = tLPAREN_ARG;
            }
            else if (lex_state == EXPR_ARG) {
                rb_warning(ps, "don't put space before argument parentheses");
                c = '(';
            }
        }
        COND_PUSH(ps, 0);
        CMDARG_PUSH(ps, 0);
        SET_lexState( EXPR_BEG);
        return c;

      case '[':
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
            if ((c = nextc(ps)) == ']') {
                if ((c = nextc(ps)) == '=') {
                    *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
                    return tASET;
                }
                pushback(c, ps);
                *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
                return tAREF;
            }
            pushback(c, ps);
            return '[';
        }
        else if (IS_EXPR_BEG_or_MID(lex_state)) {
            c = tLBRACK;
        }
        else if (IS_ARG(lex_state) && space_seen) {
            c = tLBRACK;
        }
        SET_lexState( EXPR_BEG);
        COND_PUSH(ps, 0);
        CMDARG_PUSH(ps, 0);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return c;

      case '{':
        if (IS_ARG_or_END(lex_state))
            c = '{';          /* block (primary) */
        else if (lex_state == EXPR_ENDARG)
            c = tLBRACE_ARG;  /* block (expr) */
        else
            c = tLBRACE;      /* hash */
        COND_PUSH(ps, 0);
        CMDARG_PUSH(ps, 0);
        SET_lexState( EXPR_BEG);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return c;

      case '\\':
        c = nextc(ps);
        if (c == '\n') {
            space_seen = 1;
            goto retry; /* skip \\n */
        }
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return tUBS; // yields parse error in Maglev
// Unary backspace believed rubinius specific
//      pushback(c, ps);
//      if(lex_state == EXPR_BEG
//         || lex_state == EXPR_MID || space_seen) {
//         SET_lexState( EXPR_DOT);
//          return tUBS;
//      }
//      SET_lexState( EXPR_DOT);
//      return '\\';

      case '%':
        if (IS_EXPR_BEG_or_MID(lex_state)) {
            int term;
            int paren;
            char tmpstr[256];
            char *cur;

            c = nextc(ps);
          quotation:
            if (! isAlphaNumeric(c, ps)) {
                term = c;
                c = 'Q';
            }
            else {
                term = nextc(ps);
                if ( isAlphaNumeric(term, ps) || ismbchar(term)) {
                    cur = tmpstr;
                    *cur++ = c;
                    while( isAlphaNumeric(term, ps) || ismbchar(term)) {
                        *cur++ = term;
                        term = nextc(ps);
                    }
                    *cur = 0;
                    c = 1;

                }
            }
            if (c == -1 || term == -1) {
                rb_compile_error_override(ps, "unterminated quoted string meets end of file");
                return 0;
            }
            paren = term;
            if (term == '(') term = ')';
            else if (term == '[') term = ']';
            else if (term == '{') term = '}';
            else if (term == '<') term = '>';
            else paren = 0;

            switch (c) {
              case 'Q':
                ps->set_lex_strterm(NEW_STRTERM(str_dquote, term, paren, ps));
                return tSTRING_BEG;

              case 'q':
                ps->set_lex_strterm(NEW_STRTERM(str_squote, term, paren, ps));
                return tSTRING_BEG;

              case 'W':
                ps->set_lex_strterm(NEW_STRTERM(str_dquote | STR_FUNC_QWORDS, term, paren, ps));
                do {c = nextc(ps);} while ( isSpace(c, ps));
                pushback(c, ps);
                return tWORDS_BEG;

              case 'w':
                ps->set_lex_strterm(NEW_STRTERM(str_squote | STR_FUNC_QWORDS, term, paren, ps));
                do {c = nextc(ps);} while ( isSpace(c, ps));
                pushback(c, ps);
                return tQWORDS_BEG;

              case 'x':
                ps->set_lex_strterm(NEW_STRTERM(str_xquote, term, paren, ps));
                *ps->lexvalH = int64ToSi( 0);
                return tXSTRING_BEG;

              case 'r':
                ps->set_lex_strterm(NEW_STRTERM(str_regexp, term, paren, ps));
                return tREGEXP_BEG;

              case 's':
                ps->set_lex_strterm(NEW_STRTERM(str_ssym, term, paren, ps));
                SET_lexState( EXPR_FNAME);
                return tSYMBEG;

              case 1:
                ps->set_lex_strterm(NEW_STRTERM(str_xquote, term, paren, ps));
                *ps->lexvalH = rb_parser_sym(tmpstr, ps);
                return tXSTRING_BEG;

              default:
                ps->set_lex_strterm(NEW_STRTERM(str_xquote, term, paren, ps));
                tmpstr[0] = c;
                tmpstr[1] = 0;
                *ps->lexvalH = rb_parser_sym(tmpstr, ps);
                return tXSTRING_BEG;
            }
        }
        if ((c = nextc(ps)) == '=') {
            startToken(ps);
            *ps->lexvalH = RpNameToken::s( a_sym_percent, ps);
            SET_lexState( EXPR_BEG);
            return tOP_ASGN;
        }
        if (IS_ARG(lex_state) && space_seen && ! isSpace(c, ps)) {
            goto quotation;
        }
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
        } else {
            SET_lexState( EXPR_BEG);
        }
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
        return '%';

      case '$':
        last_state = lex_state;
        SET_lexState( EXPR_END);
        startToken(ps);
        c = nextc(ps);
        switch (c) {
          case '_':             /* $_: last read line string */
            c = nextc(ps);
            if (is_identchar(c, ps)) {
                tokadd('$', ps);
                tokadd('_', ps);
                break;
            }
            pushback(c, ps);
            c = '_';
            /* fall through */
          case '~':             /* $~: match-data */
            // local_cnt(ps, OOP_OF_SMALL_LONG_(c) ); // no side effects for _ , ~
            /* fall through */
          case '*':             /* $*: argv */
          case '$':             /* $$: pid */
          case '?':             /* $?: last status */
          case '!':             /* $!: error string */
          case '@':             /* $@: error position */
          case '/':             /* $/: input record separator */
          case '\\':            /* $\: output record separator */
          case ';':             /* $;: field separator */
          case ',':             /* $,: output field separator */
          case '.':             /* $.: last read line number */
          case '=':             /* $=: ignorecase */
          case ':':             /* $:: load path */
          case '<':             /* $<: reading filename */
          case '>':             /* $>: default output handle */
          case '\"':            /* $": already loaded files */
            tokadd('$', ps);
            tokadd((char)c, ps);
            tokfix(ps);
            *ps->lexvalH = rb_parser_sym(tok(ps), ps);
            return tGVAR;

          case '-':
            tokadd('$', ps);
            tokadd((char)c, ps);
            c = nextc(ps);
            tokadd((char)c, ps);
    HAVE_gvar:
            tokfix(ps);
            *ps->lexvalH = rb_parser_sym(tok(ps), ps);
            /* xxx shouldn't check if valid option variable */
            return tGVAR;

          case '&':             /* $&: last match */
          case '`':             /* $`: string before last match */
          case '\'':            /* $': string after last match */
          case '+':             /* $+: string matches last paren. */
	    if (last_state == EXPR_FNAME) {
		tokadd((char)'$', ps);
		tokadd(c, ps);
		goto HAVE_gvar;
	    }
            *ps->lexvalH = ramOop( GCI_CHR_TO_OOP(c));
            return tBACK_REF;

          case '1': case '2': case '3':
          case '4': case '5': case '6':
          case '7': case '8': case '9':
            tokadd('$', ps);
            do {
                tokadd((char)c, ps);
                c = nextc(ps);
            } while ( isDigit(c, ps));
            pushback(c, ps);
	    if (last_state == EXPR_FNAME) {
              goto HAVE_gvar;
            }
            tokfix(ps);
            { int anInt = atoi(tok(ps)+1);
              *ps->lexvalH = OOP_OF_SMALL_LONG_(anInt);
            }
            return tNTH_REF;

          default:
            if (!is_identchar(c, ps)) {
                pushback(c, ps);
                return '$';
            }
          case '0':
            tokadd('$', ps);
        }
        break;

      case '@':
        c = nextc(ps);
        startToken(ps);
        tokadd('@', ps);
        if (c == '@') {
            tokadd('@', ps);
            c = nextc(ps);
        }
        if ( isDigit(c, ps)) {
            char msg[128];
            if (ps->tokidx == 1) {
               snprintf(msg, sizeof(msg), "`@%c' is not allowed as an instance variable name", c);
               rb_compile_error(ps, msg);
            } else {
               snprintf(msg, sizeof(msg), "`@@%c' is not allowed as a class variable name", c);
               rb_compile_error(ps, msg);
            }
        }
        if (!is_identchar(c, ps)) {
            pushback(c, ps);
            return '@';
        }
        break;

      case '_':
        if (was_bol(ps) && whole_match_p("__END__", 7, 0, ps)) {
            ps->end_seen = 1;
            return 0; // rubinius returned -1; 
                // maglev returns 0 to avoid < 0 check in customized byacc state machine
        }
        startToken(ps);
        break;

      default:
        if (!is_identchar(c, ps)) {
            char msg[128];
            snprintf(msg, sizeof(msg), "Invalid char `\\%03o' in expression", c);
            rb_compile_error(ps, msg);
            goto retry;
        }

        startToken(ps);
        break;
    }

    do {
        tokadd((char)c, ps);
        if (ismbchar(c)) {
            int i, len = mbclen(c)-1;

            for (i = 0; i < len; i++) {
                c = nextc(ps);
                tokadd((char)c, ps);
            }
        }
        c = nextc(ps);
    } while (is_identchar(c, ps));
    if ((c == '!' || c == '?') && is_identchar(tok(ps)[0], ps) 
           && !peek('=', ps)) {
        tokadd((char)c, ps);
    }
    else {
        pushback(c, ps);
    }
    tokfix(ps);

    {
        int result = 0;
        BoolType needsNameToken = FALSE;

        last_state = lex_state;
        char tokFirstCh = tok(ps)[0] ;
        char tokLastCh;
        switch (tokFirstCh) {
          case '$':
            SET_lexState( EXPR_END);
            result = tGVAR;
            break;
          case '@':
            SET_lexState( EXPR_END);
            if (tok(ps)[1] == '@')
                result = tCVAR;
            else
                result = tIVAR;
            break;

          default:
            tokLastCh = toklast(ps);
            if (tokLastCh == '!' || tokLastCh == '?') {
                result = tFID;
                needsNameToken = TRUE;
            } else {
                if (lex_state == EXPR_FNAME) {
                    c = nextc(ps);
                    if (c  == '=' && ps->lex_p != ps->lex_pend ) {
                      int p_c = *(ps->lex_p) ; // actual peek
                      if (! ch_equals('~', p_c) && ! ch_equals('>', p_c) &&
                          (! ch_equals('=', p_c ) || 
                           (ps->lex_p + 1 < ps->lex_pend && (ps->lex_p)[1] == '>'))) {
                        result = tIDENTIFIER;
                        needsNameToken = TRUE;
                        tokadd((char)c, ps);
                        tokfix(ps);
                      } else {
                        pushback(c, ps);
                      }
                    } else {
                      pushback(c, ps);
                    }
                }
                if (result == 0 && isUpper( tokFirstCh , ps)) {
                    result = tCONSTANT;
                    needsNameToken = TRUE;
                } else {
                    result = tIDENTIFIER;
                    needsNameToken = TRUE;
                }
            }
            if ((lex_state == EXPR_BEG && !cmd_state) || IS_ARG(lex_state)) {
                int p_c = *(ps->lex_p); // actual peek
                if (ch_equals(':', p_c) && !(ps->lex_p + 1 < ps->lex_pend && (ps->lex_p)[1] == ':')) {
                    lex_state = EXPR_BEG;
                    nextc(ps);
                    NODE* symqO = rb_parser_sym( tok(ps) , ps); 
                    *ps->lexvalH = RpNameToken::s( ps, symqO );
                    return tLABEL;
                }
            }

            if (lex_state != EXPR_DOT) {
                // See if it is a reserved word. 
                const kwtable *kw = mel_reserved_word(tok(ps), toklen(ps));
                if (kw) {
                    int64 resWordOffset = ps->lineStartOffset + ps->tokStartDelta; // zero based
                    LexStateKind state = lex_state;
                    SET_lexState( kw->state);

                    omObjSType *srcOfs = OOP_OF_SMALL_LONG_(resWordOffset + 1); // one based
                    AstSymbolEType a_sym = kw->a_sym;
                    *ps->lexvalH = RpNameToken::s(a_sym, srcOfs, ps);
                    
                    int kwIdZero = kw->id[0];
                    if (state == EXPR_FNAME) {
                        // Hack. Ignore the different variants of do
                        // if we're just trying to match a FNAME
                        if (kwIdZero == kDO) return kDO;
                    }
                    if (kwIdZero == kDO) {
                        // ps->command_start = TRUE;   // rubinius Sep 20, 2010
                        if (COND_P(ps)) return kDO_COND;
                        if (CMDARG_P(ps) && state != EXPR_CMDARG)
                            return kDO_BLOCK;
                        if (state == EXPR_ENDARG)
                            return kDO_BLOCK;
                        return kDO;
                    }
                    if (state == EXPR_BEG) {
                        return kwIdZero;
                    }
                    int kwIdOne = kw->id[1];
                    if (kwIdZero != kwIdOne) {
			SET_lexState( EXPR_BEG);
		    }
                    return kwIdOne;
                }
            }

            if (IS_EXPR_BEG_or_MID_DOT_ARG_CMDARG(lex_state)) {
                if (cmd_state) {
                    SET_lexState( EXPR_CMDARG);
                }
                else {
                    SET_lexState( EXPR_ARG);
                }
            }
            else {
                SET_lexState( EXPR_END);
            }
        }
        NODE* symqO = rb_parser_sym( tok(ps) , ps); 
        // symqO is a SmallInteger always
        if (needsNameToken) {
          *ps->lexvalH = RpNameToken::s( ps, symqO );
        } else {
          *ps->lexvalH = symqO;
        }
        if (is_local_id(symqO) && 
            last_state != EXPR_DOT && 
            local_id(ps, symqO)) {
          SET_lexState( EXPR_END);
        }

//         if (is_local_id(pslval->id) && local_id(pslval->id)) {  // commented out in Ribinius
//             SET_lexState( EXPR_END); 
//         } 

        return result;
    }
  /* end of yylex*/
} 

static int lexPlusMinus(rb_parse_state* ps, int space_seen, int aResult, int unaryResult)
{
  // result is either aResult, unaryResult, or -1
   UTL_ASSERT(unaryResult == tUPLUS || unaryResult == tUMINUS);
   UTL_ASSERT(aResult == '+' || aResult == '-');
   UTL_ASSERT((aResult == '+' ) == (unaryResult == tUPLUS));

        LexStateKind lex_state = ps->lex_state;   
        int c = nextc(ps);
        if (IS_EXPR_FNAME_or_DOT(lex_state)) {
            SET_lexState( EXPR_ARG);
            if (c == '@') {
              *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
              return unaryResult;
            }
            pushback(c, ps);
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
            return aResult;
        }
        if (c == '=') {
            startToken(ps);
            *ps->lexvalH = RpNameToken::s( aResult == '+' ? a_sym_plus : a_sym_minus , ps);
            SET_lexState( EXPR_BEG);
            return tOP_ASGN;
        }
        if (IS_EXPR_BEG_or_MID(lex_state) ||
            (IS_ARG(lex_state) && space_seen && ! isSpace(c, ps))) {
            int isArg = IS_ARG(lex_state);
            if (isArg) {
              arg_ambiguous(ps);
            }
            SET_lexState( EXPR_BEG);
            pushback(c, ps);
            if (aResult == '+') {
              if ( isDigit(c, ps)) {
                // c = aResult ; // not needed, caller's  c  not changed yet
                return -1; // caller does Goto start_num;
              }
            } else {
              if ( isDigit(c, ps)) {
                return tUMINUS_NUM;
              }
            }
            *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset()); // srcOffsetSi
            return isArg ? aResult : unaryResult; // Maglev fix Trac 567 for 1.8.7
        }
        SET_lexState( EXPR_BEG);
        pushback(c, ps);
        *ps->lexvalH = OOP_OF_SMALL_LONG_( ps->tokenOffset());
        return aResult;
}

#undef SET_lexState
// end of code which might change ps->lex_state

static NODE* asQuid(NODE* idO,  rb_parse_state *ps)
{
  if (! OOP_IS_SMALL_INT(idO)) {
    idO = RpNameToken::fetchQuidO(idO, ps);
  };
  return idO;
}

static NODE* gettable(rb_parse_state *ps, NODE** idH)
{
  NODE *srcOffsetSi = ram_OOP_NIL;
  NODE* idO = *idH;
  if (! OOP_IS_SMALL_INT(idO)) {
    idO = RpNameToken::fetchQuidO(idO, ps);
    srcOffsetSi = RpNameToken::srcOffsetO_noCheck(ps, *idH);
  }
  if (OOP_IS_SMALL_INT(idO)) { // expect a  QUID
    int64 id = OOP_TO_I64(idO);
    if (id < tLAST_TOKEN) {
      if (id == kSELF) {
	  return RubySelfNode::new_(ps);
      }
      else if (id == kNIL) {
	  return RubyNilNode::new_(ps);
      }
      else if (id == kTRUE) {
	  return RubyTrueNode::new_(ps);
      }
      else if (id == kFALSE) {
	  return RubyFalseNode::new_(ps);
      }
      else if (id == k__FILE__) {
	  return RubyStrNode::s( *ps->fileNameH , ps);
      }
      else if (id == k__LINE__) {
	  return RubyAbstractNumberNode::s( ps->ruby_sourceline(), ps);
      }
    }
    if (v_is_local_id(id)) {
        if (eval_local_id(ps, idO)) {
          return RubyLocalVarNode::s( quidToSymbolObj(idO, ps), ps);
        }
        /* method call without arguments */
        om *omPtr = ps->omPtr;
        OmScopeType aScope(omPtr);
        NODE **selfH = aScope.add( RubySelfNode::new_(ps));
        return RubyParser::new_vcall( *selfH, *idH, ps);
    }
    else if (v_is_global_id(id)) {
        return RubyGlobalVarNode::s( quidToSymbolObj(idO, ps), ps);  
    }
    else if (v_is_instance_id(id)) {
        return RubyInstVarNode::s( quidToSymbolObj(idO, ps), ps);
    }
    else if (v_is_const_id(id)) {
        return RubyColon2Node::sym( quidToSymbolObj(idO, ps), srcOffsetSi, ps);
    }
    else if (v_is_class_id(id)) {
        return RubyClassVarNode::s( quidToSymbolObj( idO, ps), ps);
    }
  }
  rb_compile_error_q(ps, "identifier is not valid 1\n", idO);
  return ram_OOP_NIL;
}

static void reset_block(rb_parse_state *parse_state) 
{
  LocalState *vars = parse_state->variables;
  if (vars->block_vars == NULL) {
    vars->block_vars = VarTable::allocate(parse_state, 5);
  } else {
    vars->block_vars = VarTable::push(parse_state, vars->block_vars);
  }
}

#if 0
NOT USED
static NODE* getBlockVars(rb_parse_state *ps)
{
  LocalState *vars = ps->variables;
  VarTable *block_vars = vars->block_vars;
  if (block_vars == NULL) {
    return om::NewArray(ps->omPtr, 0);
  }
  return block_vars->asArrayOfSymbols(ps);
}
#endif

NODE*  VarTable::asArray(om *omPtr)
{
  // returns an Array of QUID
  OmScopeType aScope(omPtr);
  int64 sz = this->size;
  NODE **resH = aScope.add(om::NewArray(omPtr, sz));
  QUID *lst = this->list ;
  for (int64 j = 0; j < sz; j++) {
    om::StoreSpecial(omPtr, resH, j, lst[j]);
  }
  return *resH;
}

int64 VarTable::add(rb_parse_state *ps, QUID id)
{
  // result is new size
  if (size >= allocatedSize) {
    UTL_ASSERT(size == allocatedSize);
    grow(ps);
  }
  UTL_ASSERT(size < allocatedSize);
  list[size] = id;
  size += 1;
  return size;
}

void VarTable::removeLast()
{
  UTL_ASSERT(size >= 1);
  if (size >= 1) {
    size -= 1;
  }
}

void VarTable::grow(rb_parse_state *ps)
{
  QUID *nList = (QUID*)ComHeapMalloc(&ps->omPtr->workspace()->compilerState, sizeof(QUID) * allocatedSize * 2);
  memcpy(nList, list, sizeof(QUID) * this->size);
  allocatedSize = allocatedSize * 2;
  list = nList;
}

#if 0
NOT USED
NODE*  VarTable::asArrayOfSymbols(rb_parse_state *ps) 
{
  // returns an Array of Symbols
  om *omPtr = ps->omPtr;
  OmScopeType aScope(omPtr);
  int64 sz = this->size;
  NODE **resH = aScope.add(om::NewArray(omPtr, sz));
  NODE **valH = aScope.newHandle();
  QUID *lst = this->list;
  for (int64 j = 0; j < sz; j++) {
    OopType q = lst[j]; // a SmallInt
    *valH = quidToSymbolObj((NODE*)q, ps);
    om::StoreOop(omPtr, resH, j, valH);
  }
  return *resH;
}
#endif

static void popBlockVars(rb_parse_state *ps)
{
  // replaces Rubinius extract_block_vars 
  LocalState* vars = ps->variables;
  vars->block_vars = vars->block_vars->pop() ;
}


static NODE* assignable(NODE **idH, NODE* srcOffsetArg, NODE **valH, rb_parse_state *ps)
{
  // val = value_expr(val);    rubinius had this
  NODE *idO = *idH;
  NODE* srcOffset = srcOffsetArg;
  if (OOP_IS_RAM_OOP(idO)) {
    idO = RpNameToken::fetchQuidO(idO, ps);
    srcOffset = RpNameToken::srcOffsetO_noCheck(ps, *idH);
  } else {
    srcOffset = srcOffsetArg;
  }
  if (OOP_IS_SMALL_INT(idO)) { // expect a  QUID
    int64 id = OOP_TO_I64(idO);
    if (id == kSELF) {
        rb_compile_error("Can't change the value of self", ps);
    }
    else if (id == kNIL) {
        rb_compile_error("Can't assign to nil", ps);
    }
    else if (id == kTRUE) {
        rb_compile_error("Can't assign to true", ps);
    }
    else if (id == kFALSE) {
        rb_compile_error("Can't assign to false", ps);
    }
    else if (id == k__FILE__) {
        rb_compile_error("Can't assign to __FILE__", ps);
    }
    else if (id == k__LINE__) {
        rb_compile_error("Can't assign to __LINE__", ps);
    }
    else if (v_is_local_id(id)) {
        VarTable *block_vars = ps->variables->block_vars;
        if (block_vars) {
          block_vars->add(ps, (QUID)idO);  // var_table_add
        }
        local_cnt(ps, idO);
        NODE *symO = quidToSymbolObj(idO, ps);
        return RubyLocalAsgnNode::s( symO, srcOffset, *valH, ps);
    }
    else if (v_is_global_id(id)) {
        NODE *symO = quidToSymbolObj(idO, ps);
        return RubyGlobalAsgnNode::s(symO, srcOffset, *valH, ps);
    }
    else if (v_is_instance_id(id)) {
        NODE *symO = quidToSymbolObj(idO, ps);
        return RubyInstAsgnNode::s(symO, srcOffset, *valH, ps);
    }
    else if (v_is_const_id(id)) {
        if (ps->in_def || ps->in_single) {
            rb_compile_error_q(ps, "dynamic constant assignment", idO);
        }
        NODE *symO = quidToSymbolObj(idO, ps);
        UTL_ASSERT(OOP_IS_SMALL_INT(srcOffset));
        return RubyConstDeclNode::sym(symO, srcOffset, *valH, ps);
    }
    else if (v_is_class_id(id)) {
        // Maglev :cvasgn , :cvdecl  both have same AST node
        // if (ps->in_def || ps->in_single) {
        //  return NEW_CVASGN(id, val);
        // }
        NODE *symO = quidToSymbolObj(idO, ps);
        return RubyClassVarDeclNode::s(symO, srcOffset, *valH, ps);
    }
  }
  rb_compile_error(ps, "identifier is not valid 2\n");
  return ram_OOP_NIL;
}


static void rb_backref_error(NODE *node, rb_parse_state *parse_state)
{
   RubyParser::backref_error(node, parse_state);
}


static void local_push(rb_parse_state *st, int top)
{
    st->variables = LocalState::push(st, st->variables);
}

static void local_pop(rb_parse_state *st)
{
    st->variables = LocalState::pop(st->variables);
}


static int64 var_table_find(VarTable *tbl, QUID id)
{
  QUID *lst = tbl->list;
  for (int64 j = 0; j < tbl->size; j++) {
    if (lst[j] == id) {
      return j;
    }
  }
  return -1;
}

static int64 var_table_find_chained(VarTable *tbl, QUID id)
{
  int64 k = var_table_find(tbl, id);
  if (k >= 0)
    return k;

  VarTable *next = tbl->next;
  if (next != NULL) {
    return var_table_find_chained(next, id);
  }
  return -1;
}

static int64 var_table_add(rb_parse_state *ps, VarTable *tbl, QUID id)
{
  return tbl->add(ps, id);
}

static int64 local_cnt(rb_parse_state *st, NODE *quidO)
{
    UTL_ASSERT(OOP_IS_SMALL_INT(quidO));
    QUID qid = (OopType)quidO;
    UTL_DEBUG_DEF( int64 id = OOP_TO_I64(qid); )
    UTL_ASSERT(id != '_' && id != '~');

    // if there are block variables, check to see if there is already
    // a local by this name. If not, create one in the top block_vars
    // table.
    LocalState *vars = st->variables;
    if (vars->block_vars) {
      int64 idx = var_table_find_chained(vars->block_vars, qid);
      if (idx >= 0) {
        return idx;
      } else {
        return var_table_add(st, vars->block_vars, qid);
      }
    }

    int64 idx = var_table_find(vars->variables, qid);
    if (idx >= 0) {
      return idx + 2;
    }

    return var_table_add(st, vars->variables, qid);
}

static int local_id(rb_parse_state *st, NODE* idO)
{
  UTL_ASSERT(OOP_IS_SMALL_INT(idO));
  QUID qid = (OopType)idO;
  LocalState *vars = st->variables;
  if (vars->block_vars) {
    if (var_table_find_chained(vars->block_vars, qid) >= 0) {
      return 1;
    }
  }
  return var_table_find(vars->variables, qid) >= 0 ;
}

static int eval_local_id(rb_parse_state *st, NODE* idO)
{
  if (local_id(st, idO))
    return 1;

  omObjSType **evalScopeH = st->evalScopeH;
  if (evalScopeH != NULL && st->variables->prev == NULL) {
    // parsing an eval, and we do not have an active local_push()
    omObjSType *symO = quidToSymbolObj(idO, st);
    omObjSType *isLocal = RubyNode::call(*evalScopeH, sel_includesTemp_, symO, st);
    if (isLocal == ram_OOP_TRUE) {
      return 1;
    }
  }
  return 0;
}

static const struct {
    int token;
    const char name[12];
    AstSymbolEType a_sym;
} op_tbl[] = {
    {tDOT2,     "..", a_sym_dot2    },
    {tDOT3,     "...", a_sym_dot3  },
    {'+',       "+", a_sym_plus  },
    {'-',       "-", a_sym_minus  },
    {'+',       "+(binary)", a_sym_plus  },
    {'-',       "-(binary)", a_sym_minus  },
    {'*',       "*", a_sym_star  },
    {'/',       "/", a_sym_div  },
    {'%',       "%", a_sym_percent  },
    {tPOW,      "**", a_sym_tPOW  },
    {tUPLUS,    "+@", a_sym_tUPLUS  },
    {tUMINUS,   "-@", a_sym_tUMINUS  },
    {tUPLUS,    "+(unary)", a_sym_tUPLUS  },
    {tUMINUS,   "-(unary)", a_sym_tUMINUS  },
    {'|',       "|", a_sym_orOp  },
    {'^',       "^", a_sym_upArrow  },
    {'&',       "&", a_sym_andOp  },
    {tCMP,      "<=>", a_sym_tCMP  },
    {'>',       ">", a_sym_gt  },
    {tGEQ,      ">=", a_sym_tGEQ  },
    {'<',       "<", a_sym_lt},
    {tLEQ,      "<=", a_sym_tLEQ  },
    {tEQ,       "==", a_sym_tEQ  },
    {tEQQ,      "===", a_sym_tEQQ  },
    {tNEQ,      "!=",  a_sym_tNEQ   },
    {tMATCH,    "=~", a_sym_tMATCH   },
    {tNMATCH,   "!~", a_sym_tNMATCH  },
    {'!',       "!", a_sym_bang  },
    {'~',       "~", a_sym_tilde  },
    {'!',       "!(unary)", a_sym_bang  },
    {'~',       "~(unary)", a_sym_tilde  },
    {'!',       "!@", a_sym_bang  },
    {'~',       "~@", a_sym_tilde  },
    {tAREF,     "[]", a_sym_tAREF  },
    {tASET,     "[]=", a_sym_tASET  },
    {tLSHFT,    "<<", a_sym_tLSHFT  },
    {tRSHFT,    ">>", a_sym_tRSHFT  },
    {tCOLON2,   "::", a_sym_colon2  },
    {'`',       "`", a_sym_backtick  },
    {0, "",           a_sym_INVALID  }
};


static NODE* rb_parser_sym(const char *name, rb_parse_state *ps)
{
   // returns a SmallInteger containing OopNumber of a Symbol
   //  and the  ruby token type per ID_TOK_MASK in rubyparser.h
    const char *m = name;
    int64 id = 0;
    int64 tval ;
    int64 lastIdx = strlen(name)-1;
    AstSymbolEType a_sym = a_sym_INVALID;
    char firstChar = name[0];
    switch (firstChar) {
      case '$':
        id = ID_GLOBAL;
        m++;
        if (! is_identchar(*m, ps)) m++;
        break;
      case '@':
        if (name[1] == '@') {
            m++;
            id = ID_CLASS;
        }
        else {
            id = ID_INSTANCE;
        }
        m++;
        break;
      default:
        if (isAlpha(firstChar, ps) && firstChar != '_' && !ismbchar(name[0])) {
            int64 i = 0;
            for (;;) {
              const char* tblName = op_tbl[i].name;
              char tbFirstCh = tblName[0];
              if (tbFirstCh == '\0') 
                break;
              if (tbFirstCh == firstChar && strcmp(tblName, name) == 0) {
                tval = op_tbl[i].token;
                a_sym = op_tbl[i].a_sym ;
                UTL_ASSERT(id == 0);
                goto haveOperator;
              }
              i += 1;
            }
        }
        if (name[lastIdx] == '=') {
            id = ID_ATTRSET;
        } else if ( isUpper(firstChar, ps)) {
            id = ID_CONST;
        } else {
            id = ID_LOCAL;
        }
        break;
    }
    while (m <= (name + lastIdx)  && is_identchar(*m, ps)) {
        m += mbclen(*m);
    }
    if (*m) { 
      id = ID_JUNK;
    }
    tval = tLAST_TOKEN + 1 ;
haveOperator: ;
    NODE *symO;
    if (a_sym != a_sym_INVALID) {
      symO = om::FetchOop(*ps->astSymbolsH, a_sym);
      UTL_ASSERT(symO != ram_OOP_NIL);
    } else {
      symO = ObjCanonicalSymFromCStr(ps->omPtr, (ByteType*)name, lastIdx + 1,
					       OOP_NIL);
    };
    OopType symId = om::objIdOfObj(symO);
    return RpNameToken::buildQuid(symId, tval, id);
}

static uint64 scan_oct(const char *start, int len, int *retlen)
{
    const char *s = start;
    uint64 retval = 0;

    while (len-- && *s >= '0' && *s <= '7') {
        retval <<= 3;
        retval |= *s++ - '0';
    }
    *retlen = s - start;
    return retval;
}

static uint64 scan_hex(const char *start, int len, int *retlen)
{
    static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
    const char *s = start;
    uint64 retval = 0;
    const char *tmp;

    while (len-- && *s && (tmp = strchr(hexdigit, *s))) {
        retval <<= 4;
        retval |= (tmp - hexdigit) & 15;
        s++;
    }
    *retlen = s - start;
    return retval;
}

static void nameForToken(int tok, char *msg, size_t msgSize)
{
  const char* nam = yyname[tok];
  if (nam != NULL) {
    snprintf(msg, msgSize, "%s ", nam);
  } else if (tok >= 20 && tok <= '~' ) {
    snprintf(msg, msgSize, "'%c' ", tok);
  } else {
    snprintf(msg, msgSize, "\\x%x ", tok);
  }
}

static void yyStateError(int64 yystate, int yychar, rb_parse_state*ps)
{
  if (ps->firstErrorLine == -1 &&
      ps->firstErrorReason[0] == '\0') {
    ps->firstErrorLine = ps->lineNumber;
    const short *unifiedTable = yyUnifiedTable;
    int expectedToks[YYMAXTOKEN + 1];  // chars or lexer token values
    int expCount = 0;
    int shiftB = unifiedTable[yystate + sindexBASE];
    int reduceB = unifiedTable[yystate + rindexBASE];
    for (int ch = 0; ch <= YYMAXTOKEN; ch++) {
      int x = shiftB + ch;
      if (x <= YYTABLESIZE) {
	int yChk = unifiedTable[x + checkBASE];
	if (yChk == ch) {
	  expectedToks[expCount] = ch; // would shift
	  expCount += 1;
	} else {
          x = reduceB + ch;
          if (x <= YYTABLESIZE) {
	    yChk = unifiedTable[x + checkBASE];
	    if (yChk == ch) {
	      expectedToks[expCount] = ch; // would reduce
	      expCount += 1;
	    }  
          }
        }
      }
    }
    char tokName[64];
    nameForToken(yychar, tokName, sizeof(tokName));
    if (expCount >= 1 && expectedToks[0] == 0) { // expected EOF
      expCount = 1; // ignore the other expected tokens
      snprintf(ps->firstErrorReason, sizeof(ps->firstErrorReason),
          "syntax error, unexpected %s expecting EOF ", tokName);
    } else if (expCount > 7) {
      expCount = 0; // too many to print
      snprintf(ps->firstErrorReason, sizeof(ps->firstErrorReason),
          "syntax error, unexpected %s ", tokName);
    } else {
      const char* sMsg = expCount == 0 ? "details not available"
             : (expCount > 1 ? "expected one of " : "expected " );		
      snprintf(ps->firstErrorReason, sizeof(ps->firstErrorReason),
          "syntax error, found %s , %s" , tokName, sMsg);
    }
    for (int j = 0; j < expCount; j++) {
      int tok = expectedToks[j];
      nameForToken(tok, tokName, sizeof(tokName));
      strlcat(ps->firstErrorReason, tokName, sizeof(ps->firstErrorReason));
    }
    yyerror(ps->firstErrorReason, ps);
  } else {
    yyerror("syntax error", ps);
  }
}

/* # line 9198 "rubygrammar.c" */ 

#if YYDEBUG
#include <stdio.h>		/* needed for printf */
#endif

#include <stdlib.h>	/* needed for malloc, etc */
#include <string.h>	/* needed for memset */




static int yyparse(rb_parse_state* vps)
{
    int      yyerrflag; /* named yyerrstatus in bison*/ 
    int      yychar;
    const short *unifiedTable = yyUnifiedTable; 

    /* variables for the parser stack */
    YyStackData *yystack = &vps->yystack ;
    int64 yym, yyn, yystate;

    yynerrs = 0;
    yyerrflag = 0;
    yychar = YYEMPTY;

    yystate = 0;
    UTL_ASSERT(yystack->stacksize > 0);
    YyStackElement* yymarkPtr = yystack->base;
    yystack->mark = yymarkPtr;
    yymarkPtr->state = yystate;
    yymarkPtr->obj = ram_OOP_NIL;

yyloop:
    yyn = unifiedTable[yystate + defredBASE]/*yydefred[yystate]*/ ;
    if (yyn != 0) {
      goto yyreduce;
    }
    if (yychar < 0) {
        yychar = yylex(vps); 
        UTL_ASSERT(yychar >= 0); 
#if YYDEBUG
        if (yydebug)
        {
            const char *yys = NULL;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %ld, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
            yytrap();
        }
#endif
    }
    yyn = unifiedTable[yystate + sindexBASE] /*yysindex[x]*/;
    if (yyn) { 
      yyn += yychar; 
      if ((uint64)yyn <= YYTABLESIZE) {
        int yChk = unifiedTable[yyn + checkBASE]; /*yycheck[yyn]*/ 
        if (yChk == yychar) {
          int64 new_state = unifiedTable[yyn + tableBASE]; /* yytable[yyn]*/
#if YYDEBUG
          if (yydebug) { 
            printf("%sdebug: state %ld, shifting to state %ld\n",
                    YYPREFIX, yystate, new_state );
          }
#endif
        if (yymarkPtr >= yystack->last) {
          yymarkPtr = yygrowstack(vps, yymarkPtr); 
          if (yymarkPtr == NULL) { 
            yyerror("yacc stack overflow", vps);
            return 1;
          } 
        }
        yystate = new_state; 
        yymarkPtr += 1; 
        yystack->mark = yymarkPtr ; 
        yymarkPtr->state = yystate ; 
        yymarkPtr->obj = *vps->lexvalH ;
        yychar = YYEMPTY;
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }}}
    yyn = unifiedTable[yystate + rindexBASE]/*yyrindex[x]*/ ; 
    if (yyn) {
      yyn += yychar; 
      if ((uint64)yyn <= YYTABLESIZE) {
         int yChk = unifiedTable[yyn + checkBASE]; /*yycheck[yyn]*/
         if (yChk == yychar) {
           yyn = unifiedTable[yyn + tableBASE]; /*yytable[yyn]*/ 
           goto yyreduce;
    }}}
    if (yyerrflag) goto yyinrecovery;

    yyStateError(yystate, yychar, vps);

    goto yyerrlab;

yyerrlab:
    ++yynerrs;

yyinrecovery:
  if (yyerrflag < 3) {
    yyerrflag = 3;
    for (;;) {
      yyn = unifiedTable[yymarkPtr->state + sindexBASE]/*yysindex[x]*/;
      if (yyn) { 
        yyn += YYERRCODE;
        if (yyn >= 0) {
          if ((uint64)yyn <= YYTABLESIZE) {
            int yChk = unifiedTable[yyn + checkBASE]; /*yycheck[yyn]*/
            if (yChk == YYERRCODE) {
              int64 new_state = unifiedTable[yyn + tableBASE]; /*yytable[yyn]*/
#if YYDEBUG
              if (yydebug) {
                  printf("%sdebug: state %d, error recovery shifting to state %ld\n",
                           YYPREFIX, yymarkPtr->state, new_state);
              }
#endif
              if (yymarkPtr >= yystack->last) {
                  yymarkPtr = yygrowstack(vps, yymarkPtr); 
                  if (yymarkPtr == NULL) {
                    yyerror("yacc stack overflow", vps);
                    return 1;
                  } 
              }
              yystate = new_state;
              yymarkPtr += 1; 
              yystack->mark = yymarkPtr ; 
              yymarkPtr->state = yystate ; 
              yymarkPtr->obj = *vps->lexvalH ; 
              goto yyloop;
      }}}}
#if YYDEBUG
          if (yydebug) { 
             printf("%sdebug: error recovery discarding state %d \n",
                            YYPREFIX, yymarkPtr->state);
          } 
#endif
          if (yymarkPtr <= yystack->base) {
            yTrace(vps, "yyabort");
            return 1; 
          }
          yymarkPtr -= 1; 
          yystack->mark = yymarkPtr; 
        }
  } else { 
        if (yychar == 0) {
          yTrace(vps, "yyabort");
          return 1;
        }
#if YYDEBUG
        if (yydebug) {
            const char* yys = NULL;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %ld, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = YYEMPTY;
        goto yyloop;
    }

yyreduce:
#if YYDEBUG
    if (yydebug) { 
        printf("%sdebug: state %ld, reducing by rule %ld (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
    }
#endif
    yym = unifiedTable[yyn + lenBASE] /*yylen[yyn]*/ ;
    YyStackElement* yyvalPtr; 
    if (yym) {
       UTL_ASSERT( yymarkPtr == yystack->mark );
       yyvalPtr = yymarkPtr + 1 - yym ; 
    } else {
       yyvalPtr = NULL; // was memset(&yyval, 0, sizeof yyval)
    } 
    NODE*  yyvalO = NULL; 
    switch (yyn) {
      /* no default: in this switch */
case 1:
/* # line 660 "grammar.y" */ 
	{
                        yTrace(vps,  "program: " );
                        vps->lex_state = EXPR_BEG;
                        vps->variables = LocalState::allocate(vps);
                        vps->class_nest = 0;
                    }
break;
case 2:
/* # line 667 "grammar.y" */ 
	{
                        /*if ($2 && !compile_for_eval) ... */
                        /*     last expression should not be void  ...*/
                        /*    maglev does this in AST to IR generation */
                        yTrace(vps,  "program: comp_stamt");
                        vps->class_nest = 0;
                        yyvalO  =  yymarkPtr[0].obj;
                    }
break;
case 3:
/* # line 681 "grammar.y" */ 
	{
                        yTrace(vps, "body_stamt: comp_stamt ");
                        OmScopeType scp(vps->omPtr);
                        NODE **resH = scp.add(yymarkPtr[-3].obj);
                        if (yymarkPtr[-2].obj != ram_OOP_NIL) {
                            *resH = RubyRescueNode::s(yymarkPtr[-3].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, ram_OOP_NIL, vps);
                        } else if (yymarkPtr[-1].obj != ram_OOP_NIL) {
                            rb_warning(vps, "else without rescue is useless");
                            *resH = RubyParser::block_append(*resH, yymarkPtr[-1].obj, vps);
                        }
                        if (yymarkPtr[0].obj != ram_OOP_NIL) {  /* 4 is a RubyEnsureNode*/
                            /* $4 is receiver block of rubyEnsure:*/
                            RubyEnsureNode::set_body( yymarkPtr[0].obj, *resH, vps ); 
                            *resH = yymarkPtr[0].obj;
                        }
                        yyvalO = *resH;
                        /* fixpos($$, $1);*/
                    }
break;
case 4:
/* # line 702 "grammar.y" */ 
	{
                        /* void_stmts($1, vps);*/
                      yTrace(vps, "comp_stamt: sttmts opt_termms");
                        yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 6:
/* # line 711 "grammar.y" */ 
	{
                        /* $$  =  newline_node(vps, $1);*/
                        yyvalO = yymarkPtr[0].obj; /* maglev does not use newline nodes*/
                    }
break;
case 7:
/* # line 716 "grammar.y" */ 
	{
                        /* $$  =  block_append(vps, $1, newline_node(vps, $3));*/
                        yTrace(vps, "sttmts: | sttmts terms stmt ");
                        yyvalO = RubyParser::block_append(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 8:
/* # line 722 "grammar.y" */ 
	{
                        /* $$  = remove_begin($2, vps);*/
                      yTrace(vps, "sttmts: | error stmt");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 9:
/* # line 729 "grammar.y" */ 
	{vps->lex_state = EXPR_FNAME;}
break;
case 10:
/* # line 730 "grammar.y" */ 
	{
                        /* $$  = NEW_ALIAS($2, $4);*/
                      yTrace(vps, "stmt: kALIAS fitem");
                      yTrace(vps, "stmt: fitem");
                        yyvalO = RubyAliasNode::s(& yymarkPtr[-2].obj, & yymarkPtr[0].obj, yymarkPtr[-3].obj/*alias token*/, vps);
                    }
break;
case 11:
/* # line 737 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | kALIAS tGVAR tGVAR");
                        OmScopeType aScope(vps->omPtr);
                        NODE **aH = aScope.add( quidToSymbolObj(yymarkPtr[-1].obj, vps));
                        NODE **bH = aScope.add( quidToSymbolObj(yymarkPtr[0].obj, vps));
                        yyvalO = RubyGlobalVarAliasNode::s(*aH, *bH, vps);
                    }
break;
case 12:
/* # line 745 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | kALIAS tGVAR tBACK_REF");
                        char buf[3];
                        uint ch = 'x';
                        if (OOP_IS_CHARACTER(yymarkPtr[0].obj)) {
                          ch = OOP_TO_CHAR_(yymarkPtr[0].obj);
                        } else {
                          rb_compile_error(vps, "invalid tBACK_REF value in kALIAS tGVAR tBACK_REF");
                        }
                        snprintf(buf, sizeof(buf), "$%c", (char)ch );
                        om *omPtr = vps->omPtr;
                        OmScopeType aScope(omPtr);
                        NODE **symH = aScope.add( ObjNewSym(omPtr, buf));
                        NODE **aH = aScope.add( quidToSymbolObj(yymarkPtr[-1].obj, vps));
                        yyvalO = RubyGlobalVarAliasNode::s( *aH, *symH, vps);
                    }
break;
case 13:
/* # line 762 "grammar.y" */ 
	{
                        rb_compile_error(vps, "can't make alias for the number variables");
                        yyvalO = 0;
                    }
break;
case 14:
/* # line 767 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | kUNDEF undef_list");
                        yyvalO = yymarkPtr[0].obj;
                    }
break;
case 15:
/* # line 772 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kIF_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_if( yymarkPtr[0].obj , yymarkPtr[-2].obj, ram_OOP_NIL, srcOfs, vps );
                    }
break;
case 16:
/* # line 778 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kWHILE_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_if( yymarkPtr[0].obj, ram_OOP_NIL, yymarkPtr[-2].obj , srcOfs, vps );
                    }
break;
case 17:
/* # line 784 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kWHILE_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_while( yymarkPtr[-2].obj , yymarkPtr[0].obj , srcOfs, vps);
                    }
break;
case 18:
/* # line 790 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kUNTIL_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_until( yymarkPtr[-2].obj , yymarkPtr[0].obj , srcOfs, vps);
                    }
break;
case 19:
/* # line 796 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kRESCUE_MOD stmt");
                        OmScopeType aScope(vps->omPtr);
                        omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                        NODE **rescueBodyH = aScope.add( 
                          RubyRescueBodyNode::s(ram_OOP_NIL, yymarkPtr[0].obj, ram_OOP_NIL, srcOfs, vps));
                        yyvalO = RubyRescueNode::s( yymarkPtr[-2].obj, *rescueBodyH, ram_OOP_NIL, srcOfs, vps);
                    }
break;
case 20:
/* # line 805 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | klBEGIN");
                        if (vps->in_def || vps->in_single) {
                            rb_compile_error(vps, "BEGIN in method");
                        }
                        local_push(vps, 0);
                    }
break;
case 21:
/* # line 813 "grammar.y" */ 
	{
                       /* ruby_eval_tree_begin = block_append(ruby_eval_tree_begin, NEW_PREEXE($4));*/
                       yTrace(vps, "stmt: ___ tLCURLY comp_stamt tRCURLY");
                       rParenLexPop(vps);
                       local_pop(vps);
                       yyvalO = ram_OOP_NIL ;
                    }
break;
case 22:
/* # line 821 "grammar.y" */ 
	{
                       yTrace(vps, "stmt: | klEND tLCURLY comp_stamt tRCURLY");
                       rParenLexPop(vps);
                       if (vps->in_def || vps->in_single) {
                            rb_warning(vps, "END in method; use at_exit");
                       }
                       yyvalO = RubyIterRpNode::s(ram_OOP_NIL/*no block args*/, yymarkPtr[-1].obj, yymarkPtr[-2].obj/*srcOffsetSi*/, 
						vps, 1/* strlen( '}' ) */ );
                    }
break;
case 23:
/* # line 831 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | lhs tEQL command_call");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::node_assign(& yymarkPtr[-2].obj, srcOfs, yymarkPtr[0].obj, vps);
                    }
break;
case 24:
/* # line 837 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | mLhs tEQL command_call");
                        yyvalO = RubyParser::masgn_append_arg( yymarkPtr[-2].obj , yymarkPtr[0].obj, vps );
                    }
break;
case 25:
/* # line 842 "grammar.y" */ 
	{
                        if (yymarkPtr[-2].obj != ram_OOP_NIL) {
                           yTrace(vps, "stmt: | varLhs tOP_ASGN command_call");
                           yyvalO = RubyParser::new_op_asgn(yymarkPtr[-2].obj, yymarkPtr[-1].obj/*RpNameToken*/, yymarkPtr[0].obj, vps);
                        } else {
                           yTrace(vps, "stmt: | NIL_LHS tOP_ASGN command_call");
                           yyvalO = ram_OOP_NIL;
                        }
                    }
break;
case 26:
/* # line 852 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | primary_value tLBRACK_STR aref__args tRBRACK tOP_ASGN command_call");
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[-2].obj, 0);
                      yyvalO = RubyOpElementAsgnNode::s(yymarkPtr[-3].obj, aref_args, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 27:
/* # line 858 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | primary_value tDOT tIDENTIFIER tOP_ASGN command_call");
                      /* not seen with Ryan's grammar and 1.8.7*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 28:
/* # line 864 "grammar.y" */ 
	{   
                      yTrace(vps, "stmt: | primary_value tDOT tCONSTANT tOP_ASGN command_call");
                      /* not seen with Ryan's grammar and 1.8.7*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 29:
/* # line 870 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call");
                      /* not seen with Ryan's grammar and 1.8.7*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 30:
/* # line 876 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | backref tOP_ASGN command_call");
                        rb_backref_error(yymarkPtr[-2].obj, vps);
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 31:
/* # line 882 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | lhs tEQL mrhs");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubySValueNode::s(yymarkPtr[0].obj, vps));
                        omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                        yyvalO = RubyParser::node_assign(& yymarkPtr[-2].obj, srcOfs, *valH, vps);
                    }
break;
case 32:
/* # line 890 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | mLhs tEQL arg_value");
                        yyvalO = RubyParser::masgn_append_arg(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 33:
/* # line 895 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | mLhs tEQL mrhs");
			yyvalO = RubyParser::masgn_append_mrhs(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);   		      
                    }
break;
case 36:
/* # line 904 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | expr kAND expr");
                        OmScopeType aScope(vps->omPtr);
                        NODE **clsH = aScope.add( RubyAndNode::cls(vps));
                        yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 37:
/* # line 911 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | expr kOR expr");
                        OmScopeType aScope(vps->omPtr);
                        NODE **clsH = aScope.add( RubyOrNode::cls(vps));
                        yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 38:
/* # line 918 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | kNOT expr");
                        yyvalO = RubyNotNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 39:
/* # line 923 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | tBANG command_call");
                        yyvalO = RubyNotNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 41:
/* # line 931 "grammar.y" */ 
	{
                        yTrace(vps, "expr_value: expr");
                        yyvalO = RubyParser::value_expr(yymarkPtr[0].obj, vps);
                    }
break;
case 44:
/* # line 940 "grammar.y" */ 
	{
                        yTrace(vps, "command_call: kRETURN call_args");
                        OmScopeType aScope(vps->omPtr);
			NODE **valH = aScope.add(RubyParser::ret_args(yymarkPtr[0].obj, vps));
                        yyvalO = RubyReturnNode::s(valH, yymarkPtr[-1].obj/*kRETURN token*/, vps);
                    }
break;
case 45:
/* # line 947 "grammar.y" */ 
	{
                        yTrace(vps, "command_call: | kBREAK call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args(yymarkPtr[0].obj, vps));
                        yyvalO = RubyBreakNode::s( valH, yymarkPtr[-1].obj/*kBREAK token*/, vps);
                    }
break;
case 46:
/* # line 954 "grammar.y" */ 
	{
                        yTrace(vps, "command_call: | kNEXT call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args(yymarkPtr[0].obj, vps));
                        yyvalO = RubyNextNode::s( valH, yymarkPtr[-1].obj/*kNEXT token*/, vps);
                    }
break;
case 48:
/* # line 964 "grammar.y" */ 
	{
                        yTrace(vps, "block_command: block_call...");
                        yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 49:
/* # line 969 "grammar.y" */ 
	{
                        yTrace(vps, "block_command: | block_call tCOLON2 operation2 command_args");
                        yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 50:
/* # line 976 "grammar.y" */ 
	{
                        yTrace(vps, "cmd_brace_block: tLBRACE_ARG");
                        reset_block(vps);
                        /* $1 = int64ToSi(vps->ruby_sourceline() );*/
                    }
break;
case 51:
/* # line 982 "grammar.y" */ 
	{ 
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 52:
/* # line 987 "grammar.y" */ 
	{
		      yTrace(vps, "cmd_brace_block: ___ comp_stamt tRCURLY");
                      rParenLexPop(vps);
		      popBlockVars(vps);
		      yyvalO = RubyIterRpNode::s( yymarkPtr[-3].obj/*masgn from opt_block_var*/ , yymarkPtr[-1].obj/*compstmp*/, yymarkPtr[-5].obj/*srcOffsetSi*/, 
						vps, 1/* strlen( '}' ) */ );
                    }
break;
case 53:
/* # line 997 "grammar.y" */ 
	{
                      yTrace(vps, "command: operation command_args =tLOWEST");
                        yyvalO = RubyParser::new_fcall(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                   }
break;
case 54:
/* # line 1002 "grammar.y" */ 
	{
                      yTrace(vps, "command: | operation command_args cmd_brace_block");
                      yyvalO = RubyParser::new_fcall_braceBlock(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                   }
break;
case 55:
/* # line 1007 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tDOT operation2 command_args =tLOWEST");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 56:
/* # line 1012 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tDOT operation2 command_args cmd_brace_block");
                      yyvalO = RubyParser::new_call_braceBlock(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 57:
/* # line 1017 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tCOLON2 operation2 command_args =tLOWEST");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 58:
/* # line 1022 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tCOLON2 operation2 command_args cmd_brace_block");
                      yyvalO = RubyParser::new_call_braceBlock(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                   }
break;
case 59:
/* # line 1027 "grammar.y" */ 
	{
                      yTrace(vps, "command: | kSUPER command_args");
                      yyvalO = RubyParser::new_super(& yymarkPtr[0].obj, yymarkPtr[-1].obj/*super token*/, vps);
                    }
break;
case 60:
/* # line 1032 "grammar.y" */ 
	{
                      yTrace(vps, "command: | kYIELD command_args");
                      yyvalO = RubyParser::new_yield(& yymarkPtr[0].obj, yymarkPtr[-1].obj/*yield token*/, vps);
                    }
break;
case 62:
/* # line 1040 "grammar.y" */ 
	{
                      yTrace(vps, "mLhs: | tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 64:
/* # line 1049 "grammar.y" */ 
	{
		      yTrace(vps, "mlhs_entry: | tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
		      OmScopeType aScope(vps->omPtr);
		      NODE **valH = aScope.add( RubyArrayNode::s(yymarkPtr[-1].obj, vps));
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-2].obj);
		      yyvalO = RubyParser::new_parasgn( *valH, srcOfs, vps);
                    }
break;
case 65:
/* # line 1060 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: mlhs_head ");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[0].obj, ofsO, vps);
                    }
break;
case 66:
/* # line 1066 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | mlhs_head mlhs_item");
                      OmScopeType aScope(vps->omPtr);
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      NODE *valO = RubyArrayNode::append_for_mlhs( yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn(valO, ofsO, vps);
                    }
break;
case 67:
/* # line 1074 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | mlhs_head tSTAR mlhs_node");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      *valH = RubyArrayNode::append_for_mlhs( yymarkPtr[-2].obj, *valH, vps);
                      yyvalO = RubyParser::new_parasgn(yymarkPtr[-2].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 68:
/* # line 1082 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | mlhs_head tSTAR");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      *valH = RubyArrayNode::append_for_mlhs( yymarkPtr[-1].obj, *valH, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-1].obj, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 69:
/* # line 1090 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | tSTAR mlhs_node");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      *valH = RubyArrayNode::s( *valH, vps);
                      yyvalO = RubyParser::new_parasgn( *valH, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 70:
/* # line 1098 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | tSTAR");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      *valH = RubyArrayNode::s( *valH, vps);
                      yyvalO = RubyParser::new_parasgn( *valH, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 72:
/* # line 1109 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_item: tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 73:
/* # line 1117 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_head: mlhs_item tCOMMA");
                      yyvalO = RubyArrayNode::s( yymarkPtr[-1].obj, vps);
                    }
break;
case 74:
/* # line 1122 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_head: | mlhs_head mlhs_item tCOMMA");
                      yyvalO = RubyArrayNode::append_for_mlhs(yymarkPtr[-2].obj, yymarkPtr[-1].obj, vps); /* result is $1*/
                    }
break;
case 75:
/* # line 1129 "grammar.y" */ 
	{
                     rParenLexPop(vps);
                     om *omPtr = vps->omPtr;
  		     OmScopeType scp(omPtr);
                     omObjSType **resH = scp.add(om::NewArray(omPtr, 2));
                     om::StoreOop(omPtr, resH, 0, & yymarkPtr[-1].obj );
                     om::StoreOop(omPtr, resH, 1, & yymarkPtr[0].obj /*srcOffsetSi*/);
                     yyvalO = *resH; 
                   }
break;
case 76:
/* # line 1140 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_node: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = assignable(& yymarkPtr[0].obj, ofsO, vps->nilH(), vps);
                    }
break;
case 77:
/* # line 1146 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_node: | primary_value tLBRACK_STR aref__args tRBRACK");
                      omObjSType *srcOfs = om::FetchOop(yymarkPtr[0].obj, 1); /* no gc*/
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[0].obj, 0);
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-1].obj, ram_OOP_NIL/*"[]="*/, aref_args, srcOfs, vps);
                    }
break;
case 78:
/* # line 1153 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_node: | primary_value tDOT tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, 
							ram_OOP_NIL, vps);
                    }
break;
case 79:
/* # line 1159 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 80:
/* # line 1164 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tDOT tCONSTANT");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 81:
/* # line 1169 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tCONSTANT");
                      if (vps->in_def || vps->in_single) {
                         rb_compile_error(vps, "dynamic constant assignment");
                      }
                      yyvalO = RubyConstDeclNode::colon2(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 82:
/* # line 1177 "grammar.y" */ 
	{
                      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "dynamic constant assignment");
                      }
                      yyvalO = RubyConstDeclNode::colon3( yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 83:
/* # line 1184 "grammar.y" */ 
	{
                      rb_backref_error(yymarkPtr[0].obj, vps);
                      yyvalO = ram_OOP_NIL;
                    }
break;
case 84:
/* # line 1191 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = assignable(& yymarkPtr[0].obj, ofsO, vps->nilH(), vps);
                    }
break;
case 85:
/* # line 1197 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tLBRACK_STR aref__args tRBRACK");
                      /* rParenLexPop(vps); */ /* Fix GitHub issue #148, ary_ref pops already */
                      omObjSType *srcOfs = om::FetchOop(yymarkPtr[0].obj, 1); /* no gc*/
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[0].obj, 0);
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-1].obj, ram_OOP_NIL/*"[]="*/, aref_args, srcOfs, vps);
                    }
break;
case 86:
/* # line 1205 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tDOT tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 87:
/* # line 1210 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 88:
/* # line 1215 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tDOT tCONSTANT");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 89:
/* # line 1220 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tCONSTANT");
		      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "dynamic constant assignment");
                      }
                      yyvalO = RubyConstDeclNode::colon2(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 90:
/* # line 1228 "grammar.y" */ 
	{
                      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "dynamic constant assignment");
                      }
                      OmScopeType aScope(vps->omPtr);
                      yyvalO = RubyConstDeclNode::colon3( yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 91:
/* # line 1236 "grammar.y" */ 
	{
                        rb_backref_error(yymarkPtr[0].obj, vps);
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 92:
/* # line 1243 "grammar.y" */ 
	{
                      yTrace(vps, "cname: tIDENTIFIER");
                      rb_compile_error(vps, "class/module name must be CONSTANT");
                    }
break;
case 94:
/* # line 1251 "grammar.y" */ 
	{
                      yTrace(vps, "cpath: tCOLON3 cname");
                      /* $$  = NEW_COLON3($2);*/
   		      yyvalO = RubyColon3Node::s(yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 95:
/* # line 1257 "grammar.y" */ 
	{
                      yTrace(vps, "cpath: | cname");
                      /* $$  = NEW_COLON2(0, $$);*/
                      yyvalO = yymarkPtr[0].obj ; /* a RpNameToken*/
                    }
break;
case 96:
/* # line 1263 "grammar.y" */ 
	{
                      yTrace(vps, "cpath: | primary_value tCOLON2 cname");
                      /* $$  = NEW_COLON2($1, $3);*/
                      yyvalO = RubyColon2Node::s(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps); 
                    }
break;
case 100:
/* # line 1274 "grammar.y" */ 
	{
                      yTrace(vps, "fname: tIDENTIFIER | tCONSTANT | tFID | op");
                      vps->lex_state = EXPR_END;
                      /* $$  = convert_op($1);*/
                      yyvalO = yymarkPtr[0].obj; /* a RpNameToken*/
                    }
break;
case 101:
/* # line 1281 "grammar.y" */ 
	{
                      yTrace(vps, "fname: | reswords");
                      vps->lex_state = EXPR_END;
                      /* $$  = $<id>1;*/
                      yyvalO = yymarkPtr[0].obj; /* a RpNameToken or a String*/
                    }
break;
case 102:
/* # line 1290 "grammar.y" */ 
	{  /* deleted  fsym  : fname  */
		       /*                | symbol*/
                       /*                ; */
	               yTrace(vps, "fitem: fname");
                       yyvalO = RubySymbolNode::s( RpNameToken::symval(yymarkPtr[0].obj/*RpNameToken*/, vps), vps);
		    }
break;
case 103:
/* # line 1298 "grammar.y" */ 
	{
                       yTrace(vps, "fitem: | symbol");
                       /* $$  = NEW_LIT(QUID2SYM($1));*/
                       yyvalO = RubySymbolNode::s( yymarkPtr[0].obj/*a Symbol*/, vps);
                    }
break;
case 105:
/* # line 1307 "grammar.y" */ 
	{
                      yTrace(vps, "undef_list: fitem");
                      yyvalO = RubyParser::new_undef( yymarkPtr[0].obj/*a RubySymbolNode*/, vps); 
                    }
break;
case 106:
/* # line 1311 "grammar.y" */ 
	{vps->lex_state = EXPR_FNAME;}
break;
case 107:
/* # line 1312 "grammar.y" */ 
	{
                      yTrace(vps, "undef_list: ___ fitem");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::new_undef( yymarkPtr[0].obj, vps));
                      yyvalO = RubyParser::block_append(yymarkPtr[-3].obj, *valH, vps);
                    }
break;
case 108:
/* # line 1320 "grammar.y" */ 
	{ yTrace(vps, "op |");    yyvalO = RpNameToken::s(a_sym_orOp, yymarkPtr[0].obj, vps); }
break;
case 109:
/* # line 1321 "grammar.y" */ 
	{ yTrace(vps, "op ^");    yyvalO = RpNameToken::s( a_sym_upArrow, yymarkPtr[0].obj, vps); }
break;
case 110:
/* # line 1322 "grammar.y" */ 
	{ yTrace(vps, "op &");    yyvalO = RpNameToken::s(a_sym_andOp, yymarkPtr[0].obj, vps); }
break;
case 111:
/* # line 1323 "grammar.y" */ 
	{ yTrace(vps, "op tCMP"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 112:
/* # line 1324 "grammar.y" */ 
	{ yTrace(vps, "op tEQ");  yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 113:
/* # line 1325 "grammar.y" */ 
	{ yTrace(vps, "op tEQQ"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 114:
/* # line 1326 "grammar.y" */ 
	{ yTrace(vps, "op tMATCH"); yyvalO = RpNameToken::s(a_sym_tMATCH, yymarkPtr[0].obj, vps); }
break;
case 115:
/* # line 1327 "grammar.y" */ 
	{ yTrace(vps, "op >");    yyvalO = RpNameToken::s(a_sym_gt, yymarkPtr[0].obj, vps); }
break;
case 116:
/* # line 1328 "grammar.y" */ 
	{ yTrace(vps, "op tGEQ"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 117:
/* # line 1329 "grammar.y" */ 
	{ yTrace(vps, "op <");    yyvalO = RpNameToken::s( a_sym_lt, yymarkPtr[0].obj, vps); }
break;
case 118:
/* # line 1330 "grammar.y" */ 
	{ yTrace(vps, "op tLEQ"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 119:
/* # line 1331 "grammar.y" */ 
	{ yTrace(vps, "op tLSHFT"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 120:
/* # line 1332 "grammar.y" */ 
	{ yTrace(vps, "op tRSHFT"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 121:
/* # line 1333 "grammar.y" */ 
	{ yTrace(vps, "op +");    yyvalO = RpNameToken::s(a_sym_plus, yymarkPtr[0].obj, vps); }
break;
case 122:
/* # line 1334 "grammar.y" */ 
	{ yTrace(vps, "op -");    yyvalO = RpNameToken::s(a_sym_minus, yymarkPtr[0].obj, vps); }
break;
case 123:
/* # line 1335 "grammar.y" */ 
	{ yTrace(vps, "op *");    yyvalO = RpNameToken::s( a_sym_star, yymarkPtr[0].obj, vps); }
break;
case 124:
/* # line 1336 "grammar.y" */ 
	{ yTrace(vps, "op tSTAR"); yyvalO = RpNameToken::s( a_sym_star, yymarkPtr[0].obj, vps); }
break;
case 125:
/* # line 1337 "grammar.y" */ 
	{ yTrace(vps, "op /");    yyvalO = RpNameToken::s( a_sym_div, yymarkPtr[0].obj, vps); }
break;
case 126:
/* # line 1338 "grammar.y" */ 
	{ yTrace(vps, "op %");    yyvalO = RpNameToken::s( a_sym_percent, yymarkPtr[0].obj, vps); }
break;
case 127:
/* # line 1339 "grammar.y" */ 
	{ yTrace(vps, "op tPOW"); yyvalO = RpNameToken::s( a_sym_tPOW, yymarkPtr[0].obj, vps); }
break;
case 128:
/* # line 1340 "grammar.y" */ 
	{ yTrace(vps, "op ~");    yyvalO = RpNameToken::s(a_sym_tilde, yymarkPtr[0].obj, vps); }
break;
case 129:
/* # line 1341 "grammar.y" */ 
	{ yTrace(vps, "op tUPLUS"); yyvalO = RpNameToken::s( a_sym_tUPLUS, yymarkPtr[0].obj, vps);}
break;
case 130:
/* # line 1342 "grammar.y" */ 
	{ yTrace(vps, "op tUMINUS"); yyvalO = RpNameToken::s(a_sym_tUMINUS, yymarkPtr[0].obj, vps);; }
break;
case 131:
/* # line 1343 "grammar.y" */ 
	{ yTrace(vps, "op tAREF"); yyvalO = RpNameToken::s(a_sym_tAREF, yymarkPtr[0].obj, vps); }
break;
case 132:
/* # line 1344 "grammar.y" */ 
	{ yTrace(vps, "op tASET"); yyvalO = RpNameToken::s(a_sym_tASET, yymarkPtr[0].obj, vps); }
break;
case 133:
/* # line 1345 "grammar.y" */ 
	{ yTrace(vps, "op `");    yyvalO = RpNameToken::s( a_sym_backtick, yymarkPtr[0].obj, vps); }
break;
case 175:
/* # line 1358 "grammar.y" */ 
	{
                      yTrace(vps, "arg: lhs tEQL arg");
                      yyvalO = RubyParser::node_assign( & yymarkPtr[-2].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, yymarkPtr[0].obj, vps);
                    }
break;
case 176:
/* # line 1363 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | lhs tEQL arg kRESCUE_MOD arg");
                      OmScopeType aScope(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      NODE **valH = aScope.add( 
                        RubyRescueBodyNode::s(ram_OOP_NIL, yymarkPtr[0].obj, ram_OOP_NIL, srcOfs, vps));
                      *valH = RubyRescueNode::s( yymarkPtr[-2].obj, *valH, ram_OOP_NIL, srcOfs, vps);
                      yyvalO = RubyParser::node_assign( & yymarkPtr[-4].obj, yymarkPtr[-3].obj/*srcOffsetSi*/, *valH, vps);
                    }
break;
case 177:
/* # line 1373 "grammar.y" */ 
	{
                      yymarkPtr[0].obj = RubyParser::value_expr(yymarkPtr[0].obj, vps);
		      if (yymarkPtr[-2].obj != ram_OOP_NIL) {
                        yTrace(vps, "arg: | varLhs tOP_ASGN arg");
			yyvalO = RubyParser::new_op_asgn(yymarkPtr[-2].obj, yymarkPtr[-1].obj/*RpNameToken*/, yymarkPtr[0].obj, vps);
		      } else {
                        yTrace(vps, "arg: | NIL_LHS tOP_ASGN arg");
                        yyvalO = ram_OOP_NIL;
                      }
                    }
break;
case 178:
/* # line 1384 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tLBRACK_STR aref__args tRBRACK tOP_ASGN arg");
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[-2].obj, 0);
                      yyvalO = RubyOpElementAsgnNode::s(yymarkPtr[-3].obj, aref_args, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 179:
/* # line 1390 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tDOT tIDENTIFIER tOP_ASGN arg");
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 180:
/* # line 1395 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tDOT tCONSTANT tOP_ASGN arg");
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 181:
/* # line 1400 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg");
                      /* not seen with Ryan's grammar*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 182:
/* # line 1406 "grammar.y" */ 
	{
                        rb_compile_error(vps, "constant re-assignment");
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 183:
/* # line 1411 "grammar.y" */ 
	{
                        rb_compile_error(vps, "constant re-assignment");
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 184:
/* # line 1416 "grammar.y" */ 
	{
                        rb_backref_error(yymarkPtr[-2].obj, vps);
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 185:
/* # line 1421 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tDOT2 arg");
                      yyvalO = RubyDotNode::s(2, yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 186:
/* # line 1426 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tDOT3 arg");
                      yyvalO = RubyDotNode::s(3, yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 187:
/* # line 1431 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPLUS arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_plus, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 188:
/* # line 1436 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tMINUS arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_minus, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 189:
/* # line 1441 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tSTAR2 arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_star, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 190:
/* # line 1446 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tDIVIDE arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_div, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 191:
/* # line 1451 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPERCENT arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_percent, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 192:
/* # line 1456 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPOW arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_tPOW, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 193:
/* # line 1461 "grammar.y" */ 
	{
                        /* $$  = call_op(call_op($2, tPOW, 1, $4, vps), tUMINUS, 0, 0, vps);*/
                      yTrace(vps, "arg: | tUMINUS_NUM tINTEGER tPOW arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **litH = aScope.add(RubyAbstractNumberNode::s( yymarkPtr[-2].obj, vps));
                      NODE **valH = aScope.add( 
			  RubyParser::new_call_1( litH, a_sym_tPOW, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps));
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, yymarkPtr[-1].obj, vps));
                      yyvalO = RubyParser::new_vcall( *valH, *selH , vps);
                    }
break;
case 194:
/* # line 1472 "grammar.y" */ 
	{
                        /* $$  = call_op(call_op($2, tPOW, 1, $4, vps), tUMINUS, 0, 0, vps);*/
                      yTrace(vps, "arg: | tUMINUS_NUM tFLOAT tPOW arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **litH = aScope.add(RubyAbstractNumberNode::s( yymarkPtr[-2].obj, vps));
                      NODE **valH = aScope.add( RubyParser::new_call_1( litH, a_sym_tPOW, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps));
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, yymarkPtr[-1].obj, vps));
                      yyvalO = RubyParser::new_vcall( *valH, *selH , vps);
                    }
break;
case 195:
/* # line 1482 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tUPLUS arg");
                      yyvalO = RubyParser::uplus_production( yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 196:
/* # line 1487 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tUMINUS arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, yymarkPtr[-1].obj, vps));
                      yyvalO = RubyParser::new_vcall( yymarkPtr[0].obj, *selH, vps);
                    }
break;
case 197:
/* # line 1494 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPIPE arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_orOp, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 198:
/* # line 1499 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tCARET arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_upArrow, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 199:
/* # line 1504 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tAMPER2 arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_andOp, & yymarkPtr[0].obj,  yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 200:
/* # line 1509 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tCMP arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 201:
/* # line 1514 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tGT arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_gt, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 202:
/* # line 1519 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tGEQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 203:
/* # line 1524 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tLT arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_lt, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 204:
/* # line 1529 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tLEQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 205:
/* # line 1534 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tEQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 206:
/* # line 1539 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tEQQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 207:
/* # line 1544 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tNEQ arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_tEQ, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps));
                      yyvalO = RubyNotNode::s( *valH, vps);
                    }
break;
case 208:
/* # line 1551 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tMATCH arg");
                      yyvalO = RubyParser::get_match_node(yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 209:
/* # line 1556 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tNMATCH arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::get_match_node(yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps));
                      yyvalO = RubyNotNode::s( *valH, vps);
                    }
break;
case 210:
/* # line 1563 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tBANG arg");
                      yyvalO = RubyNotNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 211:
/* # line 1568 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tTILDE arg");
                      OmScopeType aScope(vps->omPtr);	/* try it without value_expr*/
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tilde, yymarkPtr[-1].obj, vps));
                      yyvalO = RubyParser::new_vcall( yymarkPtr[0].obj,  *selH, vps);
                    }
break;
case 212:
/* # line 1575 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tRSHFT arg"); /* try without value_expr*/
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 213:
/* # line 1580 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tRSHFT arg"); /* try without value_expr*/
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 214:
/* # line 1585 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tANDOP arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **clsH = aScope.add( RubyAndNode::cls(vps));
                      yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 215:
/* # line 1592 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tOROP arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **clsH = aScope.add( RubyOrNode::cls(vps));
                      yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 216:
/* # line 1598 "grammar.y" */ 
	{vps->in_defined = 1;}
break;
case 217:
/* # line 1599 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | kDEFINED opt_nl arg");
                      vps->in_defined = 0;
                      yyvalO = RubyDefinedNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 218:
/* # line 1604 "grammar.y" */ 
	{vps->ternary_colon++;}
break;
case 219:
/* # line 1605 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tEH arg tCOLON arg");
                      yyvalO = RubyIfNode::s(yymarkPtr[-5].obj, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                      vps->ternary_colon--;
                    }
break;
case 220:
/* # line 1611 "grammar.y" */ 
	{
                        yTrace(vps, "arg: | primary");
                        yyvalO = yymarkPtr[0].obj;
                    }
break;
case 221:
/* # line 1618 "grammar.y" */ 
	{
                      yTrace(vps, "arg_value: arg");
                      yyvalO = RubyParser::value_expr(yymarkPtr[0].obj, vps);
                    }
break;
case 223:
/* # line 1626 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | command opt_nl");
                      yyvalO = RubyRpCallArgs::s(yymarkPtr[-1].obj, vps);
                    }
break;
case 224:
/* # line 1631 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | args trailer");
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 225:
/* # line 1636 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | args tCOMMA tSTAR arg opt_nl");
                      /* value_expr($4);  was in rubinius, try without*/
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyArrayNode::append( yymarkPtr[-4].obj , *valH, vps /*returns first arg*/);
                    }
break;
case 226:
/* # line 1644 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | assocs trailer");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s(*hashNodeH, vps);
                    }
break;
case 227:
/* # line 1651 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | tSTAR arg opt_nl");
                      yymarkPtr[-1].obj = RubyParser::value_expr(yymarkPtr[-1].obj, vps);
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s( yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s( *valH, vps);
                    }
break;
case 228:
/* # line 1661 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: tLPAREN2 none tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 229:
/* # line 1667 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: | tLPAREN2 call_args opt_nl tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-2].obj;
                    }
break;
case 230:
/* # line 1673 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: | tLPAREN2 block_call opt_nl tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyRpCallArgs::s( yymarkPtr[-2].obj, vps);
                    }
break;
case 231:
/* # line 1679 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: | tLPAREN2 args tCOMMA block_call opt_nl tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyArrayNode::append( yymarkPtr[-4].obj, yymarkPtr[-2].obj, vps);
                    }
break;
case 234:
/* # line 1691 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: command");
		      yyvalO = RubyRpCallArgs::s( yymarkPtr[0].obj, vps);
                    }
break;
case 235:
/* # line 1696 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args opt_block_arg");
                        yyvalO = RubyRpCallArgs::append_blkArg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps /*returns first arg*/);
                    }
break;
case 236:
/* # line 1701 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args tCOMMA tSTAR arg_value opt_block_arg");
                      /* $$  = arg_concat(vps, $1, $4);*/
                      /* $$  = arg_blk_pass($$, $5);*/
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[-1].obj, vps));
                      RubyRpCallArgs::append_arg( yymarkPtr[-4].obj, *splatH, vps);
                      RubyRpCallArgs::append_blkArg( yymarkPtr[-4].obj, yymarkPtr[0].obj, vps);  
                      yyvalO = yymarkPtr[-4].obj ;
                    }
break;
case 237:
/* # line 1712 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_blkArg(*hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 238:
/* # line 1719 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_splatArg_blkArg(*hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 239:
/* # line 1726 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::append_arg_blkArg(yymarkPtr[-3].obj, *hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 240:
/* # line 1733 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args tCOMMA assocs tCOMMA tSTAR arg opt_block_arg");
                      /* rubinius had   value_expr($6);*/
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::append_arg_splatArg_blkArg(yymarkPtr[-6].obj, *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);

                    }
break;
case 241:
/* # line 1742 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | tSTAR arg_value opt_block_arg");
                      yyvalO = RubyRpCallArgs::s_splatArg_blkArg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 243:
/* # line 1750 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: arg_value tCOMMA args opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callArgsH = aScope.add( RubyParser::list_prepend(yymarkPtr[-1].obj, yymarkPtr[-3].obj, vps));
                      RubyRpCallArgs::append_blkArg( *callArgsH, yymarkPtr[0].obj, vps);
                      yyvalO = *callArgsH;
                    }
break;
case 244:
/* # line 1758 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA block_arg");
                      yyvalO = RubyRpCallArgs::append_blkArg( yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 245:
/* # line 1763 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA tSTAR arg_value opt_block_arg");
                      yyvalO = RubyRpCallArgs::s_arg_splatArg_blkArg( yymarkPtr[-4].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 246:
/* # line 1768 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callArgsH = aScope.add( RubyParser::list_prepend(yymarkPtr[-4].obj, yymarkPtr[-6].obj, vps));
                      yyvalO = RubyRpCallArgs::append_arg_blkArg(*callArgsH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 247:
/* # line 1775 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_blkArg(*hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 248:
/* # line 1782 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_splatArg_blkArg( *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 249:
/* # line 1789 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_arg_blkArg( yymarkPtr[-3].obj, *hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 250:
/* # line 1796 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_addAll_arg_blkArg(yymarkPtr[-5].obj, yymarkPtr[-3].obj, *hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 251:
/* # line 1803 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
		      yyvalO = RubyRpCallArgs::s_arg_arg_splatArg_blkArg(yymarkPtr[-6].obj, *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 252:
/* # line 1810 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_addAll_arg_splatArg_blkArg(yymarkPtr[-8].obj, yymarkPtr[-6].obj, *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 253:
/* # line 1817 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | tSTAR arg_value opt_block_arg");
                      yyvalO = RubyRpCallArgs::s_splatArg_blkArg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 255:
/* # line 1824 "grammar.y" */ 
	{
                      yTrace(vps, "command_args:");
                      OmScopeType scp(vps->omPtr);
                      yyvalO = vps->cmdarg_stack.asSi() ;
#if defined(FLG_DEBUG)
  if (debugCmdArg) {
    printf("saving cmdarg_stack 0x%lx\n", vps->cmdarg_stack.word());
  }
#endif
                      CMDARG_PUSH(vps, 1);
                    }
break;
case 256:
/* # line 1836 "grammar.y" */ 
	{
                      yTrace(vps, "command_args: ___  open_args");
		      if (! vps->cmdarg_stack.restoreFromSi( yymarkPtr[-1].obj )) {
			rb_compile_error("invalid cmdarg_stack.restore", vps);
		      }
#if defined(FLG_DEBUG)
  if (debugCmdArg) {
    printf("restored cmdarg_stack 0x%lx\n", vps->cmdarg_stack.word());
  }
#endif
		      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 258:
/* # line 1851 "grammar.y" */ 
	{vps->lex_state = EXPR_ENDARG;}
break;
case 259:
/* # line 1852 "grammar.y" */ 
	{
                      yTrace(vps, "open_args: tLPAREN_ARG");
                      rParenLexPop(vps);
                      rb_warning(vps, "don't put space before argument parentheses");
                      yyvalO = ram_OOP_NIL;
                    }
break;
case 260:
/* # line 1858 "grammar.y" */ 
	{vps->lex_state = EXPR_ENDARG;}
break;
case 261:
/* # line 1859 "grammar.y" */ 
	{
                      yTrace(vps, "open_args: ___ tRPAREN");
                      rParenLexPop(vps);
		      rb_warning(vps, "don't put space before argument parentheses");
		      yyvalO = yymarkPtr[-2].obj;
                    }
break;
case 262:
/* # line 1868 "grammar.y" */ 
	{
                      yTrace(vps, "block_arg: tAMPER arg_value");
                      yyvalO = RubyBlockPassNode::s( yymarkPtr[0].obj , vps);
                    }
break;
case 263:
/* # line 1873 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_arg: tCOMMA block_arg");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 265:
/* # line 1881 "grammar.y" */ 
	{
                      yTrace(vps, "args: arg_value");
                      yyvalO = RubyRpCallArgs::s( yymarkPtr[0].obj, vps);
                    }
break;
case 266:
/* # line 1886 "grammar.y" */ 
	{
                      yTrace(vps, " args: | args tCOMMA arg_value");
                      yyvalO = RubyRpCallArgs::append_arg(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps /*return first arg*/);
                    }
break;
case 267:
/* # line 1893 "grammar.y" */ 
	{
                      yTrace(vps, "mrhs: args tCOMMA arg_value");
                      yyvalO = RubyRpCallArgs::append_arg(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps /*return first arg*/);
                    }
break;
case 268:
/* # line 1898 "grammar.y" */ 
	{
                      yTrace(vps, "mrhs: | args tCOMMA tSTAR arg_value");
                      yyvalO = RubyRpCallArgs::append_splatArg(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 269:
/* # line 1903 "grammar.y" */ 
	{
                      yTrace(vps, "mrhs: | tSTAR arg_value");
                      yyvalO = RubySplatNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 278:
/* # line 1918 "grammar.y" */ 
	{
                      yTrace(vps, "primary: tFID");
                      yyvalO = RubyParser::new_fcall(yymarkPtr[0].obj, ram_OOP_NIL, vps);
                    }
break;
case 279:
/* # line 1923 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kBEGIN");
                        /* $<num>1 = ruby_sourceline;*/
                      PUSH_LINE(vps, "begin");
                    }
break;
case 280:
/* # line 1930 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kBEGIN body_stamt kEND");
		      POP_LINE(vps);
		      if (yymarkPtr[-1].obj == ram_OOP_NIL) {
                         yyvalO = RubyNilNode::new_(vps);
		      } else {
                         yyvalO = RubyBeginNode::s(yymarkPtr[-1].obj, vps);
                      }
                      /*  nd_set_line($$, $<num>1);*/
                    }
break;
case 281:
/* # line 1940 "grammar.y" */ 
	{vps->lex_state = EXPR_ENDARG;}
break;
case 282:
/* # line 1941 "grammar.y" */ 
	{
                      yTrace(vps, "primary: ___ opt_nl tRPAREN");
                      rParenLexPop(vps);
		      rb_warning(vps, "(...) interpreted as grouped expression");
		      yyvalO = yymarkPtr[-3].obj;
                    }
break;
case 283:
/* # line 1948 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | tLPAREN comp_stamt tRPAREN");
                      rParenLexPop(vps);
                      OmScopeType scp(vps->omPtr);
                      NODE **resH;
                      if (yymarkPtr[-1].obj == ram_OOP_NIL) {
                        resH = scp.add( RubyNilNode::new_(vps) );
                      } else {
                        resH = scp.add( yymarkPtr[-1].obj);
                      }
                      yyvalO = RubyNode::setParen(*resH, vps);
                    }
break;
case 284:
/* # line 1961 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | primary_value tCOLON2 tCONSTANT");
                      yyvalO = RubyColon2Node::s(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 285:
/* # line 1966 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | tCOLON3 tCONSTANT");
                      yyvalO = RubyColon3Node::s( yymarkPtr[0].obj, vps);
                    }
break;
case 286:
/* # line 1971 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | primary_value tLBRACK_STR aref__args tRBRACK");
                      omObjSType *srcOfs = om::FetchOop(yymarkPtr[0].obj, 1); /* no gc*/
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[0].obj, 0);
                      yyvalO = RubyParser::new_aref(yymarkPtr[-1].obj, aref_args, srcOfs, vps);
                    }
break;
case 287:
/* # line 1978 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | tLBRACK aref__args tRBRACK");
                      rParenLexPop(vps);
                      if (yymarkPtr[-1].obj == ram_OOP_NIL) {
                         yyvalO = RubyRpCallArgs::s(vps);
                      } else {
                         yyvalO = yymarkPtr[-1].obj;
                      }
                    }
break;
case 288:
/* # line 1988 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | tLBRACE assoc_list tRCURLY");
                      rParenLexPop(vps);
                      yyvalO = RubyHashNode::s(yymarkPtr[-1].obj, vps);
                    }
break;
case 289:
/* # line 1994 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kRETURN");
                      yyvalO = RubyReturnNode::s( vps->nilH(), yymarkPtr[0].obj/*return token*/, vps);
                    }
break;
case 290:
/* # line 1999 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kYIELD tLPAREN2 call_args tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyParser::new_yield(& yymarkPtr[-1].obj, yymarkPtr[-3].obj/*yield token*/, vps);
                    }
break;
case 291:
/* # line 2005 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kYIELD tLPAREN2 tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyParser::new_yield(vps->nilH(), yymarkPtr[-2].obj/*yield token*/, vps);
                    }
break;
case 292:
/* # line 2011 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kYIELD");
                      yyvalO = RubyParser::new_yield(vps->nilH(), yymarkPtr[0].obj/*yield token*/, vps);
                    }
break;
case 293:
/* # line 2015 "grammar.y" */ 
	{vps->in_defined = 1;}
break;
case 294:
/* # line 2016 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEFINED opt_nl tLPAREN2 expr tRPAREN");
                      rParenLexPop(vps);
                      vps->in_defined = 0;
                      yyvalO = RubyDefinedNode::s(yymarkPtr[-1].obj, vps);
                    }
break;
case 295:
/* # line 2023 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | operation brace_blck");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callH = aScope.add( RubyParser::new_fcall( yymarkPtr[-1].obj, ram_OOP_NIL, vps));
                      RubyIterRpNode::set_call( yymarkPtr[0].obj, *callH, vps);
                      yyvalO = yymarkPtr[0].obj; /* $2 is a RubyIterRpNode*/
                    }
break;
case 297:
/* # line 2032 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | method_call brace_blck");
                      if (RubyBlockPassNode::is_a(yymarkPtr[-1].obj, vps)) {
                         rb_compile_error(vps, "both block arg and actual block given");
		      }
                      RubyIterRpNode::set_call( yymarkPtr[0].obj, yymarkPtr[-1].obj, vps);
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 298:
/* # line 2040 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "if");
                  }
break;
case 299:
/* # line 2046 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kIF expr_value then comp_stamt if_tail kEND");
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* kIF*/
                      yyvalO = RubyParser::new_if( yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 300:
/* # line 2052 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "unless");
                  }
break;
case 301:
/* # line 2058 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kUNLESS expr_value then comp_stamt opt_else kEND");
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* kUNLESS*/
		      yyvalO = RubyParser::new_if( yymarkPtr[-4].obj, yymarkPtr[-1].obj, yymarkPtr[-2].obj, srcOfs, vps);
                    }
break;
case 302:
/* # line 2064 "grammar.y" */ 
	{
                    yTrace(vps, "primary: | kWHILE");
                    PUSH_LINE(vps, "while");
                    COND_PUSH(vps, 1);
                  }
break;
case 303:
/* # line 2068 "grammar.y" */ 
	{ COND_POP(vps);}
break;
case 304:
/* # line 2071 "grammar.y" */ 
	{
                      yTrace(vps, "primary: kWHILE ___ comp_stamt kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* of kWHILE*/
                      yyvalO = RubyParser::new_while( yymarkPtr[-1].obj, yymarkPtr[-4].obj, srcOfs, vps);
                    }
break;
case 305:
/* # line 2077 "grammar.y" */ 
	{
                    yTrace(vps, "primary: | kUNTIL");
                    PUSH_LINE(vps, "until");
                    COND_PUSH(vps, 1);
                  }
break;
case 306:
/* # line 2081 "grammar.y" */ 
	{ COND_POP(vps);}
break;
case 307:
/* # line 2084 "grammar.y" */ 
	{
                      yTrace(vps, "kUNTIL ___ comp_stamt kEND");
		      /* maglev had premature_eof() check*/
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* of kUNTIL*/
		      yyvalO = RubyParser::new_until( yymarkPtr[-1].obj, yymarkPtr[-4].obj, srcOfs, vps);
                    }
break;
case 308:
/* # line 2091 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "case");
                  }
break;
case 309:
/* # line 2096 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCASE expr_value opt_termms case_body kEND");
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj); /* of kCASE*/
		      yyvalO = RubyCaseNode::s(yymarkPtr[-3].obj, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 310:
/* # line 2102 "grammar.y" */ 
	{ 
                    push_start_line(vps, vps->ruby_sourceline() - 1, "case");
                  }
break;
case 311:
/* # line 2105 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCASE opt_termms case_body kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-4].obj); /* of kCASE*/
                      yyvalO = RubyCaseNode::s(ram_OOP_NIL, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 312:
/* # line 2111 "grammar.y" */ 
	{
                    push_start_line(vps, vps->ruby_sourceline() - 1, "case");
                  }
break;
case 313:
/* # line 2114 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCASE opt_termms kELSE comp_stamt kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj); /* of kCASE*/
                      yyvalO = RubyCaseNode::s(ram_OOP_NIL, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 314:
/* # line 2120 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "for");
                  }
break;
case 315:
/* # line 2122 "grammar.y" */ 
	{ COND_PUSH(vps, 1);}
break;
case 316:
/* # line 2122 "grammar.y" */ 
	{ COND_POP(vps);}
break;
case 317:
/* # line 2125 "grammar.y" */ 
	{
                      yTrace(vps, "primary: kFOR ___ comp_stamt kEND");
                      POP_LINE(vps);
                      yyvalO = RubyForNode::s( & yymarkPtr[-4].obj, & yymarkPtr[-7].obj, & yymarkPtr[-1].obj, yymarkPtr[-9].obj/*for token*/, 
						vps, 3/* strlen( 'end' ) */ );
                    }
break;
case 318:
/* # line 2132 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCLASS cpath superclass");
		      PUSH_LINE(vps, "class");
		      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "class definition in method body");
                      }
                      vps->class_nest++;
                      local_push(vps, 0);
                      yyvalO = int64ToSi( vps->ruby_sourceline() );
                    }
break;
case 319:
/* # line 2144 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCLASS ___ body_stamt kEND");
		      POP_LINE(vps);
                      /* new_class( path, superclass, body)*/
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj);  /* of kCLASS*/
                      NODE **resH = scp.add( RubyClassNode::s( yymarkPtr[-4].obj, yymarkPtr[-3].obj, yymarkPtr[-1].obj, *vps->sourceStrH, srcOfs,  vps));
                      /*  nd_set_line($$, $<num>4);*/
                      local_pop(vps);
                      vps->class_nest--;
                      yyvalO = *resH;
                    }
break;
case 320:
/* # line 2157 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCLASS tLSHFT expr");
		      PUSH_LINE(vps, "class");
		      yyvalO = int64ToSi( vps->in_def );
		      vps->in_def = 0;
                    }
break;
case 321:
/* # line 2164 "grammar.y" */ 
	{
                      yTrace(vps, "primary | kCLASS ___ Term");
		      yyvalO = int64ToSi( vps->in_single );
		      vps->in_single = 0;
		      vps->class_nest++;
		      local_push(vps, 0);
                    }
break;
case 322:
/* # line 2173 "grammar.y" */ 
	{
                      yTrace(vps, "primary  | kCLASS ___ body_stamt kEND");
		      int lineNum = POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      NODE **resH = scp.add( RubySClassNode::s(& yymarkPtr[-5].obj, & yymarkPtr[-1].obj, yymarkPtr[-7].obj/*RpNameTokenkCLASS*/, lineNum, vps));
		      local_pop(vps);
		      vps->class_nest--;
		      vps->in_def = siToI64( yymarkPtr[-4].obj );
		      vps->in_single = siToI64( yymarkPtr[-2].obj) ;
                      yyvalO = *resH;
                    }
break;
case 323:
/* # line 2185 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kMODULE cpath");
		      PUSH_LINE(vps, "module");
		      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "module definition in method body");
                      }
                      vps->class_nest++;
                      local_push(vps, 0);
                      yyvalO = int64ToSi( vps->ruby_sourceline() );
                    }
break;
case 324:
/* # line 2197 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kMODULE ___ body_stamt kEND");
		      POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-4].obj);  /* of kMODULE*/
                      NODE **resH = scp.add( RubyModuleNode::s( yymarkPtr[-3].obj, yymarkPtr[-1].obj, *vps->sourceStrH, srcOfs, vps));
		      /* nd_set_line($$, $<num>3);*/
		      local_pop(vps);
		      vps->class_nest--;
                      yyvalO = *resH;
                    }
break;
case 325:
/* # line 2209 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEF fname");
		      PUSH_LINE(vps, "def");
		      yyvalO = ram_OOP_Zero; /* $<id>$ = cur_mid;*/
		      /* cur_mid = $2;*/
		      vps->in_def++;
		      local_push(vps, 0);
                    }
break;
case 326:
/* # line 2220 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEF ___ f_arglist body_stamt kEND");
		      int lineNum = POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj/*kDEF*/);
                      omObjSType *endOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[0].obj/*kEND*/);
                      NODE **resH = scp.add( RubyParser::new_defn( yymarkPtr[-4].obj/*fname*/, yymarkPtr[-2].obj/*arglist*/, 
					yymarkPtr[-1].obj/*body*/, srcOfs, lineNum, endOfs, vps));
		      local_pop(vps);
		      vps->in_def--;
		      /* cur_mid = $<id>3;*/
                      yyvalO = *resH;
                    }
break;
case 327:
/* # line 2233 "grammar.y" */ 
	{vps->lex_state = EXPR_FNAME;}
break;
case 328:
/* # line 2234 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEF ___ fname");
		      PUSH_LINE(vps, "def");
		      vps->in_single++;
		      local_push(vps, 0);
		      vps->lex_state = EXPR_END; /* force for args */
                    }
break;
case 329:
/* # line 2244 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEF ___ f_arglist body_stamt kEND");
		      int lineNum = POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-8].obj); /* of kDEF*/
                      omObjSType *endOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[0].obj/*kEND*/);
                      NODE **resH = scp.add( RubyParser::new_defs( yymarkPtr[-7].obj/*rcvr (the singleton)*/, 
			         yymarkPtr[-4].obj/*fname*/, yymarkPtr[-2].obj/*args*/, yymarkPtr[-1].obj/*body*/, srcOfs, 
				  lineNum, endOfs, vps));
		      local_pop(vps);
		      vps->in_single--;
                      yyvalO = *resH;
                    }
break;
case 330:
/* # line 2258 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kBREAK");
                      yyvalO = RubyBreakNode::s(vps->nilH(), yymarkPtr[0].obj/*break token*/, vps);
                    }
break;
case 331:
/* # line 2263 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kNEXT");
                      yyvalO = RubyNextNode::s(vps->nilH(), yymarkPtr[0].obj/*next token*/, vps);
                    }
break;
case 332:
/* # line 2268 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kREDO");
                      yyvalO = RubyRedoNode::s(yymarkPtr[0].obj/*redo token*/, vps);
                    }
break;
case 333:
/* # line 2273 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kRETRY");
                      yyvalO = RubyRetryNode::s(yymarkPtr[0].obj/*retry token*/, vps);
                    }
break;
case 334:
/* # line 2280 "grammar.y" */ 
	{
                      yTrace(vps, "primary_value: primary");
                      yyvalO = RubyParser::value_expr(yymarkPtr[0].obj, vps);
                    }
break;
case 343:
/* # line 2301 "grammar.y" */ 
	{
                      yTrace(vps, "if_tail: opt_else| kELSIF___if_tail ");
                      yyvalO = RubyIfNode::s(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 345:
/* # line 2309 "grammar.y" */ 
	{
                      yTrace(vps, "opt_else: | kELSE comp_stamt");
		      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 348:
/* # line 2320 "grammar.y" */ 
	{
		      yTrace(vps, "block_par : mlhs_item");
		      yyvalO = RubyArrayNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 349:
/* # line 2325 "grammar.y" */ 
	{
		      yTrace(vps, "block_par : block_par , mlhs_item");
		      yyvalO = RubyArrayNode::append(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 350:
/* # line 2332 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var : block_par x");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
		      yyvalO = RubyParser::new_parasgn( yymarkPtr[0].obj, ofsO, vps);
                    }
break;
case 351:
/* # line 2338 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var | block_par , x");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
		      yyvalO = RubyParser::new_parasgn_trailingComma( yymarkPtr[-1].obj, ofsO, vps);
                    }
break;
case 352:
/* # line 2344 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var | block_par , & lhs x");
                      RubyArrayNode::append_amperLhs(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-3].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);   
                    }
break;
case 353:
/* # line 2350 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var | block_par , STAR lhs , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[-3].obj, vps));
                      RubyArrayNode::append(yymarkPtr[-6].obj, *splatH, vps);
                      RubyArrayNode::append_amperLhs(yymarkPtr[-6].obj, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-6].obj, yymarkPtr[-4].obj/*srcOffsetSi*/, vps);
                    }
break;
case 354:
/* # line 2359 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var | block_par , STAR , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      RubyArrayNode::append(yymarkPtr[-5].obj, *splatH, vps);
                      RubyArrayNode::append_amperLhs(yymarkPtr[-5].obj, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-5].obj, yymarkPtr[-3].obj/*srcOffsetSi*/, vps);
                    }
break;
case 355:
/* # line 2368 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | block_par , STAR lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      RubyArrayNode::append(yymarkPtr[-3].obj, *splatH, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-3].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 356:
/* # line 2376 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | block_par , STAR x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      RubyArrayNode::append(yymarkPtr[-2].obj, *splatH, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-2].obj, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 357:
/* # line 2384 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | STAR lhs , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[-3].obj, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      RubyArrayNode::append_amperLhs(*arrH, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[-4].obj/*srcOffsetSi*/, vps);
                    }
break;
case 358:
/* # line 2393 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | STAR , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      RubyArrayNode::append_amperLhs(*arrH, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[-3].obj/*srcOffsetSi*/, vps);
                    }
break;
case 359:
/* # line 2402 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | STAR lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 360:
/* # line 2410 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | STAR x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 361:
/* # line 2418 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **arrH = aScope.add(RubyArrayNode::new_(vps));
                      RubyArrayNode::append_amperLhs(*arrH, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 363:
/* # line 2429 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_var: | tPIPE tPIPE");
                      yyvalO = ram_OOP_NIL ;
                    }
break;
case 364:
/* # line 2434 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_var: | tOROP");
                      yyvalO = ram_OOP_NIL ;
                    }
break;
case 365:
/* # line 2439 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_var: | tPIPE blck_var tPIPE");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 366:
/* # line 2446 "grammar.y" */ 
	{
                      yTrace(vps, "do_block: kDO_BLOCK");
		      PUSH_LINE(vps, "do");
		      reset_block(vps);
                      /* $1 = int64ToSi(vps->ruby_sourceline() );*/
                    }
break;
case 367:
/* # line 2453 "grammar.y" */ 
	{
                      yTrace(vps, "do_block: ___ opt_block_var");
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 368:
/* # line 2459 "grammar.y" */ 
	{
                      yTrace(vps, "do_block: ___ comp_stamt kEND");
		      POP_LINE(vps);
                      popBlockVars(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj); /* of kDO_BLOCK*/
                      yyvalO = RubyIterRpNode::s( yymarkPtr[-3].obj/*masgn from opt_block_var*/, yymarkPtr[-1].obj/*compstmt*/, srcOfs, 
						vps, 3/* strlen( 'end' ) */ );
                    }
break;
case 369:
/* # line 2470 "grammar.y" */ 
	{
                      yTrace(vps, "block_call: command do_block");
                      if (RubyBlockPassNode::is_a(yymarkPtr[-1].obj, vps)) {
			 rb_compile_error(vps, "both block arg and actual block given");
                      }
                      RubyIterRpNode::set_call( yymarkPtr[0].obj, yymarkPtr[-1].obj, vps);
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 370:
/* # line 2479 "grammar.y" */ 
	{
                      yTrace(vps, "block_call: | block_call tDOT operation2 opt_paren_args");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 371:
/* # line 2484 "grammar.y" */ 
	{
                      yTrace(vps, "block_call: block_call tCOLON2 operation2 opt_paren_args");
		      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 372:
/* # line 2491 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: operation  paren_args");
                      yyvalO = RubyParser::new_fcall(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 373:
/* # line 2496 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | primary_value tDOT operation2 opt_paren_args");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 374:
/* # line 2501 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | primary_value tCOLON2 operation2 paren_args");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 375:
/* # line 2506 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | primary_value tCOLON2 operation3");
		      yyvalO = RubyParser::new_vcall(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 376:
/* # line 2512 "grammar.y" */ 
	{
                        rb_compile_error(vps, "\\ operator is rubinius-specific get_reference");
                    }
break;
case 377:
/* # line 2516 "grammar.y" */ 
	{
                        rb_compile_error(vps, "\\ operator is rubinius-specific get_reference");
                    }
break;
case 378:
/* # line 2521 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | kSUPER paren_args");
                      yyvalO = RubyParser::new_super(&  yymarkPtr[0].obj, yymarkPtr[-1].obj/*super token*/, vps);
                    }
break;
case 379:
/* # line 2526 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | kSUPER");
                      yyvalO = RubyZSuperNode::s( yymarkPtr[0].obj/*super token*/ , vps);
                    }
break;
case 380:
/* # line 2533 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: tLCURLY");
		      reset_block(vps);
		      /* $1 is srcOffsetSi */
                    }
break;
case 381:
/* # line 2539 "grammar.y" */ 
	{ 
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 382:
/* # line 2543 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: tLCURLY ___ comp_stamt tRCURLY");
                      rParenLexPop(vps);
                      popBlockVars(vps);
                      yyvalO = RubyIterRpNode::s(yymarkPtr[-3].obj/*masgn from opt_block_var*/, yymarkPtr[-1].obj/*compstmt*/, yymarkPtr[-5].obj/*srcOffsetSi*/, 
						vps, 1/* strlen( '}' ) */ );
                    }
break;
case 383:
/* # line 2551 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: | kDO");
		      PUSH_LINE(vps, "do");
		      /* $1 is RpNameToken of 'do'*/
		      reset_block(vps);
                    }
break;
case 384:
/* # line 2558 "grammar.y" */ 
	{
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 385:
/* # line 2562 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: | kDO ___ comp_stamt kEND");
		      POP_LINE(vps);
                      popBlockVars(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj);
                      yyvalO = RubyIterRpNode::s(yymarkPtr[-3].obj/*masgn from opt_block_var*/, yymarkPtr[-1].obj/*compstmt*/, srcOfs, 
						vps, 3/* strlen( 'end' ) */ );
                    }
break;
case 386:
/* # line 2575 "grammar.y" */ 
	{
                      yTrace(vps, "case_body: kWHEN when_args then comp_stamt cases");
                      yyvalO = RubyWhenNode::s( & yymarkPtr[-3].obj, & yymarkPtr[-1].obj, & yymarkPtr[0].obj, yymarkPtr[-4].obj/*when token*/, vps);
                    }
break;
case 388:
/* # line 2582 "grammar.y" */ 
	{
                      yTrace(vps, "when_args: args | args tCOMMA tSTAR arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE **whenH = aScope.add( RubyWhenNode::s( & yymarkPtr[0].obj, vps->nilH(), vps->nilH(), 
									yymarkPtr[-1].obj/*srcOffsetSi of tSTAR*/, vps));
                      yyvalO = RubyParser::list_append(yymarkPtr[-3].obj, *whenH, vps);
                    }
break;
case 389:
/* # line 2590 "grammar.y" */ 
	{
                      yTrace(vps, "when_args: | tSTAR arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE **whenH = aScope.add( RubyWhenNode::s( & yymarkPtr[0].obj, vps->nilH(), vps->nilH(),
                                                                        yymarkPtr[-1].obj/*srcOffsetSi of tSTAR*/, vps));
                      yyvalO = RubyRpCallArgs::s( *whenH, vps);
                    }
break;
case 392:
/* # line 2606 "grammar.y" */ 
	{
                      yTrace(vps, "opt_rescue: kRESCUE exc_list exc_var then comp_stamt opt_rescue");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj);
                      yyvalO = RubyParser::opt_rescue( yymarkPtr[-4].obj, yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, srcOfs, vps);
                    }
break;
case 394:
/* # line 2615 "grammar.y" */ 
	{
                      yTrace(vps, "exc_list: arg_value");
                      yyvalO = RubyArrayNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 397:
/* # line 2624 "grammar.y" */ 
	{
                      yTrace(vps, "exc_var: tASSOC lhs");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 399:
/* # line 2632 "grammar.y" */ 
	{
                      yTrace(vps, "opt_ensure: kENSURE comp_stamt");
                      /* $2 is argument block to rubyEnsure:*/
                      yyvalO = RubyEnsureNode::s(& yymarkPtr[0].obj/*nil arg coerced to RubyNilNode::new_*/ , yymarkPtr[-1].obj/*ensure token*/, vps);
                    }
break;
case 401:
/* # line 2641 "grammar.y" */ 
	{
                      yTrace(vps, "literal: numeric");
                      yyvalO= RubyAbstractNumberNode::s( yymarkPtr[0].obj , vps);
                    }
break;
case 402:
/* # line 2646 "grammar.y" */ 
	{
                      yTrace(vps, "literal: | symbol");
                      yyvalO = RubySymbolNode::s( quidToSymbolObj(yymarkPtr[0].obj, vps), vps);
                    }
break;
case 404:
/* # line 2654 "grammar.y" */ 
	{
                      yTrace(vps, "strings: string");
                      yyvalO = RubyParser::new_string(yymarkPtr[0].obj, vps);
                    }
break;
case 407:
/* # line 2663 "grammar.y" */ 
	{
                      yTrace(vps, "string: | string string1");
                      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 408:
/* # line 2670 "grammar.y" */ 
	{
                      yTrace(vps, "string1: tSTRING_BEG string_contents tSTRING_END");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 409:
/* # line 2677 "grammar.y" */ 
	{
                      yTrace(vps, "xstring: tXSTRING_BEG xstring_contents tSTRING_END");
                      yyvalO = RubyParser::new_xstring(yymarkPtr[-1].obj, vps);
                    }
break;
case 410:
/* # line 2684 "grammar.y" */ 
	{
                      yTrace(vps, "regexp: tREGEXP_BEG xstring_contents tREGEXP_END");
                      yyvalO = RubyParser::new_regexp( yymarkPtr[-1].obj, yymarkPtr[0].obj/*regexp options Si*/, vps);
                    }
break;
case 411:
/* # line 2691 "grammar.y" */ 
	{
                      yTrace(vps, "words: tWORDS_BEG tSPACE tSTRING_END");
                      yyvalO = RubyArrayNode::new_(vps);
                    }
break;
case 412:
/* # line 2696 "grammar.y" */ 
	{
                      yTrace(vps, "words: | tWORDS_BEG word_list tSTRING_END");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 413:
/* # line 2703 "grammar.y" */ 
	{
                      yTrace(vps, "word_list: none");
                      yyvalO = RubyArrayNode::new_(vps); /* $$  = 0;*/
                    }
break;
case 414:
/* # line 2708 "grammar.y" */ 
	{
                      yTrace(vps, "word_list: | word_list word tSPACE");
                      yyvalO = RubyParser::append_evstr2dstr( yymarkPtr[-2].obj , yymarkPtr[-1].obj, vps);
                    }
break;
case 416:
/* # line 2716 "grammar.y" */ 
	{
                      yTrace(vps, "word: | word string_content");
		      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 417:
/* # line 2723 "grammar.y" */ 
	{
		      yTrace(vps, "tQWORDS_BEG tSPACE tSTRING_END");
                      yyvalO = RubyArrayNode::new_(vps);
                    }
break;
case 418:
/* # line 2728 "grammar.y" */ 
	{
		      yTrace(vps, "tQWORDS_BEG qword_list tSTRING_END");
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 419:
/* # line 2735 "grammar.y" */ 
	{
                      yTrace(vps, "qword_list: none");
		      yyvalO = RubyArrayNode::new_(vps); /* $$  = 0;*/
                    }
break;
case 420:
/* # line 2740 "grammar.y" */ 
	{
                      yTrace(vps, "qword_list: | qword_list tSTRING_CONTENT tSPACE");
                      OmScopeType aScope(vps->omPtr);
                      NODE **strH = aScope.add(RubyStrNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyArrayNode::append(yymarkPtr[-2].obj, *strH, vps);/* returns first arg*/
                    }
break;
case 421:
/* # line 2749 "grammar.y" */ 
	{
		      yTrace(vps, "string_contents: none");
		      yyvalO = RubyStrNode::s( om::NewString(vps->omPtr , 0), vps);
                    }
break;
case 422:
/* # line 2754 "grammar.y" */ 
	{
                      yTrace(vps, "string_contents: | string_contents string_content");
		      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 423:
/* # line 2761 "grammar.y" */ 
	{
                      yTrace(vps, "xstring_contents: none");
		      yyvalO = ram_OOP_NIL;
                    }
break;
case 424:
/* # line 2766 "grammar.y" */ 
	{
                      yTrace(vps, "xstring_contents: | xstring_contents string_content");
		      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 425:
/* # line 2773 "grammar.y" */ 
	{
                      yTrace(vps,  "string_content: tSTRING_CONTENT" );
	              yyvalO = RubyStrNode::s( yymarkPtr[0].obj, vps );
                    }
break;
case 426:
/* # line 2778 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | tSTRING_DVAR");
		      vps->lex_state = EXPR_BEG;
                      yyvalO = vps->clear_lex_strterm();
                    }
break;
case 427:
/* # line 2784 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | string_dvar");
		      vps->set_lex_strterm( yymarkPtr[-1].obj);
		      yyvalO = RubyEvStrNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 428:
/* # line 2790 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | tSTRING_DBEG");
                      OmScopeType scp(vps->omPtr);
                      NODE **resH = scp.add( vps->clear_lex_strterm());
		      vps->lex_state = EXPR_BEG;
		      COND_PUSH(vps, 0);
		      CMDARG_PUSH(vps, 0);
		      yyvalO = *resH;
                    }
break;
case 429:
/* # line 2800 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | tSTRING_DBEG ___ comp_stamt tRCURLY");
		      vps->set_lex_strterm( yymarkPtr[-2].obj);
                      rParenLexPop(vps);
		      yyvalO = RubyParser::new_evstr(yymarkPtr[-1].obj, vps);
                    }
break;
case 430:
/* # line 2809 "grammar.y" */ 
	{
                      yTrace(vps, "string_dvar: tGVAR");
                      yyvalO = RubyGlobalVarNode::s( quidToSymbolObj( yymarkPtr[0].obj, vps), vps);
                   }
break;
case 431:
/* # line 2814 "grammar.y" */ 
	{
                      yTrace(vps, "string_dvar: | tIVAR");
                      yyvalO = RubyInstVarNode::s( quidToSymbolObj( yymarkPtr[0].obj, vps), vps);
                   }
break;
case 432:
/* # line 2819 "grammar.y" */ 
	{
                      yTrace(vps, "string_dvar: | tCVAR");
                      yyvalO = RubyClassVarNode::s( quidToSymbolObj( yymarkPtr[0].obj, vps), vps);
                   }
break;
case 434:
/* # line 2827 "grammar.y" */ 
	{
                      yTrace(vps, "symbol: tSYMBEG sym");
		      vps->lex_state = EXPR_END;
		      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 439:
/* # line 2841 "grammar.y" */ 
	{
                      yTrace(vps, "dsym: tSYMBEG xstring_contents tSTRING_END");
		      vps->lex_state = EXPR_END;
		      if ( yymarkPtr[-1].obj == ram_OOP_NIL) {
			rb_compile_error(vps, "empty symbol literal");
		      } else {
			yyvalO = RubyParser::new_dsym(yymarkPtr[-1].obj, vps);
		      }
                    }
break;
case 442:
/* # line 2855 "grammar.y" */ 
	{
                      yTrace(vps, "numeric: tUMINUS_NUM tINTEGER");
                      om *omPtr = vps->omPtr;
                      OmScopeType aScope(omPtr);
                      NODE **aH = aScope.add(yymarkPtr[0].obj);
                      yyvalO = LrgNegate(omPtr, aH);
                    }
break;
case 443:
/* # line 2863 "grammar.y" */ 
	{
                      yTrace(vps, "numeric: tUMINUS_NUM tFLOAT");
                      om *omPtr = vps->omPtr;
                      OmScopeType aScope(omPtr);
                      NODE **aH = aScope.add(yymarkPtr[0].obj);
                      double d;
                      if (! FloatPrimFetchArg(omPtr, aH, &d)) {
                        rb_compile_error(vps, "tUMINUS_NUM tFLOAT , number not a Float");
                      }
                      yyvalO = FloatPrimDoubleToOop(omPtr, d * -1.0 );
                    }
break;
case 449:
/* # line 2881 "grammar.y" */ 
	{ yyvalO = int64ToSi( kNIL) ; }
break;
case 450:
/* # line 2882 "grammar.y" */ 
	{ yyvalO = int64ToSi(kSELF); }
break;
case 451:
/* # line 2883 "grammar.y" */ 
	{ yyvalO = int64ToSi(kTRUE); }
break;
case 452:
/* # line 2884 "grammar.y" */ 
	{yyvalO = int64ToSi(kFALSE); }
break;
case 453:
/* # line 2885 "grammar.y" */ 
	{  yyvalO = int64ToSi(k__FILE__); }
break;
case 454:
/* # line 2886 "grammar.y" */ 
	{  yyvalO = int64ToSi(k__LINE__); }
break;
case 455:
/* # line 2890 "grammar.y" */ 
	{
                      yTrace(vps, "var_ref: variable");
                      yyvalO = gettable(vps, & yymarkPtr[0].obj);
                    }
break;
case 456:
/* # line 2897 "grammar.y" */ 
	{
                      yTrace(vps, "varLhs: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = assignable(& yymarkPtr[0].obj, ofsO, vps->nilH(), vps);
                    }
break;
case 457:
/* # line 2905 "grammar.y" */ 
	{
                    NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
		    yyvalO = RubyNthRefNode::s(yymarkPtr[0].obj/*a SmallInt*/, ofsO, vps);
                  }
break;
case 458:
/* # line 2910 "grammar.y" */ 
	{
		    yyvalO = RubyBackRefNode::s(yymarkPtr[0].obj/*a Character*/, vps);
                  }
break;
case 459:
/* # line 2916 "grammar.y" */ 
	{
                      yTrace(vps, "superclass: Term");
		      yyvalO = ram_OOP_NIL;
                    }
break;
case 460:
/* # line 2921 "grammar.y" */ 
	{
		      vps->lex_state = EXPR_BEG;
                    }
break;
case 461:
/* # line 2925 "grammar.y" */ 
	{
                      yTrace(vps, "superclass: | tLT expr_value Term");
                      yyvalO = yymarkPtr[-1].obj; 
                    }
break;
case 462:
/* # line 2929 "grammar.y" */ 
	{ yyerrflag = 0; yyvalO = ram_OOP_NIL;}
break;
case 463:
/* # line 2933 "grammar.y" */ 
	{
                      yTrace(vps, "f_arglist: tLPAREN2 f_args opt_nl tRPAREN");
                      rParenLexPop(vps);
		      yyvalO = yymarkPtr[-2].obj;
		      vps->lex_state = EXPR_BEG;
		      vps->command_start = TRUE;
                    }
break;
case 464:
/* # line 2941 "grammar.y" */ 
	{
                      yTrace(vps, "f_arglist: | f_args Term");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 465:
/* # line 2948 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: f_arg tCOMMA f_optarg tCOMMA f_rest_arg opt_f_block_arg");
		      RubyArgsNode::add_optional_arg(yymarkPtr[-5].obj, yymarkPtr[-3].obj, vps);
		      RubyArgsNode::add_star_arg(yymarkPtr[-5].obj, yymarkPtr[-1].obj, vps);
		      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-5].obj, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 466:
/* # line 2955 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_arg tCOMMA f_optarg  opt_f_block_arg");
                      RubyArgsNode::add_optional_arg(yymarkPtr[-3].obj, yymarkPtr[-1].obj, vps);
		      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 467:
/* # line 2961 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_arg tCOMMA  f_rest_arg opt_f_block_arg");
                      RubyArgsNode::add_star_arg(yymarkPtr[-3].obj, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 468:
/* # line 2967 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_arg  opt_f_block_arg");
                      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 469:
/* # line 2972 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_optarg tCOMMA f_rest_arg opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_optional_arg(*argsH, yymarkPtr[-3].obj, vps);
                      RubyArgsNode::add_star_arg(*argsH, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 470:
/* # line 2981 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: |  f_optarg  opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_optional_arg(*argsH, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 471:
/* # line 2989 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_rest_arg opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_star_arg(*argsH, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 472:
/* # line 2997 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: |  f_blck_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps);
                    }
break;
case 473:
/* # line 3004 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | <nothing>");
		      yyvalO = RubyArgsNode::new_(vps);
                    }
break;
case 474:
/* # line 3011 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be a constant");
                    }
break;
case 475:
/* # line 3015 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be an instance variable");
                    }
break;
case 476:
/* # line 3019 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be a global variable");
                    }
break;
case 477:
/* # line 3023 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be a class variable");
                    }
break;
case 478:
/* # line 3027 "grammar.y" */ 
	{
                      yTrace(vps, "f_norm_arg: | tIDENTIFIER");
                      OmScopeType aScope(vps->omPtr);
                      NODE *quidO = asQuid(yymarkPtr[0].obj, vps);
		      if (! is_local_id(quidO)) {
			  rb_compile_error_q(vps, "formal argument must be local variable", quidO);
		      } else if (local_id(vps, quidO)) {
			  rb_compile_error_q(vps, "duplicate argument name", quidO);
                      }
		      local_cnt(vps, quidO);
		      yyvalO = yymarkPtr[0].obj ;
                    }
break;
case 479:
/* # line 3042 "grammar.y" */ 
	{ yTrace(vps, "f_arg: f_norm_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      yyvalO = RubyArgsNode::add_arg(argsH, yymarkPtr[0].obj/*RpNameToken*/, vps);  /* returns first arg*/
                    }
break;
case 480:
/* # line 3049 "grammar.y" */ 
	{
                      yTrace(vps, "f_arg: | f_arg tCOMMA f_norm_arg");
                      yyvalO = RubyArgsNode::add_arg(& yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, vps); 
                    }
break;
case 481:
/* # line 3056 "grammar.y" */ 
	{
                      yTrace(vps, "f_opt: tIDENTIFIER tEQL arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE *quidO = asQuid(yymarkPtr[-2].obj, vps);
		      if (! is_local_id(quidO)) {
			  rb_compile_error_q(vps, "formal argument must be local variable", quidO);
		      } else if (local_id(vps, quidO)) {
			  rb_compile_error_q(vps, "duplicate optional argument name", quidO);
                      } 
                      NODE **thirdH = aScope.add(yymarkPtr[0].obj);
		      yyvalO = assignable(& yymarkPtr[-2].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, thirdH, vps);
                    }
break;
case 482:
/* # line 3071 "grammar.y" */ 
	{
                      yTrace(vps, "f_optarg: f_opt");
                      yyvalO = RubyBlockNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 483:
/* # line 3076 "grammar.y" */ 
	{
                      yTrace(vps, "f_optarg: | f_optarg tCOMMA f_opt");
                      yyvalO = RubyBlockNode::append_to_block(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 486:
/* # line 3087 "grammar.y" */ 
	{
                      yTrace(vps, "f_rest_arg: restarg_mark tIDENTIFIER");
                      NODE *quidO = asQuid(yymarkPtr[0].obj, vps);
		      if (! is_local_id(quidO)) {
			  rb_compile_error("rest argument must be local variable", vps);
		      } else if (local_id(vps, quidO)) {
			  rb_compile_error("duplicate rest argument name", vps);
                      }
		      local_cnt(vps, quidO);
		      yyvalO = yymarkPtr[0].obj /* a RpNameToken or quid*/;
                    }
break;
case 487:
/* # line 3099 "grammar.y" */ 
	{
                      yTrace(vps, "f_rest_arg: | restarg_mark");
                      yyvalO = RpNameToken::s(a_sym_rest_args, vps);
                    }
break;
case 490:
/* # line 3110 "grammar.y" */ 
	{
                      yTrace(vps, "f_blck_arg: blkarg_mark tIDENTIFIER");
                      NODE *quidO = asQuid(yymarkPtr[0].obj, vps);
		      if (! is_local_id(quidO)) {
			  rb_compile_error("block argument must be local variable", vps);
		      } else if (local_id(vps, quidO)) {
			  rb_compile_error("duplicate block argument name", vps);
                      }
		      local_cnt(vps, quidO);
		      yyvalO = RubyBlockArgNode::s(RpNameToken::symval(yymarkPtr[0].obj, vps), vps);
                    }
break;
case 491:
/* # line 3124 "grammar.y" */ 
	{
                      yTrace(vps, "opt_f_block_arg: tCOMMA f_blck_arg");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 492:
/* # line 3129 "grammar.y" */ 
	{
                      yTrace(vps, "opt_f_block_arg: | <nothing>");
                      yyvalO = ram_OOP_NIL;
                    }
break;
case 493:
/* # line 3136 "grammar.y" */ 
	{
                        yTrace(vps, "singleton : var_ref");
                        yyvalO = yymarkPtr[0].obj;
                    }
break;
case 494:
/* # line 3140 "grammar.y" */ 
	{ vps->lex_state = EXPR_BEG;}
break;
case 495:
/* # line 3141 "grammar.y" */ 
	{
                       yTrace(vps, "singleton: ___ expr opt_nl tRPAREN");
                       rParenLexPop(vps);
                       if (yymarkPtr[-2].obj == ram_OOP_NIL) {
                         rb_compile_error("can't define singleton method for ().", vps);
                       } else if (RubyAbstractLiteralNode::kind_of(yymarkPtr[-2].obj, vps)) {
                         rb_compile_error("can't define singleton method for literals", vps);
                       }
                       yyvalO = yymarkPtr[-2].obj; /* rubinius had  $$  =  value_expr($3);*/
                    }
break;
case 496:
/* # line 3154 "grammar.y" */ 
	{
                      yTrace(vps, "assoc_list: none");
                      yyvalO = RubyArrayNode::new_(vps);
                    }
break;
case 497:
/* # line 3159 "grammar.y" */ 
	{
                      yTrace(vps, "assoc_list: | assocs trailer");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 498:
/* # line 3164 "grammar.y" */ 
	{
                      yTrace(vps, "assoc_list: | args trailer");
                      if ((RubyArrayNode::arrayLength(yymarkPtr[-1].obj, vps) & 1) != 0) {
                        rb_compile_error("odd number list for Hash", vps);
                      }
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 500:
/* # line 3175 "grammar.y" */ 
	{
                      yTrace(vps, "assocs: | assocs tCOMMA assoc");
                      yyvalO = RubyArrayNode::appendAll(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 501:
/* # line 3182 "grammar.y" */ 
	{
                      yTrace(vps, "assoc: arg_value tASSOC arg_value");
                      yyvalO = RubyArrayNode::s_a_b( yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 502:
/* # line 3187 "grammar.y" */ 
	{
                      yTrace(vps, "assoc: arg_value tLABEL arg_value");
                      yyvalO = RubyArrayNode::s_a_b(RubySymbolNode::s(yymarkPtr[-1].obj, vps), yymarkPtr[0].obj, vps);
                    }
break;
case 522:
/* # line 3226 "grammar.y" */ 
	{ yyerrflag = 0 ;}
break;
case 525:
/* # line 3231 "grammar.y" */ 
	{ yyerrflag = 0;}
break;
case 526:
/* # line 3234 "grammar.y" */ 
	{  yTrace(vps, "none:");  yyvalO = ram_OOP_NIL; }
break;
/* # line 12471 "rubygrammar.c" */ 
    }
    if (yyvalO == NULL) {  /*compute default state result*/ 
      if (yyvalPtr != NULL) {
        yyvalO = yyvalPtr->obj;
      } else {
        yyvalO = ram_OOP_NIL;
      }
    }
    yymarkPtr -= yym;
    yystack->mark = yymarkPtr ;
    yystate = yymarkPtr->state ;
    yym = unifiedTable[yyn + lhsBASE]; /* yylhs[yyn]*/ ;
    if ((yystate | yym) == 0 /*yystate==0 && yym == 0*/ ) {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reductionZ, shifting from state 0 to state %d\n", 
                       YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        UTL_ASSERT(yymarkPtr < yystack->last);
        yymarkPtr += 1;
        yystack->mark = yymarkPtr ; 
        yymarkPtr->state = YYFINAL;
        yymarkPtr->obj = yyvalO ;
        if (yychar < 0) {
            yychar = yylex(vps); 
            UTL_ASSERT(yychar >= 0); 
#if YYDEBUG
            if (yydebug) {
                const char* yys = NULL;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
                yytrap();
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    yyn = unifiedTable[yym + gindexBASE]/*yygindex[x]*/;
    if (yyn) {
      yyn += yystate;
      if ((uint64)yyn <= YYTABLESIZE) {
        int yChk = unifiedTable[yyn + checkBASE]/*yycheck[yyn]*/;
        if (yChk == yystate) {
          yystate = unifiedTable[yyn + tableBASE];
          goto reduction2 ;
    }}}
    yystate = unifiedTable[yym + dgotoBASE]/*yydgoto[yym]*/ ;
reduction2: ;
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d to state %ld\n", 
               YYPREFIX, yystack->mark->state, yystate);
#endif
    if (yymarkPtr >= yystack->last) { 
        *vps->yyvalH = yyvalO;
        yymarkPtr = yygrowstack(vps, yymarkPtr); 
        if (yymarkPtr == NULL) { 
           yyerror("yacc stack overflow", vps);
           return 1;
        } 
        yyvalO = *vps->yyvalH; 
    }
    yymarkPtr += 1 ; 
    yystack->mark = yymarkPtr ; 
    yymarkPtr->state = yystate ; 
    yymarkPtr->obj = yyvalO; 
    goto yyloop;


yyaccept:
    /* yyfreestack(&yystack);*/ 
    return 0;
}
