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

/* # line 512 "rubygrammar.c" */ 
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
   15,   15,   15,    2,    3,    3,    4,    5,    6,   10,
   10,   12,   12,   14,   14,   11,   11,   13,   13,    7,
    7,    8,    8,    9,  146,    9,  147,    9,   68,   68,
   68,   68,   87,   86,   86,   86,   86,   17,   16,   16,
   16,   16,   85,   85,   85,   85,   85,   85,   85,   85,
   85,   85,   85,   51,   52,   67,   67,   54,  148,   54,
   54,   57,   57,   58,   58,   58,   58,   58,   58,   58,
   58,   58,   95,   95,   95,   95,   95,   96,   96,   60,
   59,   59,  149,  149,   94,   94,  150,  150,   61,   62,
   62,    1,  151,    1,   63,   63,   63,   64,   64,   65,
   88,   88,   88,   89,   89,   89,   89,   90,   90,   90,
  137,  137,   98,   98,  107,  107,  110,  110,  110,  132,
  132,   99,   99,   77,
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
    1,    1,    1,    1,    1,    2,    3,    3,    3,    3,
    3,    0,    3,    1,    2,    3,    3,    0,    3,    0,
    2,    0,    2,    1,    0,    3,    0,    4,    1,    1,
    1,    1,    2,    1,    1,    1,    1,    3,    1,    1,
    2,    2,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    0,    4,
    2,    4,    2,    6,    4,    4,    2,    4,    2,    2,
    1,    0,    1,    1,    1,    1,    1,    1,    3,    3,
    1,    3,    1,    1,    2,    1,    1,    1,    2,    2,
    1,    1,    0,    5,    1,    2,    2,    1,    3,    3,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    0,    1,    0,    1,    0,    1,    1,    1,
    1,    1,    2,    0,
/* start of table defred */  1, 
    0,    0,    0,    0,    0,    0,    0,  279,  298,  300,
    0,  302,  305,  314,    0,    0,  332,  333,    0,    0,
    0,  449,  448,  450,  451,    0,    0,    0,   20,    0,
  453,  452,    0,    0,  445,  444,    0,  447,  422,  439,
  440,  456,  457,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  420,  422,    0,    0,    0,    0,    0,
  271,    0,  405,  272,  273,  274,  275,  270,  401,  403,
    2,    0,    0,    0,    0,    0,    0,   35,    0,    0,
  276,    0,    0,   43,    0,    0,    5,    0,    0,   61,
    0,   71,    0,  402,    0,    0,  330,  331,  289,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  334,
    0,  277,  454,    0,   95,  323,  143,  154,  144,  167,
  140,  160,  150,  149,  165,  148,  147,  142,  168,  152,
  141,  155,  159,  161,  153,  146,  162,  169,  164,    0,
    0,    0,    0,  139,  158,  157,  170,  171,  172,  173,
  174,  138,  145,  136,  137,    0,    0,    0,   99,    0,
  129,  130,  127,  111,  112,  113,  116,  118,  114,  131,
  132,  119,  120,  124,  115,  117,  108,  109,  110,  121,
  122,  123,  125,  126,  128,  133,  493,    0,  492,  325,
  100,  101,  163,  156,  166,  151,  134,  135,   97,   98,
  104,    0,  105,  103,  102,    0,    0,    0,  521,  520,
    0,    0,    0,  522,    0,    0,    0,    0,    0,    0,
    0,    0,  234,    0,    0,    0,   45,  242,    0,    0,
  498,    0,    0,    0,   46,   44,    0,   60,    0,    0,
  378,   59,   38,    0,    9,  516,    0,    0,    0,    0,
  195,    0,    0,  504,  506,  505,  377,  507,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  222,
    0,    0,    0,  495,    0,    0,    0,   69,    0,  436,
  435,  437,    0,  433,  434,    0,    0,    0,    0,    0,
    0,    0,    0,  210,   39,  211,  406,    4,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  218,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  366,  369,  383,  380,  297,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   73,  372,    0,  295,    0,    0,   92,    0,   94,
  441,  442,    0,  459,  318,  458,    0,    0,  286,    0,
    0,  512,  511,  327,    0,  106,    0,    0,    0,    0,
    0,    0,    0,  523,    0,    0,    0,    0,    0,    0,
    0,  346,  347,    0,    0,    0,  262,    0,    0,    0,
    0,    0,  235,  264,    0,    0,  237,    0,    0,  291,
    0,    0,  257,  256,    0,    0,    0,    0,    0,   11,
   13,   12,    0,  293,    0,    0,    0,  424,  427,  425,
  408,  423,    0,    0,    0,    0,  283,    0,    0,    0,
  223,    0,  518,  224,  287,    0,  226,    0,  497,  288,
  496,    0,    0,    0,    0,  438,  407,  421,  409,  410,
  411,  414,    0,  416,    0,  417,    0,    0,    0,   15,
   16,   17,   18,   19,   36,   37,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  375,
    0,    0,    0,    0,    0,  376,    0,    0,   25,    0,
    0,    0,   30,    0,    0,   23,  265,    0,   31,   24,
    0,   33,    0,   67,   74,   50,   54,    0,  461,    0,
    0,    0,    0,    0,   96,    0,    0,    0,    0,    0,
  475,  474,  473,  476,  484,  488,  487,  483,    0,    0,
    0,    0,  481,  471,    0,  478,    0,    0,    0,    0,
  280,    0,    0,  393,  337,  336,    0,    0,    0,    0,
    0,    0,    0,  341,  340,  303,  339,  306,    0,    0,
    0,    0,  315,    0,  241,  500,    0,    0,    0,    0,
    0,    0,    0,  263,    0,    0,    0,  499,    0,  290,
    0,    0,    0,  260,  254,    0,    0,    0,    0,    0,
    0,    0,  228,   10,    0,    0,    0,   22,    0,    0,
    0,    0,    0,  227,    0,  266,    0,    0,    0,    0,
  413,  415,  419,    0,    0,    0,  364,    0,  367,  362,
  384,  381,    0,    0,  374,    0,    0,    0,  233,  373,
    0,  232,   75,    0,   26,  371,   49,  370,   48,  269,
    0,    0,   72,    0,  321,    0,    0,  324,    0,  328,
    0,    0,    0,  463,    0,  469,  491,    0,  470,    0,
  467,  485,  489,  107,    0,    0,  395,  396,    0,    0,
  344,    0,  338,    0,    0,    0,    0,  311,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  239,    0,    0,    0,    0,    0,  247,  259,
    0,    0,  229,    0,    0,  230,    0,   21,    0,  429,
  430,  431,  432,  426,  282,    0,    0,    0,    0,  363,
    0,    0,  348,    0,    0,    0,    0,   29,    0,   58,
    0,   27,    0,   28,   56,    0,    0,    0,   51,    0,
  460,  319,  494,    0,  480,    0,  326,    0,  482,  490,
    0,    0,    0,  479,    0,    0,  398,  345,    0,    3,
  400,    0,    0,  342,    0,  389,    0,    0,  313,  309,
    0,    0,    0,  236,    0,  238,  253,    0,    0,  244,
    0,  261,    0,    0,  294,  428,  225,    0,    0,    0,
    0,    0,    0,    0,  361,  365,    0,    0,    0,    0,
  268,    0,    0,    0,  462,  468,    0,  465,  466,  397,
    0,  399,    0,  299,  301,    0,    0,  304,  307,  316,
    0,    0,    0,  243,    0,  249,    0,  231,    0,    0,
    0,    0,    0,    0,    0,    0,  349,  368,  385,  382,
    0,  322,    0,    0,    0,    0,  388,  390,  391,  386,
    0,  240,  245,    0,    0,    0,  248,  358,    0,    0,
    0,    0,    0,    0,    0,  352,   52,  329,  464,  392,
    0,    0,    0,    0,  250,    0,  357,    0,    0,  343,
  317,  246,    0,  251,  354,    0,    0,  353,  252,
/* start of table dgoto */  1, 
  188,   61,   62,   63,   64,   65,  286,  250,  432,   66,
   67,  289,  291,  463,   68,   69,   70,  109,  377,  378,
   72,   73,   74,   75,   76,   77,   78,   79,  380,  224,
  252,  793,  794,  581,  880,  573,  696,  786,  790,  226,
  707,  227,  614,  414,  659,  660,  238,  268,  403,  604,
   81,  229,  529,  365,   83,   84,  560,  561,  562,  563,
  780,  686,  272,  230,  231,  202,  232,  744,  391,  751,
  649,  752,  355,  537,  334,  233,   87,  203,   88,   89,
   90,  263,   91,   92,  234,  284,   94,  114,  544,  510,
  115,  205,  258,  565,  566,  567,    2,  211,  212,  423,
  248,  401,  674,  832,  192,  570,  247,  425,  492,  444,
  239,  617,  727,  206,  439,  625,  207,  577,  208,  215,
  586,  711,  216,  712,  213,  382,  383,  217,  717,  881,
  541,  578,  538,  770,  370,  375,  374,  549,  774,  503,
  754,  505,  756,  504,  755,  630,  629,  540,  568,  569,
  371,
/* start of table sindex */  0, 
    0,13600,14030,19140, 6461,17364,17063,    0,    0,    0,
  124,    0,    0,    0,14430,14430,    0,    0,14430,   77,
   83,    0,    0,    0,    0,15606,16970,   48,    0,  -21,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,16488,16488,17789, -168,13817,15606,15704,
16488,19434,17455,    0,    0,  141,  177,  261,16586,16488,
    0, -186,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  124,  379,  242,18764,    0,  -52,    0,  -68,    3,
    0, -103,   53,    0,  -79,  207,    0,  210,19238,    0,
  234,    0,    0,    0,   72,  379,    0,    0,    0,   77,
   83,   48,    0,    0,15606, -107,13600,  310,  129,    0,
   54,    0,    0,   72,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   62,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  243,    0,    0,    0,13600,15606,15606,    0,    0,
    0,  256,15606,    0,15606,15606,19336,   55,16488,16488,
16488,18764,    0,  -47,   76,  336,    0,    0,   59,  343,
    0,   98,  405,    0,    0,    0,14132,    0,14528,14430,
    0,    0,    0,  328,    0,    0,  400,  357,13600,  307,
    0,  102,  166,    0,    0,    0,    0,    0,  156,13817,
  461,    0,  468,  242,16488,   48,   94,  444,  115,    0,
  119,  368,  115,    0,  206,  154,    0,    0,    0,    0,
    0,    0,  371,    0,    0,  532,  205,  163,  625,  186,
 -200,  225,  229,    0,    0,    0,    0,    0,13921,15606,
15606,15606,15606,14030,15606,15606,16488,16488,16488,16488,
16488,16488,16488,16488,16488,16488,16488,16488,16488,16488,
16488,    0,16488,16488,16488,16488,16488,16488,16488,16488,
16488,16488,    0,    0,    0,    0,    0,17824,17857,15704,
17789,  214,16586,17789,17789,16586,15802,15802,13817,19434,
  537,    0,    0,  228,    0,  400,  242,    0,    0,    0,
    0,    0,  124,    0,    0,    0,18158,17789,    0,13600,
15606,    0,    0,    0,  639,    0,  320,  330,  242,   91,
   91,  333,  341,    0,  124,   68,   68,  304,  171,    0,
  340,    0,    0,    0,  156,  595,    0,16488,18191,18226,
  302,14626,    0,    0,16488,14724,    0,16488,16488,    0,
  603,14234,    0,    0,  -52,  601,   48,   44,  607,    0,
    0,    0,17063,    0,16488,13600,  524,    0,    0,    0,
    0,    0,18191,18226,16488,  611,    0,    0,   48,13122,
    0,15900,    0,    0,    0,16488,    0,16488,    0,    0,
    0,    0,18259,18294,    0,    0,    0,    0,    0,    0,
    0,    0,   79,    0,  637,    0,16488,16488,  379,    0,
    0,    0,    0,    0,    0,    0,  166, 1874, 1874, 1874,
 1874,  430,  430,11008,17484, 1874, 1874, 7234, 7234,  416,
  416,16488,  430,  430,  917,  917,  992,  237,  237,  166,
  166,  166,  -71,  -71,  -71,  347,    0,  350,   83,    0,
    0,  354,  359,   83,  587,    0,16586,18764,    0,   83,
   83,18764,    0,16488,13640,    0,    0,  655,    0,    0,
    0,    0,  659,    0,    0,    0,    0,  124,    0,15606,
13600,    0,    0,   83,    0,   83,  437,  106,17756,  641,
    0,    0,    0,    0,    0,    0,    0,    0,  717,13600,
  124,  667,    0,    0,  668,    0,  669,  399,  410,17063,
    0,15998,  447,    0,    0,    0,13600,  463,13600,16096,
  470,13600,  333,    0,    0,    0,    0,    0,    0,18327,
18362,    0,    0,  389,    0,    0,  419,  350,  424,  434,
16488,16488,  -47,    0,  700,16488,  -47,    0,13640,    0,
16488,18764,  -13,    0,    0,  705,  737,14822,  739,17789,
17789,  743,    0,    0,15606,18764,  661,    0,13600,  502,
18764,    0,  747,    0,16488,    0,    0,    0,    0,    0,
    0,    0,    0,  166,  166,17111,    0,18944,    0,    0,
    0,    0,16586,16488,    0,  228,16586,16586,    0,    0,
  228,    0,    0,18764,    0,    0,    0,    0,    0,    0,
16488,16194,    0,  -71,    0,  124,  527,    0,  751,    0,
16488,   48,  531,    0,  142,    0,    0,   -6,    0,  717,
    0,    0,    0,    0,    0,  464,    0,    0,13600,  538,
    0,   43,    0,  447,16488,  761,   91,    0,  551,  554,
13600,13600,    0,    0,    0,    0,15606,16488,16488,16488,
  595,14920,    0,  595,  595,15018,  782,15116,    0,    0,
  -52,   44,    0,   83,   83,    0,   32,    0,  699,    0,
    0,    0,    0,    0,    0,13122,16488, 5580,19532,    0,
  702,  787,    0,13600,13600,13600,18764,    0,18764,    0,
18764,    0,18764,    0,    0,18764,16488,    0,    0,13600,
    0,    0,    0,  639,    0,  795,    0,  641,    0,    0,
  668,  794,  668,    0,19532,   91,    0,    0,13600,    0,
    0,15606,  578,    0,  584,    0,16292,13600,    0,    0,
  585,  588,   68,    0,16488,    0,    0,16488,  807,    0,
  810,    0,16488,  814,    0,    0,    0,18764,  549,  513,
  194,    0,  821,    0,    0,    0,19042,  606,  608,  746,
    0,13600,  612,13600,    0,    0,  142,    0,    0,    0,
13600,    0,   91,    0,    0,16488, -192,    0,    0,    0,
18731,  595,15214,    0,15312,    0,  595,    0,    0,19532,
18663,18696,    0,  536, 6078,19532,    0,    0,    0,    0,
  753,    0,  632,  668,  330,13600,    0,    0,    0,    0,
13600,    0,    0,16488,  845,16488,    0,    0,    0,    0,
    0,    0,19532,  550,  856,    0,    0,    0,    0,    0,
   43,  646,  595,15410,    0,  595,    0,19532,  555,    0,
    0,    0,16488,    0,    0,19532,  595,    0,    0,
/* start of table rindex */  0, 
    0,  132,    0,    0,    0,    0,    0,    0,    0,    0,
14332,    0,    0,    0,12908,13030,    0,    0,13212, 4606,
 4077,    0,    0,    0,    0,    0,    0,16390,    0,    0,
    0,    0, 2353, 3313,    0,    0, 2451,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  161,    0,  812,
  790,  456,  703,    0,    0,  785, -123,    0,    0,    0,
    0, 8331,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  460, 1330, 7170,18839, 8416,18850,    0, 8512,    0,
    0,    0,18901,    0,13426,    0,    0,    0,  466,    0,
    0,    0,13331,    0,15508, 1571,    0,    0,    0, 8818,
 7354,  877, 5904, 6184,    0,    0,  161,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0, 1105,
 1155, 1180, 1386,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0, 1622, 1757, 1812,    0, 1941,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0, 7656,    0,    0,    0,  558,    0,    0,    0,    0,
   84,  143,    0,    0,    0,    0,    0,    0,    0,    0,
    0,12026,    0, 2172,    0, 8146,    0,    0,    0, 8146,
    0, 7844,    0, 7443,    0,    0,    0,    0,    0,  880,
    0,    0,    0,    0,    0,    0,16684,    0,   71,    0,
    0,    0, 9213,    0,    0,    0,    0,    0,13516,  161,
    0,  211,    0,  151,    0,  829,  832,    0,  832,    0,
  802,    0,  802,    0,    0,    0, 1523,    0, 1733,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0, 8903, 8999,    0,    0,    0,    0,    0, 1458,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  812,
    0,13730,    0,    0,    0,    0,    0,    0,  161,  485,
  495,    0,    0, 8633,    0,    0,  144,    0, 6670,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  558,
    0,    0,    0,    0,  159,    0,    0,  530, 8267,    0,
    0,    0,    0,    0,  660,    0,    0,    0,    0,  107,
    0,    0,    0,  175, 7929, 8146,    0,    0,    0,    0,
 8025,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  889,    0,    0,  239,  249,  895,  895,    0,    0,
    0,    0,    0,    0,    0,   71,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  126,  895,  829,
    0,  860,    0,    0,    0,  -27,    0,  831,    0,    0,
    0, 1864,    0,    0, 1964,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0, 5561,    0,
    0,    0,    0,    0,    0,    0, 9305, 9119,11306,11391,
11485, 2267,11030,11572,11847,11666,11751, 1111,11932,10507,
10602,    0,11116,11210,10816,10935,10687,10259,10354, 9394,
 9694, 9779, 6890, 6890, 7032, 4939, 3646, 5468,15508,    0,
 3744, 5037, 5370, 4175,    0,    0,    0,12063,    0, 5801,
 5801,12123,    0,    0, 9992,    0,    0,    0,    0,    0,
 5130,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  558, 6287, 6567,    0,    0, 7538,    0,  895,    0,  697,
    0,    0,    0,    0,    0,    0,    0,    0,  411,  558,
    0,  161,    0,    0,  161,    0,  161,  738,    0,    0,
    0,   27,  362,    0,    0,    0,  426, 6775,  503,    0,
    0,  218,    0,    0,    0,    0,    0,    0,  523,    0,
    0,  898,    0,    0,    0,    0, 2784, 4508, 2882, 3215,
    0,    0,12760,    0, 8146,    0,    0,    0, 1051,    0,
    0,  111,    0,    0,    0,  880,    0,    0,    0,    0,
    0,    0,    0,    0,    0,12161,    0,    0,   71,    0,
12221,  545,    0,    0,    0,    0, 1612,  567, 2180, 5921,
    0,    0,    0, 9874,10174,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,12847,    0,    0,    0,    0,
18658,    0,    0,12340,    0,    0,    0,    0,    0,    0,
    0,    0,    0, 7032,    0,    0,    0,    0,    0,    0,
    0,  895,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   24,   99,    0,    0,  619,  701,
    0,  701,    0,  701,    0,  127,    0,    0,    0,    0,
  218,  218,  789,  204, 1090, 1194,    0,    0,    0,    0,
 8146,    0,    0, 8146,  880,    0,    0,    0,    0,    0,
    0,  895,    0,   78,   78,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  829,    0,  837,    0,    0,
    0,  848,    0,  218,  218,   71,12435,    0,12495,    0,
12531,    0,12569,    0,    0,12629,    0, 1475,    0,  558,
    0,    0,    0,  159,    0,    0,    0,    0,    0,    0,
  161,  161,  161,    0,    0,    0,    0,    0,  218,    0,
    0,    0,    0,    0,    0,    0,    0,  396,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  880,    0,
  880,    0,    0,    0,    0,    0,    0,12724,    0,    0,
    0,  197,  854,  849,    0,    0,  864,    0,    0,    0,
    0,   71,    0,  558,    0,    0,    0,    0,    0,    0,
  558,    0,    0,    0,    0,    0,  701,    0,    0,    0,
 8146,  880,    0,    0,    0,    0,  880,    0,  891,    0,
    0,    0,  984,    0,  868,    0,    0,    0,    0,    0,
    0,    0,    0,  161,  530,  426,    0,    0,    0,    0,
  218,    0,    0,    0,  880,    0,    0,    0, 1026,  452,
 2039, 2241,    0,    0,  869,    0,    0,    0,    0,    0,
  701,    0,  880,    0,    0,  880,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  880,    0,    0,
/* start of table gindex */  0, 
    0,    0,    0,  922,    0,    0,    0,  421, -171,    0,
    0,    0,    0,    0,    0,    0,    8,  990, -299,   21,
    0,   35,  117,  952,   92,   14,   13,    0, -124, 1497,
   -2,   97, -530, -539,    0,  130,    0,    0,    0,   25,
    0,  689,    0,    0,   19,  133,  -15,  664, -221,   -8,
  993, 1241, -296,    0, -230,    0,  227,  448,  319, -629,
 -349, -405,    0,   -9, -339,    0,   58,    0,    0,    0,
   18,    0,  934, -422,    0,  334, 1471,  -23,  797,    0,
   22, -167,  -84,  -24,  221,    0,    9, 1336,  -25,    0,
  -61,    6,   12, -554,  327,    0,    0,  -50,  946,    0,
    0,  -60,    0,    0,    0,    0,   39,    0,    0,  279,
    0,    0,    0,    0,    0,    0,    0, -356,    0,    0,
 -360,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   63,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,
/* start of table table */  80, 
   80,  111,  111,  245,  351,  242,  228,  228,  407,  418,
  228,  190,  225,  225,  201,  204,  225,  191,  191,  342,
  257,  298,   71,  225,  579,  564,  588,  278,  223,  223,
  726,  557,  223,  394,  201,  204,  524,   96,  191,  241,
  269,  273,  700,  710,  360,   80,  225,  225,  339,  276,
  369,  532,  648,  246,  336,  779,  225,  246,  285,   85,
   85,  112,  112,  266,  191,  519,  608,  265,  261,  262,
  547,  295,  815,  214,  267,  271,  699,  209,  580,  354,
  524,  394,  394,  381,  524,  524,  276,  524,  385,  621,
  386,  387,  436,  340,  341,  110,  110,  519,  345,  368,
  209,  249,  225,  443,   80,   85,  608,  373,  524,  277,
  641,  240,  465,  353,  458,  246,  237,  462,  524,  241,
   40,  400,  240,  524,  443,  585,  210,  261,  443,  524,
  781,  524,  353,  209,  214,  783,  387,  442,  209,  259,
  356,  524,  243,  110,  340,  341,  277,  434,  576,  210,
   83,   40,  277,  320,  221,  466,  524,  524,  446,  689,
  281,  691,  448,   54,   85,  264,  340,  341,  472,   72,
  524,  366,  288,  795,  595,  470,  471,  472,  473,  557,
  110,  533,  210,  558,  387,  387,   62,  210,  364,  418,
  524,  281,  340,  341,  336,  524,  358,  277,  277,  454,
  359,  524,  320,   80,  225,  225,   91,  779,  290,  564,
  225,  335,  225,  225,  389,  455,  591,  472,   76,  524,
  454,  357,   93,   93,  113,  113,  113,  524,  228,  333,
  228,  228,  418,  760,  225,  343,  225,  225,  765,  862,
   91,  677,  277,  505,  340,  341,   80,   81,  234,  284,
  223,   63,  223,  415,   91,   91,  647,   80,  524,  346,
  683,  340,  341,   85,  416,  454,  454,  347,   93,  427,
  348,   61,  279,  332,  390,  697,  524,  352,  330,  234,
  261,  262,  874,  331,  340,  341,  376,  277,  277,  524,
  394,  642,  398,  524,  284,  284,   80,  225,  225,  225,
  225,   80,  225,  225,  441,  545,   85,  879,  110,  279,
  792,  699,  509,  514,  384,  516,  878,   85,  520,  521,
   91,  305,  306,  379,  379,  534,  398,   93,  592,  379,
  269,  379,  379,  469,  583,   86,   86,  225,  474,  338,
  225,  556,  546,  225,  225,  225,   80,  276,  584,  511,
  798,  335,  312,  266,  310,  519,   85,  575,  523,  526,
  530,   85,  395,  394,  267,  524,  524,   80,  225,  261,
  262,  528,  528,  509,  514,  836,  838,  839,  511,  402,
  620,   86,  608,  723,  363,   91,  406,  732,  608,  344,
  367,  428,  605,  387,  729,  305,  306,  405,  372,  624,
   40,   40,  616,  615,   62,  524,   85,  277,  546,  225,
  511,  514,  399,  514,  524,  676,  379,  379,  379,  379,
  472,  475,  476,   80,  564,  539,   93,   85,  546,  841,
  201,  204,  429,  430,  191,  524,  408,  394,  433,  424,
   86,  110,  850,  277,  511,  778,  627,  214,  587,  587,
  221,  472,  332,   84,  524,  619,  622,  330,  328,  513,
  329,   89,  331,   91,  511,  409,  332,  327,  899,   93,
  833,  330,  328,  283,  329,  287,  331,  633,  634,  426,
   93,  524,   89,   85,  524,  307,  876,  548,  555,  556,
  453,  505,  450,  656,  435,   89,   70,  284,  661,  804,
  513,  437,  806,  807,  667,  669,   65,  590,  438,   89,
   89,  454,  524,  452,  225,  608,   70,  428,  460,   93,
  459,  651,  652,  326,   93,   68,   65,  655,  264,  665,
  861,  305,  306,  277,  873,   66,  445,  225,   80,   86,
  284,  464,  284,  284,  467,   68,  694,  447,  468,  449,
  392,  451,  517,  325,  680,   66,  307,   80,  429,  430,
  191,  524,  655,  753,  608,  546,   82,  524,  285,   93,
  279,  292,  293,  536,   80,   89,   80,  201,  204,   80,
  535,  191,   86,  571,  513,   64,  679,  854,   72,  856,
   93,  572,  803,   86,  734,  735,  528,  702,   85,  704,
  675,  511,  709,  580,  706,   62,  505,   81,  605,  582,
   81,  589,  284,  285,  285,  225,  524,   85,  593,  428,
  361,  362,  225,  684,  524,  524,   80,   81,  524,  882,
  883,  731,   86,  420,   85,  887,   85,   86,  594,   85,
  601,  421,  422,  610,  618,  276,   93,  623,  628,  739,
  225,  632,  666,  668,  225,  225,  379,  284,  284,  524,
  429,  430,  431,  905,  524,  758,  524,  843,  643,  762,
  764,  300,  301,  302,  303,  304,  557,  524,  559,  663,
  558,  912,   86,  428,  914,  653,   85,  743,  654,  524,
   81,  769,  657,  524,  524,  919,   80,  658,  672,  673,
  678,  681,  692,   86,  235,  277,  477,  236,   80,   80,
  685,  688,  690,  693,  225,  699,  811,  810,   89,  788,
  776,  513,  513,  513,  429,  430,  456,  513,  513,  703,
  513,  801,  802,  708,   70,  307,  220,  477,  771,  110,
  477,  737,  867,  722,   65,  821,  821,  486,  728,  307,
  809,   80,   80,   80,  557,  477,   85,  718,  558,   86,
  863,   93,  719,   68,  320,  321,  524,   80,   85,   85,
  814,  524,  720,   66,  828,  829,  830,  730,  486,  733,
   93,  486,  821,  736,  817,  738,   80,  745,  284,  225,
  772,  773,  524,  524,  777,   80,  486,   93,  524,   93,
  789,   90,   93,  785,  797,  822,  822,  740,  741,  842,
  742,   85,   85,   85,  799,   42,   43,  800,  847,  524,
  524,  524,  812,  816,  276,  826,  524,   85,  504,   80,
  827,   80,   79,  379,  508,  835,  546,  837,   80,  110,
  110,  844,  822,  885,  428,   81,   85,  845,  848,   93,
  853,  849,  871,  855,  858,   85,  859,  821,   84,  285,
  860,  875,  821,  821,  864,  587,  666,  668,  279,  868,
  870,  869,  511,   80,   86,  872,  110,  897,   80,  508,
  508,  524,  524,  893,  277,  429,  430,  457,  904,   85,
  821,   85,   84,   86,  454,  898,  901,  908,   85,  909,
   90,  902,  916,  284,  524,  821,   84,   84,  379,  911,
   86,  508,   86,  821,  524,   86,  515,  822,  110,   93,
  524,  515,  822,  822,  517,  411,  517,  413,  417,  258,
  513,   93,   93,   85,   90,  515,  285,  428,   85,  454,
  454,   77,  550,  286,  551,  552,  553,  554,   90,   90,
  822,  110,  519,  332,  327,  519,  110,  110,  330,  328,
  360,  329,   86,  331,  524,  822,  222,  222,  824,  824,
  222,  350,   84,  822,   93,   93,   93,  359,  429,  430,
  461,  285,  285,  297,  110,  555,  556,  351,  286,  286,
   93,  356,  355,   85,  116,  251,  253,  910,  189,  110,
  834,  222,  222,  515,  900,  824,  682,  110,  782,   93,
  294,  296,  337,  393,   90,  422,  784,  299,   93,    0,
  550,    0,  551,  552,  553,  554,    0,   85,  332,  286,
    0,    0,   86,  330,  328,   87,  329,    0,  331,    0,
    0,   85,   85,    0,   86,   86,    0,  279,    0,    0,
  175,    0,   93,    0,   93,    0,  422,  422,  422,    0,
  175,   93,    0,  555,  556,  504,    0,   87,  508,   87,
    0,  508,    0,    0,  286,  286,    0,    0,    0,    0,
  824,  823,  825,   87,   87,  824,  824,   86,   86,   86,
    0,  175,    0,    0,  175,    0,   93,  412,    0,    0,
    0,   93,    0,   86,    0,    0,    0,   85,  175,  175,
  185,    0,    0,  824,  163,   84,  508,  508,  840,    0,
  185,    0,   86,    0,    0,  508,    0,    0,  824,  504,
    0,   86,    0,   78,    0,  504,  824,    0,  412,  412,
  412,    0,  163,  175,  163,    0,  163,    0,  508,   87,
  449,  185,    0,    0,  185,    0,    0,   90,    0,    0,
    0,    0,    0,  163,  156,   86,    0,   86,  185,  185,
  222,  222,  294,  185,   86,  175,   85,    0,    0,    0,
  504,  504,    0,    0,    0,  454,    0,    0,  222,  166,
  222,  222,  156,  888,  156,    0,  156,    0,  895,  896,
  448,    0,    0,  185,    0,    0,    0,    0,    0,   86,
    0,    0,  504,  156,   86,    0,  440,  166,    0,  166,
    0,  166,    0,    0,    0,  450,  907,  285,    0,    0,
    0,    0,    0,  505,  286,  185,  307,   80,  166,  505,
    0,  915,   82,   82,    0,    0,    0,    0,    0,  918,
   85,  320,  321,    0,    0,    0,    0,    0,  477,  478,
  479,  480,  481,  482,  483,  484,  485,  486,  487,  488,
  489,  490,  491,    0,  493,  494,  495,  496,  497,  498,
  499,  500,  501,  502,  505,  505,    0,    0,   82,    0,
    0,  222,   87,    0,  518,    0,    0,  522,  525,  222,
    0,    0,    0,    0,    0,  508,    0,    0,    0,    0,
    0,  307,  175,  175,  175,    0,  505,  175,  175,  175,
  286,  175,    0,    0,    0,    0,  320,  321,    0,    6,
    0,  175,  175,    0,    0,    0,    0,   95,   95,    6,
  175,  175,    0,  175,  175,  175,  175,   82,    0,  222,
   95,   95,    0,  222,   95,    0,  518,  222,    0,  522,
  609,   95,  508,  612,    0,    0,    0,    0,   86,  504,
    6,    0,  185,  185,  185,    0,  626,  185,  185,  185,
    0,  185,    0,   95,   95,   95,  631,    0,    6,    0,
  175,  185,  185,  222,   95,  151,  175,  222,    0,  222,
  185,  185,    0,  185,  185,  185,  185,  185,  163,    0,
  163,  163,  163,  163,    0,    0,    0,    0,  644,  645,
    0,    0,    0,  151,    0,  151,  504,  151,    0,    0,
    0,  451,    0,    0,    0,    0,    0,    0,    0,    0,
   95,  449,   95,  646,  151,    0,   82,    0,    0,    0,
  185,  163,  163,    0,    6,    0,  185,  514,  156,    0,
  156,  156,  156,  156,    0,    0,    0,    0,  664,    0,
    0,    0,   88,  505,  267,  222,    0,    0,    0,    0,
    0,    0,    0,  166,  267,  166,  166,  166,  166,   82,
    0,  448,    0,    0,    0,    0,    0,    0,  514,    0,
   82,  156,  156,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  267,  450,    0,  266,    0,
  270,  274,    0,  222,    0,    0,  166,  166,    0,    0,
  505,  222,  267,  267,    0,    0,    0,    0,    0,   82,
    0,   95,   95,   95,   82,    0,    0,    0,   95,    0,
   95,   95,  664,  222,    0,    0,    0,  222,    0,    0,
    0,    0,  222,   83,    0,    0,   83,    0,  277,  222,
    8,    0,   95,    0,   95,   95,    0,    0,    0,    0,
    8,    0,  514,   83,   95,    0,  746,    0,    0,   82,
    0,    6,    6,    6,    0,   95,    0,    6,    6,  267,
    6,    0,    0,    0,  757,  759,    0,    0,  761,  763,
   82,    8,    0,  277,  277,    0,    0,    0,    0,    0,
    0,    0,  766,  222,    0,    0,    0,    0,    0,    8,
    0,  134,  222,    0,   95,   95,   95,   95,   95,   95,
   95,   95,    0,    0,    0,    0,   83,    0,    0,    0,
    0,  504,   79,    0,    0,   79,  222,  508,    0,  134,
    0,  134,    0,  134,    0,    0,   82,  453,    0,  757,
  761,  763,   79,  222,    0,   95,    0,  222,   95,  222,
  134,   95,   95,   95,   95,    0,    0,    0,    0,  151,
    0,  151,  151,  151,  151,    8,  404,    0,  818,    0,
  404,    0,  508,  508,    0,   95,   95,    0,    0,    0,
  419,    0,    0,    0,    0,  396,  397,    0,  222,  514,
  514,  514,  451,    0,    0,  514,  514,    0,  514,    0,
    0,    0,  151,  151,  508,   79,  267,  267,  267,    0,
    0,  267,  267,  267,    0,  267,    0,   95,  222,    0,
    0,    0,    0,    0,    0,    0,  851,    0,    0,  222,
    0,   95,    0,    0,  222,    0,  135,  267,  267,  267,
  267,  267,    0,   76,    0,    0,   76,    0,  454,    0,
    0,   82,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   76,  135,    0,  135,  222,  135,    0,
   82,   83,  452,    0,  222,    0,  222,    0,    0,    0,
  270,    0,    0,    0,  267,  135,    0,   82,    0,   82,
    0,   97,   82,  454,  454,    0,    0,    0,    0,    0,
    0,    0,    8,    8,    8,  222,    0,  222,    8,    8,
    0,    8,    0,  527,  531,    0,    0,    0,  574,   97,
    0,   97,   95,   97,    0,  222,   76,  443,    0,  277,
    0,    0,    0,    0,  222,    0,  404,    0,    0,   82,
   97,    0,    0,    0,    0,   95,   95,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  404,    0,    0,    0,
   79,  508,    0,    0,  596,   95,    0,    0,  603,    0,
    0,    0,  607,    0,   82,    0,    0,   82,  613,  285,
  332,  327,   95,    0,   95,  330,  328,   95,  329,    0,
  331,    0,    0,    0,   82,  134,    0,  134,  134,  134,
  134,    0,    0,  324,    0,  323,    0,    0,  636,   82,
    0,    0,  607,    0,  636,    0,    0,    0,  508,    0,
   98,   82,   82,   95,  285,  285,    0,    0,  453,    0,
   95,    0,    0,    0,   95,    0,    0,  326,  134,  134,
    0,    0,    0,  650,  650,  650,    0,    0,   98,    0,
   98,    0,   98,    0,  662,    0,  446,   82,   95,    0,
  662,  662,   95,   95,   82,   82,   82,  325,    0,   98,
    0,    0,    0,    0,   77,    0,    0,   77,    0,  286,
   82,   76,    0,    0,    0,    0,  662,    0,    0,    0,
  670,    0,    0,    0,   77,    0,    0,    0,    0,   82,
    0,    0,  687,    0,   95,  687,    0,  687,   82,    0,
    0,    0,  698,  701,    0,    0,   95,   95,   86,    0,
    0,    0,   95,    0,  286,  286,    0,    0,    0,    0,
  135,    0,  135,  135,  135,  135,    0,    0,  695,  454,
    0,    0,   82,    0,   82,  404,  527,    0,  504,    0,
    0,   82,   86,    0,  504,    0,  404,   77,    0,   95,
   95,   95,    0,  452,    0,    0,   86,   86,  721,    0,
    0,    0,  724,  135,  135,   95,    0,  725,    0,    0,
    0,    0,    0,    0,  603,   97,   82,   97,   97,   97,
   97,   82,    0,    0,   95,    0,    0,   95,    0,  504,
  504,    0,    0,   95,    0,    0,    0,    0,    0,    0,
    0,    0,   82,    0,  650,    0,    0,    0,  443,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   97,   97,
    0,  504,   86,    0,    0,    0,  787,   95,  768,   95,
  791,  265,  701,    0,  701,    0,   95,  775,    0,    0,
    0,  265,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  404,    0,  307,  404,  404,    0,    0,  312,  313,
  285,  796,    0,    0,  662,  662,    0,    0,  320,  321,
    0,   95,  265,    0,    0,  265,   95,    0,  607,  504,
   78,    0,    0,   78,  607,  504,    0,    0,    0,  265,
  265,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   78,    0,   77,    0,   98,    0,   98,   98,   98,   98,
   88,  687,  687,  687,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  831,  265,    0,  202,    0,    0,    0,
  504,  504,    0,    0,    0,    0,  202,  446,    0,  404,
  505,  404,    0,    0,   88,    0,  505,   98,   98,    0,
    0,    0,    0,  636,    0,    0,  265,    0,   88,   88,
  286,    0,  504,   78,  852,   86,    0,  202,    0,  857,
  202,    0,    0,    0,    0,    0,    0,  701,  504,    0,
    0,  404,  404,    0,  202,  202,  202,  404,  202,  202,
    0,  505,  505,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  877,    0,  687,  574,    0,    0,    0,  603,
    0,  607,  443,    0,    0,  404,    0,    0,    0,  202,
    0,    0,  443,  505,   88,    0,    0,    0,    0,    0,
    0,  701,    0,  404,    0,  504,  404,    0,    0,    0,
  903,    0,  906,    0,    0,  501,    0,  404,    0,  443,
  443,  202,  501,  443,  443,  443,  443,  443,  443,  443,
  607,    0,    0,    0,    0,    0,    0,    0,    0,  917,
  443,  443,  443,  443,  443,  443,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  265,  265,  265,    0,    0,  265,  265,
  265,    0,  265,  443,  443,  443,  443,    0,    0,    0,
  446,    0,  265,  265,    0,    0,    0,    0,   78,  504,
  446,  265,  265,    0,  265,  265,  265,  265,  265,    0,
    0,    0,    0,    0,    0,  501,  443,  443,  501,    0,
    0,    0,    0,  502,    0,    0,    0,  446,  446,    0,
  502,  446,  446,  446,  446,  446,  446,  446,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   88,  446,  446,
  446,  446,  446,  446,    0,    0,  504,  265,    0,    0,
  505,    0,    0,    0,    0,    0,    0,    0,  202,  202,
  202,    0,    0,  202,  202,  202,    0,  202,    0,    0,
    0,  446,  446,  446,  446,    0,    0,  202,  202,    0,
    0,    0,    0,    0,    0,    0,  202,  202,    0,  202,
  202,  202,  202,  202,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  502,  446,  446,  502,  505,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  202,  202,  202,
  202,  202,  202,  202,  202,  202,  202,  202,  202,    0,
    0,    0,    0,    0,    0,    0,  202,    0,    0,  501,
  501,  501,  202,  501,  443,  443,  443,  501,  501,  443,
  443,  443,  501,  443,  501,  501,  501,  501,  501,  501,
  501,  443,  501,  443,  443,  501,  501,  501,  501,  501,
  501,  501,  443,  443,    0,  443,  443,  443,  443,  443,
    0,  501,    0,    0,  501,  501,  501,  501,  501,  501,
  501,  501,  501,  501,  501,    0,  501,  501,    0,  501,
  501,  501,  443,  443,  443,  443,  443,  443,  443,  443,
  443,  443,  443,  443,  443,    0,    0,  443,  443,  443,
  501,  443,  443,  501,  501,    0,  501,  501,  443,  501,
  501,  501,  501,  501,  501,  501,    0,  502,  502,  502,
  501,  502,  446,  446,  446,  502,  502,  446,  446,  446,
  502,  446,  502,  502,  502,  502,  502,  502,  502,  446,
  502,  446,  446,  502,  502,  502,  502,  502,  502,  502,
  446,  446,    0,  446,  446,  446,  446,  446,    0,  502,
    0,    0,  502,  502,  502,  502,  502,  502,  502,  502,
  502,  502,  502,    0,  502,  502,    0,  502,  502,  502,
  446,  446,  446,  446,  446,  446,  446,  446,  446,  446,
  446,  446,  446,  508,    0,  446,  446,  446,  502,  446,
  446,  502,  502,  508,  502,  502,  446,  502,  502,  502,
  502,  502,  502,  502,    0,    0,    0,    0,  502,    0,
    0,    0,    0,    0,    0,    0,  504,    0,    0,    0,
  508,  508,    0,  504,  508,  508,  508,  508,  508,  508,
  508,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  508,  508,  508,   87,  508,  508,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  508,  508,  508,  508,    0,    0,
    0,  504,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  504,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  508,  508,  508,  504,
    0,    0,    0,    0,  504,    0,    0,    0,  504,  504,
    0,  504,  504,  504,  504,  504,  504,  504,  504,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  504,
  504,  504,   86,  504,  504,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  504,  504,  504,  504,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  504,  504,  504,  504,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  504,  504,  504,    0,  504,  508,  508,  508,  504,  504,
  508,  508,  508,  504,  508,  504,  504,  504,  504,  504,
  504,  504,    0,  508,  508,  508,  504,  504,  504,  504,
  504,  504,  504,  508,  508,    0,  508,  508,  508,  508,
  508,    0,  504,    0,    0,  504,  504,  504,  504,  504,
  504,  504,  504,  504,  504,  504,    0,  504,  504,    0,
  504,  504,  504,  508,  508,  508,  508,  508,  508,  508,
  508,  508,  508,  508,  508,  508,    0,    0,  508,  508,
  508,  504,    0,  508,  504,  504,    0,  504,  504,  508,
  504,  504,  504,  504,  504,  504,  504,    0,  504,  504,
  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,
  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,
    0,  504,  504,  504,  504,  504,  504,  504,  504,  504,
  504,  504,  504,    0,  504,  504,  504,  504,  504,    0,
  504,    0,    0,  504,  504,  504,  504,  504,  504,  504,
  504,  504,  504,  504,    0,  504,  504,    0,  504,  504,
  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,
  504,  504,  504,  504,  505,    0,  504,  504,  504,  504,
    0,  504,  504,  504,  505,  504,  504,  504,  504,  504,
  504,  504,  504,  504,  504,    0,    0,    0,    0,  504,
    0,    0,    0,    0,    0,    0,    0,  505,    0,    0,
    0,  505,  505,    0,  505,  505,  505,  505,  505,  505,
  505,  505,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  505,  505,  505,   88,  505,  505,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  505,  505,  505,  505,    0,
    0,    0,  278,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  278,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  505,  505,  505,
  505,    0,    0,    0,    0,  503,    0,    0,    0,  278,
  278,    0,  503,  278,  278,  278,  278,  278,  278,  278,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  278,  278,  278,    0,  278,  278,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  278,  278,  278,  278,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  503,  278,  278,  503,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  505,  505,  505,    0,  505,  505,  505,  505,  505,
  505,  505,  505,  505,  505,  505,  505,  505,  505,  505,
  505,  505,  505,    0,  505,  505,  505,  505,  505,  505,
  505,  505,  505,  505,  505,  505,    0,  505,  505,  505,
  505,  505,    0,  505,    0,    0,  505,  505,  505,  505,
  505,  505,  505,  505,  505,  505,  505,    0,  505,  505,
    0,  505,  505,  505,  505,  505,  505,  505,  505,  505,
  505,  505,  505,  505,  505,  505,  505,    0,    0,  505,
  505,  505,  505,    0,  505,  505,  505,    0,  505,  505,
  505,  505,  505,  505,  505,  505,  505,  505,    0,  503,
  503,  503,  505,  503,  278,  278,  278,  503,  503,  278,
  278,  278,  503,  278,  503,  503,  503,  503,  503,  503,
  503,    0,  503,  278,  278,  503,  503,  503,  503,  503,
  503,  503,  278,  278,    0,  278,  278,  278,  278,  278,
    0,  503,    0,    0,  503,  503,  503,  503,  503,  503,
  503,  503,  503,  503,  503,    0,  503,  503,    0,  503,
  503,  503,  278,  278,  278,  278,  278,  278,  278,  278,
  278,  278,  278,  278,  278,  509,    0,  278,  278,  278,
  503,    0,  278,  503,  503,  509,  503,  503,  278,  503,
  503,  503,  503,  503,  503,  503,    0,    0,    0,    0,
  503,    0,    0,    0,    0,    0,    0,    0,  506,    0,
    0,    0,  509,  509,    0,  506,  509,  509,  509,  509,
  509,  509,  509,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  509,  509,  509,    0,  509,  509,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  509,  509,  509,  509,
    0,    0,    0,  510,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  510,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  509,  509,
  509,  506,    0,    0,    0,    0,  507,    0,    0,    0,
  510,  510,    0,  507,  510,  510,  510,  510,  510,  510,
  510,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  510,  510,  510,    0,  510,  510,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  510,  510,  510,  510,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  510,  510,  510,  507,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  506,  506,  506,    0,  506,  509,  509,  509,
  506,  506,  509,  509,  509,  506,  509,  506,  506,  506,
  506,  506,  506,  506,    0,  509,  509,  509,  506,  506,
  506,  506,  506,  506,  506,  509,  509,    0,  509,  509,
  509,  509,  509,    0,  506,    0,    0,  506,  506,  506,
  506,  506,  506,  506,  506,  506,  506,  506,    0,  506,
  506,    0,  506,  506,  506,  509,  509,  509,  509,  509,
  509,  509,  509,  509,  509,  509,  509,  509,    0,    0,
  509,  509,  509,  506,    0,  509,  506,  506,    0,  506,
  506,  509,  506,  506,  506,  506,  506,  506,  506,    0,
  507,  507,  507,  506,  507,  510,  510,  510,  507,  507,
  510,  510,  510,  507,  510,  507,  507,  507,  507,  507,
  507,  507,    0,  510,  510,  510,  507,  507,  507,  507,
  507,  507,  507,  510,  510,    0,  510,  510,  510,  510,
  510,    0,  507,    0,    0,  507,  507,  507,  507,  507,
  507,  507,  507,  507,  507,  507,    0,  507,  507,    0,
  507,  507,  507,  510,  510,  510,  510,  510,  510,  510,
  510,  510,  510,  510,  510,  510,  379,    0,  510,  510,
  510,  507,    0,  510,  507,  507,  379,  507,  507,  510,
  507,  507,  507,  507,  507,  507,  507,    0,    0,    0,
    0,  507,    0,    0,    0,    0,    0,    0,    0,  255,
    0,    0,    0,  379,  379,    0,    0,  379,  379,  379,
  379,  379,  379,  379,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  379,  379,  379,    0,  379,  379,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  379,  379,  379,
  379,    0,    0,    0,  524,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  524,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  379,
  379,  379,  255,    0,    0,    0,    0,  255,    0,    0,
    0,  524,  524,    0,    0,  524,  524,  524,  524,  524,
  524,  524,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  524,  524,  524,    0,  524,  524,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  524,  524,  524,  524,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  524,  524,  524,
  255,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  255,  255,  255,    0,  255,  379,  379,
  379,  255,  255,  379,  379,  379,  255,  379,  255,  255,
  255,  255,  255,  255,  255,    0,  379,  379,  379,  255,
  255,  255,  255,  255,  255,  255,  379,  379,    0,  379,
  379,  379,  379,  379,    0,  255,    0,    0,  255,  255,
  255,  255,  255,  255,  255,  255,  255,  255,  255,    0,
  255,  255,    0,  255,  255,  255,  379,  379,  379,  379,
  379,  379,  379,  379,  379,  379,  379,  379,  379,    0,
    0,  379,  379,  379,  255,    0,  379,  255,  255,    0,
  255,  255,  379,  255,  255,  255,  255,  255,  255,  255,
    0,  255,  255,  255,  255,  255,  524,  524,  524,  255,
  255,  524,  524,  524,  255,  524,  255,  255,  255,  255,
  255,  255,  255,    0,  524,  524,  524,  255,  255,  255,
  255,  255,  255,  255,  524,  524,    0,  524,  524,  524,
  524,  524,    0,  255,    0,    0,  255,  255,  255,  255,
  255,  255,  255,  255,  255,  255,  255,    0,  255,  255,
    0,  255,  255,  255,  524,  524,  524,  524,  524,  524,
  524,  524,  524,  524,  524,  524,  524,  284,    0,  524,
  524,  524,  255,    0,  524,  255,  255,  284,  255,  255,
  524,  255,  255,  255,  255,  255,  255,  255,    0,    0,
    0,    0,  255,    0,    0,    0,    0,    0,    0,    0,
  505,    0,    0,    0,  284,  284,    0,  505,  284,  284,
  284,  284,  284,  284,  284,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  284,  284,  284,   89,  284,
  284,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  284,  284,
  284,  284,    0,    0,    0,  292,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  292,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  284,  284,  505,    0,    0,    0,    0,  255,    0,
    0,    0,  292,  292,    0,    0,  292,  292,  292,  292,
  292,  292,  292,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  292,  292,  292,    0,  292,  292,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  292,  292,  292,  292,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  292,
  292,  255,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  505,  505,  505,    0,  505,  284,
  284,  284,  505,  505,  284,  284,  284,  505,  284,  505,
  505,  505,  505,  505,  505,  505,    0,    0,  284,  284,
  505,  505,  505,  505,  505,  505,  505,  284,  284,    0,
  284,  284,  284,  284,  284,    0,  505,    0,    0,  505,
  505,  505,  505,  505,  505,  505,  505,  505,  505,  505,
    0,  505,  505,    0,  505,  505,  505,  284,  284,  284,
  284,  284,  284,  284,  284,  284,  284,  284,  284,  284,
    0,    0,  284,  284,  284,  505,    0,  284,  505,  505,
    0,  505,  505,  284,  505,  505,  505,  505,  505,  505,
  505,    0,  255,  255,  255,  505,  255,  292,  292,  292,
  255,  255,  292,  292,  292,  255,  292,  255,  255,  255,
  255,  255,  255,  255,    0,    0,  292,  292,  255,  255,
  255,  255,  255,  255,  255,  292,  292,    0,  292,  292,
  292,  292,  292,    0,  255,    0,    0,  255,  255,  255,
  255,  255,  255,  255,  255,  255,  255,  255,    0,  255,
  255,    0,  255,  255,  255,  292,  292,  292,  292,  292,
  292,  292,  292,  292,  292,  292,  292,  292,  508,    0,
  292,  292,  292,  255,    0,  292,  255,  255,  508,  255,
  255,  292,  255,  255,  255,  255,  255,  255,  255,    0,
    0,    0,    0,  255,    0,    0,    0,    0,    0,    0,
    0,  504,    0,    0,    0,  508,  508,    0,  504,  508,
  508,  508,   79,  508,  508,  508,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  508,  508,   87,
  508,  508,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  508,
  508,    0,  508,    0,    0,    0,  504,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  504,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  508,  508,  508,  504,    0,    0,    0,    0,  504,
    0,    0,    0,  504,  504,    0,  504,  504,  504,  504,
   78,  504,  504,  504,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  504,  504,   86,  504,  504,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  504,  504,   32,
  504,    0,    0,    0,    0,    0,    0,    0,    0,   32,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  504,
  504,  504,  504,    0,    0,    0,    0,    0,    0,    0,
   32,    0,    0,  265,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   32,    0,
    0,    0,    0,    0,    0,  504,  504,  504,    0,  504,
  508,  508,  508,  504,  504,    0,  508,  508,  504,  508,
  504,  504,  504,  504,  504,  504,  504,    0,  508,    0,
    0,  504,  504,  504,  504,  504,  504,  504,  508,  508,
    0,  508,  508,  508,  508,  508,    0,  504,    0,    0,
  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,
  504,    0,  504,  504,   32,  504,  504,  504,  508,  508,
  508,  508,  508,  508,  508,  508,  508,  508,  508,  508,
  508,    0,    0,  508,  508,  508,  504,    0,    0,  504,
  504,    0,  504,  504,    0,  504,  504,  504,  504,  504,
  504,  504,    0,  504,  504,  504,  504,  504,  504,  504,
  504,  504,  504,    0,  504,  504,  504,  504,  504,  504,
  504,  504,  504,  504,  504,    0,  504,    0,    0,  504,
  504,  504,  504,  504,  504,  504,  504,  504,    0,  504,
  504,  504,  504,  504,    0,  504,    0,    0,  504,  504,
  504,  504,  504,  504,  504,  504,  504,  504,  504,    0,
  504,  504,    0,  504,  504,  504,  504,  504,  504,  504,
  504,  504,  504,  504,  504,  504,  504,  504,  504,  505,
    0,  504,  504,  504,  504,    0,    0,  504,  504,  505,
  504,  504,    0,  504,  504,  504,  504,  504,  504,  504,
    0,   32,   32,   32,  504,    0,    0,   32,   32,    0,
   32,    0,  505,    0,    0,    0,  505,  505,    0,  505,
  505,  505,  505,   80,  505,  505,  505,    0,    0,    0,
    0,    0,   32,   32,   32,   32,   32,    0,  505,  505,
   88,  505,  505,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  505,  505,    0,  505,    0,    0,    0,  284,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  284,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  505,  505,  505,  505,    0,    0,    0,    0,
  505,    0,    0,    0,  284,  284,    0,  505,  284,  284,
  284,   81,  284,  284,  284,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  284,  284,   89,  284,
  284,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  284,  284,
    7,  284,    0,    0,    0,    0,    0,    0,    0,    0,
    7,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  284,  284,  505,    0,    0,    0,    0,    0,    0,
    0,    7,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    7,
    0,    0,    0,  820,    0,    0,  505,  505,  505,    0,
  505,  505,  505,  505,  505,  505,    0,  505,  505,  505,
  505,  505,  505,  505,  505,  505,  505,  505,    0,  505,
    0,    0,  505,  505,  505,  505,  505,  505,  505,  505,
  505,    0,  505,  505,  505,  505,  505,    0,  505,    0,
    0,  505,  505,  505,  505,  505,  505,  505,  505,  505,
  505,  505,    0,  505,  505,    7,  505,  505,  505,  505,
  505,  505,  505,  505,  505,  505,  505,  505,  505,  505,
  505,  505,    0,    0,  505,  505,  505,  505,    0,    0,
  505,  505,    0,  505,  505,    0,  505,  505,  505,  505,
  505,  505,  505,    0,  505,  505,  505,  505,  505,  284,
  284,  284,  505,  505,    0,  284,  284,  505,  284,  505,
  505,  505,  505,  505,  505,  505,    0,    0,    0,    0,
  505,  505,  505,  505,  505,  505,  505,  284,  284,    0,
  284,  284,  284,  284,  284,    0,  505,    0,    0,  505,
  505,  505,  505,  505,  505,  505,  505,  505,  505,  505,
    0,  505,  505,    0,  505,  505,  505,  284,  284,  284,
  284,  284,  284,  284,  284,  284,  284,  284,  284,  284,
  524,    0,  284,  284,  284,  505,    0,    0,  505,  505,
  524,  505,  505,    0,  505,  505,  505,  505,  505,  505,
  505,    0,    7,    7,    7,  505,    0,    0,    7,    7,
    0,    7,    0,  255,    0,    0,    4,    5,    6,    0,
    8,  524,    0,    0,    9,   10,  524,    0,    0,   11,
    0,   12,   13,   14,   97,   98,   17,   18,  524,  524,
    0,    0,   99,  100,  101,   22,   23,   24,   25,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  102,    0,
    0,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,    0,    0,   46,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   92,    0,    0,    0,  819,    0,    0,
  107,   49,    0,   50,   51,  524,  255,    0,   53,   54,
   55,   56,   57,    0,    0,    0,   92,  108,    0,    0,
    0,    0,    0,  501,    0,    0,    0,    0,    0,  443,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  505,   80,   92,   92,   80,    0,  505,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   80,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  443,  443,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  505,  505,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  501,    0,    0,   92,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  505,   80,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  255,  255,  255,
    0,  255,  524,  524,  524,  255,  255,  524,  524,  524,
  255,  524,  255,  255,  255,  255,  255,  255,  255,    0,
    0,  524,    0,  255,  255,  255,  255,  255,  255,  255,
  524,  524,    0,  524,  524,  524,  524,  524,    0,  255,
    0,    0,  255,  255,  255,  255,  255,  255,  255,  255,
  255,  255,  255,    0,  255,  255,    0,  255,  255,  255,
    0,  894,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  524,  255,    0,
    0,  255,  255,    0,  255,  255,    0,  255,  255,  255,
  255,  255,  255,  255,    0,    0,    0,    0,  255,   92,
   92,   92,   92,   92,   92,   92,   92,   92,   92,   92,
    0,    0,   92,   92,    0,   92,   92,   92,   92,   92,
   92,   92,    0,  501,    0,    0,   92,   92,   92,   92,
   92,   92,   92,   93,    0,   92,    0,    0,    0,   80,
  505,   92,   92,   92,   92,   92,   92,   92,   92,   92,
   92,   92,   92,   92,   92,   92,   93,   92,   92,    0,
   92,   92,   92,  502,    0,    0,    0,    0,    0,  446,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  443,   92,   93,   93,   92,   92,    0,   92,   92,    0,
   92,    0,   92,   92,   92,   92,   92,  505,    0,    0,
    0,   92,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  446,  446,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   92,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  502,    0,    0,   93,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   92,
    0,    0,    0,    0,    0,    0,  504,    0,    0,    0,
    0,    0,  508,    0,    4,    5,    6,    0,    8,    0,
    0,    0,    9,   10,    0,   92,   92,   11,    0,   12,
   13,   14,   97,   98,   17,   18,    0,    0,    0,    0,
   99,  100,  101,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  102,  508,  508,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
    0,   42,   43,    0,    0,    0,   46,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  508,
    0,    0,   92,    0,    0,  819,    0,    0,  107,   49,
    0,   50,   51,    0,    0,    0,   53,   54,   55,   56,
   57,    0,    0,    0,    0,  108,    0,    0,    0,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
    0,    0,   93,   93,    0,   93,   93,   93,   93,   93,
   93,   93,    0,  502,    0,    0,   93,   93,   93,   93,
   93,   93,   93,    0,    0,   93,    0,    0,    0,    0,
    0,   93,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,   93,   93,   93,   93,    0,   93,   93,    0,
   93,   93,   93,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  446,   93,    0,    0,   93,   93,    0,   93,   93,    0,
   93,    0,   93,   93,   93,   93,   93,    0,    0,    0,
    0,   93,   92,   92,   92,   92,   92,   92,   92,   92,
   92,   92,   92,    0,    0,   92,   92,    0,   92,   92,
   92,   92,   92,   92,   92,    0,  508,    0,    0,   92,
   92,   92,   92,   92,   92,   92,   93,    0,   92,    0,
    0,    0,    0,    0,   92,   92,   92,   92,   92,   92,
   92,   92,   92,   92,   92,   92,   92,   92,   92,   93,
   92,   92,    0,   92,   92,   92,  505,    0,    0,    0,
    0,    0,  284,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  508,   92,   93,   93,   92,   92,    0,
   92,   92,    0,   92,    0,   92,   92,   92,   92,   92,
    0,    0,    0,    0,   92,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  284,  284,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   93,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   93,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   93,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  285,    0,    4,    5,    6,
    0,    8,    0,    0,    0,    9,   10,    0,   93,   93,
   11,    0,   12,   13,   14,   97,   98,   17,   18,    0,
    0,    0,    0,   99,  100,  101,   22,   23,   24,   25,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  102,
  285,  285,   31,   32,  103,   34,   35,   36,  104,   38,
   39,   40,   41,    0,   42,   43,    0,    0,    0,   46,
    0,    0,    0,    0,  335,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   93,    0,    0,  106,    0,
    0,  107,   49,    0,   50,   51,    0,  335,    0,   53,
   54,   55,   56,   57,    0,    0,    0,    0,  108,    0,
    0,    0,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,   93,  335,    0,   93,   93,    0,   93,   93,
   93,   93,   93,   93,   93,    0,    0,    0,    0,   93,
   93,   93,   93,   93,   93,   93,    0,    0,   93,    0,
    0,    0,    0,    0,   93,   93,   93,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,    0,
   93,   93,    0,   93,   93,   93,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  524,
  335,    0,    0,  284,   93,    0,    0,   93,   93,    0,
   93,   93,    0,   93,    0,   93,   93,   93,   93,   93,
    0,    0,  524,    0,   93,   93,   93,   93,   93,   93,
   93,   93,   93,   93,   93,   93,    0,    0,   93,   93,
    0,   93,   93,   93,   93,   93,   93,   93,  524,    0,
    0,    0,   93,   93,   93,   93,   93,   93,   93,    0,
    0,   93,    0,    0,    0,    0,    0,   93,   93,   93,
   93,   93,   93,   93,   93,   93,   93,   93,   93,   93,
   93,   93,    0,   93,   93,    0,   93,   93,   93,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  285,   93,    0,    0,
   93,   93,    0,   93,   93,  524,   93,    0,   93,   93,
   93,   93,   93,    0,    0,    0,    0,   93,    0,    0,
  335,  335,  335,  335,  335,  335,  335,  335,  335,  335,
  335,  524,  335,  335,  335,  335,  335,  335,  335,  335,
  335,  335,  335,    0,    0,    0,    0,  335,  335,  335,
  335,  335,  335,  335,  524,    0,  335,    0,    0,    0,
    0,    0,  335,  335,  335,  335,  335,  335,  335,  335,
  335,  335,  335,  335,  335,  335,  335,    0,  335,  335,
  524,  335,  335,  335,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  335,    0,    0,  335,  335,    0,  335,  335,
    0,  335,    0,  335,  335,  335,  335,  335,    0,    0,
    0,    0,  335,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  524,  524,  524,  524,  524,
  524,    0,    0,  524,  524,  524,  524,  524,    0,  524,
    0,  524,  524,  524,  524,  524,  524,  524,    0,   34,
    0,    0,  524,  524,  524,  524,  524,  524,  524,   34,
    0,  524,    0,    0,    0,    0,    0,  524,  524,  524,
  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,
  524,  524,    0,  524,  524,    0,  524,  524,  524,    0,
   34,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  524,   34,    0,
  524,  524,    0,  524,  524,    0,  524,    0,  524,  524,
  524,  524,  524,    0,    0,    0,    0,  524,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  332,  327,    0,    0,    0,  330,  328,    0,  329,    0,
  331,    0,    0,    0,    0,    0,    0,  524,  524,  524,
  524,  524,  524,  324,   34,  323,  524,  524,    0,    0,
    0,  524,    0,  524,  524,  524,  524,  524,  524,  524,
    0,    0,    0,    0,  524,  524,  524,  524,  524,  524,
  524,    0,    0,  524,    0,    0,    0,  326,    0,  524,
  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,
  524,  524,  524,  524,    0,  524,  524,    0,  524,  524,
  524,    0,    0,  379,    0,    0,    0,  325,    0,    0,
    0,    0,    0,  379,    0,    0,    0,    0,    0,  524,
    0,    0,  524,  524,    0,  524,  524,    0,  524,    0,
  524,  524,  524,  524,  524,    0,    0,    0,    0,  524,
  379,  379,    0,    0,  379,  379,  379,  379,  379,  379,
  379,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  379,  379,  379,    0,  379,  379,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   34,   34,   34,    0,    0,    0,   34,   34,    0,
   34,    0,  454,    0,  379,  379,  379,  379,    0,    0,
    0,    0,  454,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   34,   34,   34,   34,   34,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  379,  379,  379,  454,
  454,    0,    0,  454,  454,  454,  454,  454,  454,  454,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  454,  454,  454,   84,  454,  454,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  454,  454,  454,  454,  524,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  524,    0,    0,
    0,    0,    0,  307,  308,  309,  310,  311,  312,  313,
  314,  315,  316,  317,    0,    0,  454,  454,  320,  321,
    0,    0,    0,    0,  524,  524,    0,    0,  524,  524,
  524,  524,  524,  524,  524,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  524,  524,  524,    0,  524,
  524,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  379,  379,  379,    0,    0,
  379,  379,  379,    0,  379,    0,    0,    0,  524,  524,
  524,  524,    0,  379,  379,  379,    0,    0,    0,    0,
    0,    0,    0,  379,  379,    0,  379,  379,  379,  379,
  379,    0,    0,    0,    0,   14,    0,    0,    0,    0,
  524,  524,  524,    0,    0,   14,    0,    0,    0,    0,
    0,    0,    0,  379,  379,  379,  379,  379,  379,  379,
  379,  379,  379,  379,  379,  379,    0,    0,  379,  379,
  379,    0,    0,  379,    0,    0,   14,    0,    0,  379,
    0,    0,    0,    0,  454,  454,  454,    0,    0,  454,
  454,  454,    0,  454,   14,    0,    0,    0,    0,    0,
    0,    0,    0,  454,  454,    0,    0,    0,    0,    0,
    0,    0,  454,  454,    0,  454,  454,  454,  454,  454,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  454,  454,  454,  454,  454,  454,  454,  454,
  454,  454,  454,  454,  454,    0,    0,  454,  454,  454,
   14,  455,  454,    0,    0,    0,    0,    0,  454,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  524,
  524,  524,    0,    0,  524,  524,  524,    0,  524,    0,
    0,    0,    0,    0,    0,    0,    0,  524,  524,  524,
    0,    0,    0,    0,    0,    0,    0,  524,  524,    0,
  524,  524,  524,  524,  524,    0,    0,    0,    0,    0,
    0,    0,    0,  277,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  277,    0,    0,    0,  524,  524,  524,
  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,
    0,    0,  524,  524,  524,    0,    0,  524,    0,    0,
  277,  277,    0,  524,  277,  277,  277,  277,  277,  277,
  277,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  277,  277,  277,   91,  277,  277,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   14,   14,   14,
    0,    0,    0,   14,   14,    0,   14,    0,  285,    0,
    0,    0,    0,    0,  277,  277,  277,  277,  285,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   14,   14,
   14,   14,   14,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  285,  285,  277,  277,  285,
  285,  285,  285,  285,  285,  285,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  285,  285,  285,   90,
  285,  285,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  285,
  285,  285,  285,    0,  286,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  286,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  285,  285,    0,    0,    0,    0,    0,    0,
    0,  286,  286,    0,    0,  286,  286,  286,  286,  286,
  286,  286,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  286,  286,  286,   85,  286,  286,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,  277,  277,    0,    0,
  277,  277,  277,    0,  277,  286,  286,  286,  286,    0,
    0,    0,    0,    0,  277,  277,    0,    0,    0,    0,
    0,    0,    0,  277,  277,    0,  277,  277,  277,  277,
  277,    0,    0,    0,    0,  524,    0,    0,  286,  286,
    0,    0,    0,    0,    0,  524,    0,    0,    0,    0,
    0,    0,    0,  277,  277,  277,  277,  277,  277,  277,
  277,  277,  277,  277,  277,  277,    0,    0,  277,  277,
  277,    0,    0,  277,    0,    0,  524,    0,    0,  277,
  285,  285,  285,    0,    0,  285,  285,  285,    0,  285,
    0,    0,    0,  524,  524,    0,    0,    0,    0,  285,
  285,    0,    0,    0,    0,    0,    0,    0,  285,  285,
    0,  285,  285,  285,  285,  285,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  524,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  285,  285,
  285,  285,  285,  285,  285,  285,  285,  285,  285,  285,
  285,    0,    0,  285,  285,  285,   41,    0,  285,    0,
  524,    0,    0,    0,  285,    0,   41,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  286,  286,  286,    0,
    0,  286,  286,  286,    0,  286,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  286,  286,   41,    0,    0,
    0,    0,    0,    0,  286,  286,    0,  286,  286,  286,
  286,  286,    0,    0,   41,   41,    0,    0,    0,    0,
  404,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  404,    0,    0,    0,  286,  286,  286,  286,  286,  286,
  286,  286,  286,  286,  286,  286,  286,    0,    0,  286,
  286,  286,    0,    0,  286,    0,    0,  404,  404,    0,
  286,  404,  404,  404,  404,  404,  404,  404,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  404,  404,
  404,   41,  404,  404,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  524,  524,  524,
    0,    0,  524,  524,  524,  220,  524,    0,    0,    0,
    0,  404,  404,  404,  404,  220,  524,  524,    0,    0,
    0,    0,    0,    0,    0,  524,  524,    0,  524,  524,
  524,  524,  524,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  220,  220,  404,  404,  220,  220,  220,  220,
  220,  334,  220,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  220,  220,  220,    0,  220,  220,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  524,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  334,  334,  220,  220,
    0,  296,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  296,    0,    0,    0,    0,    0,    0,   41,   41,
   41,    0,    0,   41,   41,   41,    0,   41,    0,  220,
  220,    0,    0,    0,    0,    0,    0,   41,  296,  296,
    0,    0,  296,  296,  296,  296,  296,  296,  296,   41,
   41,   41,   41,   41,    0,    0,    0,    0,    0,  296,
  296,  296,    0,  296,  296,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  404,  404,  404,    0,    0,  404,  404,  404,
    0,  404,  296,  296,  296,  296,    0,    0,    0,    0,
    0,  404,  404,    0,    0,    0,    0,    0,    0,    0,
  404,  404,    0,  404,  404,  404,  404,  404,    0,    0,
    0,    0,   53,    0,    0,  296,  296,    0,    0,    0,
    0,    0,   53,    0,    0,    0,    0,    0,    0,    0,
  404,  404,  404,  404,  404,  404,  404,  404,  404,  404,
  404,  404,  404,    0,    0,  404,  404,  404,    0,    0,
  404,    0,    0,   53,    0,    0,  404,  220,  220,  220,
    0,    0,  220,  220,  220,    0,  220,    0,    0,    0,
   53,   53,    0,    0,    0,    0,  220,  220,    0,    0,
    0,    0,    0,    0,    0,  220,  220,    0,  220,  220,
  220,  220,  220,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   53,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  220,  220,  220,  220,  220,
  220,  220,  220,  220,  220,  220,  220,  220,    0,    0,
  220,  220,  334,    0,    0,  220,    0,   53,    0,    0,
    0,  220,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  296,  296,  296,    0,    0,  296,  296,
  296,    0,  296,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  296,  296,    0,    0,    0,    0,    0,    0,
    0,  296,  296,    0,  296,  296,  296,  296,  296,    0,
    0,    0,    0,    0,    0,    0,    0,  292,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  292,    0,    0,
    0,  296,  296,  296,  296,  296,  296,  296,  296,  296,
  296,  296,  296,  296,    0,    0,  296,  296,  296,    0,
    0,  296,    0,    0,  292,  292,    0,  296,  292,  292,
  292,  292,  292,  292,  292,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  292,  292,  292,    0,  292,
  292,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   53,   53,   53,    0,    0,   53,
   53,   53,  441,   53,    0,    0,    0,    0,  292,  292,
  292,  292,  441,   53,   53,    0,    0,    0,    0,    0,
    0,    0,   53,   53,    0,   53,   53,   53,   53,   53,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  441,
  441,  292,  292,  441,  441,  441,  441,  441,  441,  441,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  441,  441,  441,    0,  441,  441,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  441,  441,  441,  441,    0,  442,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  442,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  441,  441,    0,    0,
    0,    0,    0,    0,    0,  442,  442,    0,    0,  442,
  442,  442,  442,  442,  442,  442,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  442,  442,  442,    0,
  442,  442,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  292,
  292,  292,    0,    0,  292,  292,  292,    0,  292,  442,
  442,  442,  442,    0,    0,    0,    0,    0,  292,  292,
    0,    0,    0,    0,    0,    0,    0,  292,  292,    0,
  292,  292,  292,  292,  292,    0,    0,    0,  200,    0,
    0,    0,  442,  442,    0,    0,    0,    0,  200,    0,
    0,    0,    0,    0,    0,    0,    0,  292,  292,  292,
  292,  292,  292,  292,  292,  292,  292,  292,  292,  292,
    0,    0,  292,  292,  292,    0,    0,  292,    0,  200,
    0,    0,  200,  292,  441,  441,  441,    0,    0,  441,
  441,  441,    0,  441,    0,    0,  200,  200,    0,    0,
    0,  200,    0,  441,  441,    0,    0,    0,    0,    0,
    0,    0,  441,  441,    0,  441,  441,  441,  441,  441,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  200,  196,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  196,  441,  441,  441,  441,  441,  441,  441,
  441,  441,  441,  441,  441,    0,    0,  441,  441,  441,
    0,    0,  441,  200,    0,    0,    0,    0,  441,  196,
  196,    0,    0,  196,  196,  196,  196,  196,    0,  196,
  442,  442,  442,    0,    0,  442,  442,  442,    0,  442,
  196,  196,  196,    0,  196,  196,    0,    0,    0,  442,
  442,    0,    0,    0,    0,    0,    0,    0,  442,  442,
    0,  442,  442,  442,  442,  442,    0,    0,    0,    0,
    0,    0,    0,    0,  192,  196,  196,    0,    0,    0,
    0,    0,    0,    0,  192,    0,    0,    0,    0,  442,
  442,  442,  442,  442,  442,  442,  442,  442,  442,  442,
  442,    0,    0,  442,  442,  442,  196,  196,  442,    0,
    0,  192,  192,    0,  442,  192,  192,  192,  192,  192,
    0,  192,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  192,  192,  192,    0,  192,  192,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  200,  200,  200,    0,    0,  200,  200,  200,    0,  200,
    0,    0,    0,  189,    0,    0,    0,  192,  192,  200,
  200,    0,    0,  189,    0,    0,    0,    0,  200,  200,
    0,  200,  200,  200,  200,  200,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  192,  192,
  189,  189,    0,    0,  189,  189,  189,  189,  189,    0,
  189,    0,    0,    0,    0,  200,  200,    0,    0,  200,
  200,  189,  189,  189,    0,  189,  189,    0,  200,    0,
    0,    0,    0,    0,  200,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  196,  196,  196,    0,    0,  196,
  196,  196,    0,  196,    0,    0,  189,  189,    0,    0,
    0,    0,    0,  196,  196,    0,    0,    0,    0,    0,
    0,    0,  196,  196,    0,  196,  196,  196,  196,  196,
    0,    0,    0,    0,    0,    0,    0,  189,  189,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  196,  196,  196,  196,  196,  196,  196,
  196,  196,  196,  196,  196,    0,    0,  196,  196,    0,
    0,    0,  196,    0,    0,    0,    0,    0,  196,    0,
    0,    0,    0,    0,    0,    0,  192,  192,  192,    0,
    0,  192,  192,  192,    0,  192,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  192,  192,    0,    0,    0,
    0,    0,    0,    0,  192,  192,    0,  192,  192,  192,
  192,  192,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  192,  192,  192,  192,  192,
  192,  192,  192,  192,  192,  192,  192,    0,    0,  192,
  192,    0,    0,    0,  192,    0,    0,    0,    0,    0,
  192,    0,    0,    0,    0,  189,  189,  189,    0,    0,
  189,  189,  189,    0,  189,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  189,  189,    0,    0,    0,    0,
    0,    0,    0,  189,  189,    0,  189,  189,  189,  189,
  189,    0,    0,  190,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  190,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  189,  189,  189,  189,  189,  189,
  189,  189,  189,  189,  189,  189,    0,    0,  189,  189,
  190,  190,    0,  189,  190,  190,  190,  190,  190,  189,
  190,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  190,  190,  190,    0,  190,  190,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  191,    0,
    0,    0,    0,    0,    0,    0,  190,  190,  191,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  191,  191,  190,  190,  191,
  191,  191,  191,  191,    0,  191,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  191,  191,  191,    0,
  191,  191,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  191,  191,  193,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  193,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  191,  191,    0,    0,    0,    0,    0,    0,
  193,  193,    0,    0,  193,  193,  193,  193,  193,    0,
  193,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  193,  193,  193,    0,  193,  193,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  190,  190,  190,    0,    0,
  190,  190,  190,    0,  190,    0,  193,  193,    0,    0,
    0,    0,    0,    0,  190,  190,    0,    0,    0,    0,
    0,    0,    0,  190,  190,    0,  190,  190,  190,  190,
  190,  175,    0,    0,    0,    0,    0,  193,  193,    0,
    0,  175,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  190,  190,  190,  190,  190,  190,
  190,  190,  190,  190,  190,  190,    0,    0,  190,  190,
    0,    0,  175,  190,    0,  221,    0,    0,    0,  190,
  191,  191,  191,    0,    0,  191,  191,  191,    0,  191,
  175,    0,    0,    0,    0,    0,    0,    0,    0,  191,
  191,    0,    0,    0,    0,    0,    0,    0,  191,  191,
    0,  191,  191,  191,  191,  191,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  191,
  191,  191,  191,  191,  191,  191,  191,  191,  191,  191,
  191,    0,    0,  191,  191,    0,  175,    0,  191,    0,
    0,    0,    0,    0,  191,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  193,  193,  193,    0,    0,
  193,  193,  193,    0,  193,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  193,  193,    0,    0,    0,    0,
    0,    0,    0,  193,  193,    0,  193,  193,  193,  193,
  193,    0,    0,  194,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  194,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  193,  193,  193,  193,  193,  193,
  193,  193,  193,  193,  193,  193,    0,    0,  193,  193,
  194,  194,    0,  193,  194,  194,  194,  194,  194,  193,
  194,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  194,  194,  194,    0,  194,  194,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  175,  175,  175,    0,    0,  187,  175,
  175,    0,  175,    0,    0,    0,  194,  194,  187,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  175,  175,    0,  175,  175,  175,  175,    0,    0,
    0,    0,    0,    0,    0,    0,  187,  194,  194,  187,
    0,  187,  187,  187,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  187,  187,  187,    0,
  187,  187,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  187,  187,  188,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  188,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  187,  187,    0,    0,    0,    0,    0,    0,
    0,  188,    0,    0,  188,    0,  188,  188,  188,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  188,  188,  188,    0,  188,  188,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  194,  194,  194,    0,    0,
  194,  194,  194,    0,  194,    0,  188,  188,    0,    0,
    0,    0,    0,    0,  194,  194,    0,    0,    0,    0,
    0,    0,    0,  194,  194,    0,  194,  194,  194,  194,
  194,    0,    0,    0,    0,    0,    0,  188,  188,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  194,  194,  194,  194,  194,  194,
  194,  194,  194,  194,  194,  194,  212,    0,  194,  194,
    0,    0,    0,  194,    0,    0,  212,    0,    0,  194,
  187,  187,  187,    0,    0,  187,  187,  187,    0,  187,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  187,
  187,    0,    0,    0,  212,    0,    0,  212,  187,  187,
  212,  187,  187,  187,  187,  187,    0,    0,    0,    0,
    0,    0,    0,    0,  212,  212,  212,    0,  212,  212,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  187,
  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,
  187,    0,    0,  187,  187,    0,    0,    0,  187,  212,
  212,  213,    0,    0,  187,    0,    0,    0,    0,    0,
    0,  213,    0,    0,    0,  188,  188,  188,    0,    0,
  188,  188,  188,    0,  188,    0,    0,    0,    0,    0,
  212,  212,    0,    0,  188,  188,    0,    0,    0,  213,
    0,    0,  213,  188,  188,  213,  188,  188,  188,  188,
  188,    0,    0,    0,    0,    0,    0,    0,    0,  213,
  213,  213,    0,  213,  213,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  188,  188,  188,  188,  188,  188,
  188,  188,  188,  188,  188,  188,  199,    0,  188,  188,
    0,    0,    0,  188,  213,  213,  199,    0,    0,  188,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  199,  213,  213,  199,    0,    0,
  199,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  199,  199,  199,    0,  199,  199,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  212,  212,
  212,    0,    0,  212,  212,  212,    0,  212,    0,  199,
  199,    0,    0,    0,    0,    0,    0,  212,  212,    0,
    0,    0,    0,    0,    0,    0,  212,  212,    0,  212,
  212,  212,  212,  212,    0,    0,    0,    0,    0,    0,
  199,  199,    0,    0,    0,  197,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  197,    0,  212,  212,  212,
  212,  212,  212,  212,  212,  212,  212,  212,  212,    0,
    0,  212,  212,    0,    0,    0,  212,    0,    0,    0,
    0,    0,  212,    0,    0,    0,  197,    0,    0,  197,
    0,    0,    0,  213,  213,  213,    0,    0,  213,  213,
  213,    0,  213,  197,  197,  197,    0,  197,  197,    0,
    0,    0,  213,  213,    0,    0,    0,    0,    0,    0,
    0,  213,  213,    0,  213,  213,  213,  213,  213,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  197,  197,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  213,  213,  213,  213,  213,  213,  213,  213,
  213,  213,  213,  213,  198,    0,  213,  213,    0,  197,
  197,  213,    0,    0,  198,    0,    0,  213,  199,  199,
  199,    0,    0,  199,  199,  199,    0,  199,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  199,  199,    0,
    0,    0,    0,    0,    0,  198,  199,  199,  198,  199,
  199,  199,  199,  199,    0,    0,    0,    0,    0,    0,
    0,    0,  198,  198,  198,    0,  198,  198,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  199,  199,  199,
  199,  199,  199,  199,  199,  199,  199,  199,  199,    0,
    0,    0,    0,    0,    0,    0,  199,  198,  198,  204,
    0,    0,  199,    0,    0,    0,    0,    0,    0,  204,
    0,    0,    0,    0,  332,  327,    0,    0,    0,  330,
  328,    0,  329,    0,  331,    0,    0,    0,  198,  198,
    0,    0,    0,    0,    0,    0,    0,  324,    0,  323,
  204,    0,    0,  204,    0,    0,    0,  197,  197,  197,
    0,    0,  197,  197,  197,    0,  197,  204,  204,  204,
    0,  204,  204,    0,    0,    0,  197,  197,    0,    0,
    0,  326,    0,    0,    0,  197,  197,    0,  197,  197,
  197,  197,  197,    0,    0,  201,    0,    0,    0,    0,
    0,    0,  204,    0,    0,  201,    0,    0,    0,    0,
    0,  325,    0,    0,    0,    0,  197,  197,  197,  197,
  197,  197,  197,  197,  197,  197,  197,  197,    0,    0,
    0,    0,    0,    0,  204,  197,  201,    0,    0,  201,
    0,  197,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  201,  201,  201,    0,  201,  201,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  198,  198,  198,    0,
    0,  198,  198,  198,    0,  198,    0,    0,  201,  203,
    0,    0,    0,    0,    0,  198,  198,    0,    0,  203,
    0,    0,    0,    0,  198,  198,    0,  198,  198,  198,
  198,  198,    0,    0,    0,    0,    0,    0,    0,    0,
  201,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  203,    0,    0,  203,    0,  198,  198,  198,  198,  198,
  198,  198,  198,  198,  198,  198,  198,  203,  203,  203,
    0,  203,  203,    0,  198,    0,    0,    0,    0,    0,
  198,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  204,  204,  204,    0,    0,  204,  204,  204,    0,
  204,    0,  203,    0,    0,  205,    0,    0,    0,    0,
  204,  204,    0,    0,    0,  205,    0,    0,    0,  204,
  204,    0,  204,  204,  204,  204,  204,  307,  308,  309,
  310,  311,  312,  313,  203,    0,  316,  317,    0,    0,
    0,    0,  320,  321,    0,    0,  205,    0,    0,  205,
  204,  204,  204,  204,  204,  204,  204,  204,  204,  204,
  204,  204,    0,  205,  205,    0,    0,    0,  205,  204,
    0,    0,    0,    0,    0,  204,    0,  201,  201,  201,
    0,    0,  201,  201,  201,    0,  201,    0,    0,    0,
  206,    0,    0,    0,    0,    0,  201,  201,  205,    0,
  206,    0,    0,    0,    0,  201,  201,    0,  201,  201,
  201,  201,  201,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  205,  206,    0,    0,  206,    0,  201,  201,  201,  201,
  201,  201,  201,  201,  201,  201,  201,  201,  206,  206,
    0,    0,    0,  206,    0,  201,    0,    0,    0,    0,
    0,  201,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  203,  203,  203,    0,    0,  203,  203,  203,    0,
  203,    0,    0,  206,  207,    0,    0,    0,    0,    0,
  203,  203,    0,    0,  207,    0,    0,    0,    0,  203,
  203,    0,  203,  203,  203,  203,  203,    0,    0,    0,
    0,    0,    0,    0,    0,  206,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  207,    0,    0,  207,    0,
  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
  203,  203,  207,  207,    0,    0,    0,  207,    0,  203,
    0,    0,    0,    0,    0,  203,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  205,  205,  205,
    0,  214,  205,  205,  205,    0,  205,  207,    0,    0,
    0,  214,    0,    0,    0,    0,  205,  205,    0,    0,
    0,    0,    0,    0,    0,  205,  205,    0,  205,  205,
  205,  205,  205,    0,    0,    0,    0,    0,    0,  207,
    0,    0,  214,    0,    0,  214,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  214,
  214,    0,  205,  205,  214,    0,  205,  205,    0,    0,
    0,    0,    0,    0,    0,  205,    0,    0,    0,    0,
    0,  205,  206,  206,  206,    0,    0,  206,  206,  206,
    0,  206,    0,    0,  214,  208,    0,    0,    0,    0,
    0,  206,  206,    0,    0,  208,    0,    0,    0,    0,
  206,  206,    0,  206,  206,  206,  206,  206,    0,    0,
    0,    0,    0,    0,    0,    0,  214,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  208,    0,    0,  208,
    0,    0,    0,    0,    0,    0,    0,  206,  206,    0,
    0,  206,  206,  208,  208,    0,    0,    0,  208,    0,
  206,    0,    0,    0,    0,    0,  206,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  207,  207,  207,    0,
  209,  207,  207,  207,    0,  207,    0,    0,  208,    0,
  209,    0,    0,    0,    0,  207,  207,    0,    0,    0,
    0,    0,    0,    0,  207,  207,    0,  207,  207,  207,
  207,  207,    0,    0,    0,    0,    0,    0,    0,    0,
  208,  209,    0,    0,  209,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  209,  209,
    0,  207,  207,  209,    0,  207,  207,    0,    0,    0,
    0,    0,    0,    0,  207,    0,    0,    0,    0,    0,
  207,    0,    0,  214,  214,  214,    0,    0,  214,  214,
  214,    0,  214,  209,    0,    0,  215,    0,    0,    0,
    0,    0,  214,  214,    0,    0,  215,    0,    0,    0,
    0,  214,  214,    0,  214,  214,  214,  214,  214,    0,
    0,    0,    0,    0,    0,  209,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  215,    0,    0,
  215,    0,    0,    0,    0,    0,    0,    0,  214,  214,
    0,    0,  214,  214,  215,  215,    0,    0,    0,  215,
    0,  214,    0,    0,    0,    0,    0,  214,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  208,  208,  208,
    0,  186,  208,  208,  208,    0,  208,    0,    0,  215,
    0,  186,    0,    0,    0,    0,  208,  208,    0,    0,
    0,    0,    0,    0,    0,  208,  208,    0,  208,  208,
  208,  208,  208,    0,    0,    0,    0,    0,    0,    0,
    0,  215,  186,    0,    0,  186,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  186,
  186,    0,  208,  208,  186,    0,  208,  208,    0,    0,
    0,    0,    0,    0,    0,  208,    0,    0,    0,    0,
    0,  208,  209,  209,  209,    0,    0,  209,  209,  209,
    0,  209,    0,    0,  186,  221,    0,    0,    0,    0,
    0,  209,  209,    0,    0,  221,    0,    0,    0,    0,
  209,  209,    0,  209,  209,  209,  209,  209,    0,    0,
    0,    0,    0,    0,    0,    0,  186,    0,    0,    0,
    0,    0,  177,    0,    0,    0,  221,    0,    0,  221,
    0,    0,  177,    0,    0,    0,    0,  209,  209,    0,
    0,  209,  209,  221,  221,    0,    0,    0,    0,    0,
  209,    0,    0,    0,    0,    0,  209,    0,    0,    0,
    0,    0,    0,  177,    0,    0,  177,    0,  215,  215,
  215,    0,    0,  215,  215,  215,    0,  215,  221,    0,
  177,  177,  184,    0,    0,    0,    0,  215,  215,    0,
    0,    0,  184,    0,    0,    0,  215,  215,    0,  215,
  215,  215,  215,  215,    0,    0,    0,    0,    0,    0,
  221,    0,    0,    0,    0,  177,    0,    0,    0,    0,
  217,    0,    0,  184,    0,    0,  184,    0,    0,    0,
  217,    0,    0,    0,  215,    0,    0,  215,  215,    0,
  184,  184,    0,    0,    0,    0,  215,  177,    0,    0,
    0,    0,  215,  186,  186,  186,    0,    0,  186,  186,
  186,  217,  186,    0,  217,    0,    0,    0,    0,    0,
    0,    0,  186,  186,    0,  184,    0,    0,  217,  217,
  183,  186,  186,    0,  186,  186,  186,  186,  186,    0,
  183,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  184,    0,    0,
    0,    0,    0,  217,    0,    0,    0,    0,    0,    0,
    0,  183,    0,    0,  183,    0,    0,    0,    0,    0,
    0,  186,    0,    0,    0,    0,    0,  186,  183,  183,
    0,    0,    0,    0,    0,  217,    0,  221,  221,  221,
    0,    0,  221,  221,  221,    0,  221,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  221,  221,    0,    0,
    0,    0,    0,  183,    0,  221,  221,    0,  221,  221,
  221,  221,  221,    0,  177,  177,  177,    0,    0,  177,
  177,  177,    0,  177,    0,    0,    0,    0,    0,  178,
    0,    0,    0,  177,  177,  183,    0,    0,    0,  178,
    0,    0,  177,  177,    0,  177,  177,  177,  177,  177,
    0,    0,    0,    0,    0,  221,    0,    0,    0,    0,
    0,  221,    0,    0,    0,    0,    0,    0,    0,    0,
  178,    0,    0,  178,  184,  184,  184,    0,    0,  184,
  184,  184,    0,  184,    0,    0,    0,  178,  178,    0,
    0,    0,  177,  184,  184,    0,    0,    0,  177,    0,
    0,    0,  184,  184,    0,  184,  184,  184,  184,  184,
    0,    0,  217,  217,  217,    0,    0,  217,  217,  217,
    0,  217,  178,    0,  181,    0,    0,    0,    0,    0,
    0,  217,  217,    0,  181,    0,    0,    0,    0,    0,
  217,  217,    0,  217,  217,  217,  217,  217,    0,    0,
    0,    0,  184,    0,  178,    0,    0,    0,  184,    0,
    0,    0,    0,    0,    0,  181,    0,    0,  181,    0,
    0,    0,  183,  183,  183,    0,    0,  183,  183,  183,
    0,  183,  181,  181,  182,    0,    0,    0,    0,    0,
  217,  183,  183,    0,  182,    0,  217,    0,    0,    0,
  183,  183,    0,  183,  183,  183,  183,  183,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  181,    0,    0,
  179,    0,    0,    0,    0,  182,    0,    0,  182,    0,
  179,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  182,  182,    0,    0,    0,    0,    0,  181,
  183,    0,    0,    0,    0,    0,  183,    0,  180,    0,
    0,  179,    0,    0,  179,    0,    0,    0,  180,    0,
    0,    0,    0,    0,    0,    0,    0,  182,  179,  179,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  178,  178,  178,    0,    0,  178,  178,  178,  180,
  178,    0,  180,    0,    0,    0,    0,    0,    0,  182,
  178,  178,    0,  179,    0,    0,  180,  180,  176,  178,
  178,    0,  178,  178,  178,  178,  178,    0,  176,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  179,    0,    0,    0,    0,
    0,  180,    0,    0,    0,    0,    0,    0,    0,  176,
    0,    0,  176,    0,    0,    0,    0,    0,    0,  178,
    0,    0,    0,    0,    0,  178,  176,  176,    0,    0,
    0,    0,    0,  180,    0,    0,  181,  181,  181,    0,
    0,  181,  181,  181,    0,  181,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  181,  181,    0,    0,    0,
    0,  176,    0,  219,  181,  181,    0,  181,  181,  181,
  181,  181,    0,  219,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  176,    0,    0,  182,  182,  182,  266,
    0,  182,  182,  182,  219,  182,    0,  219,    0,  266,
    0,    0,    0,    0,  181,  182,  182,    0,    0,    0,
  181,  219,  219,    0,  182,  182,    0,  182,  182,  182,
  182,  182,  179,  179,  179,    0,    0,  179,  179,  179,
  266,  179,    0,  266,    0,    0,    0,    0,    0,    0,
    0,  179,  179,    0,    0,    0,  219,  266,  266,    0,
  179,  179,    0,  179,  179,  179,  179,  179,    0,    0,
  180,  180,  180,    0,  182,  180,  180,  180,    0,  180,
  182,    0,    0,    0,    0,    0,   57,    0,  219,  180,
  180,    0,  266,    0,    0,    0,   57,    0,  180,  180,
    0,  180,  180,  180,  180,  180,    0,    0,    0,    0,
  179,    0,    0,    0,    0,    0,  179,    0,    0,    0,
    0,    0,    0,    0,  266,    0,    0,   57,    0,    0,
  176,  176,  176,    0,    0,  176,  176,  176,    0,  176,
    0,    0,    0,    0,   57,   57,    0,  330,  180,  176,
  176,    0,    0,    0,  180,    0,    0,  330,  176,  176,
    0,  176,  176,  176,  176,  176,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   57,
    0,    0,    0,    0,  330,  330,    0,    0,  330,  330,
  330,  330,  330,  330,  330,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  330,  330,  330,  176,  330,
  330,   57,    0,    0,  176,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  219,  219,  219,    0,    0,
  219,  219,  219,    0,  219,    0,    0,    0,  330,  330,
    0,  330,    0,    0,  219,  219,    0,    0,    0,    0,
    0,    0,    0,  219,  219,    0,  219,  219,  219,  219,
  219,  266,  266,  266,    0,    0,  266,  266,  266,  331,
  266,  330,  330,    0,    0,    0,    0,    0,    0,  331,
  266,  266,    0,    0,    0,    0,    0,    0,    0,  266,
  266,    0,  266,  266,  266,  266,  266,    0,    0,    0,
    0,    0,    0,  219,    0,    0,  331,  331,    0,  219,
  331,  331,  331,  331,  331,  331,  331,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  331,  331,  331,
    0,  331,  331,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  266,    0,    0,   57,   57,
   57,    0,    0,   57,   57,   57,    0,   57,    0,    0,
  331,  331,    0,  331,    0,    0,    0,   57,   57,    0,
    0,  246,    0,    0,    0,    0,   57,   57,    0,   57,
   57,   57,   57,   57,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  331,  331,    0,    0,    0,  332,  327,
    0,    0,    0,  330,  328,    0,  329,    0,  331,  330,
  330,  330,    0,    0,  330,  330,  330,    0,  330,    0,
    0,  324,    0,  323,  322,    0,    0,    0,  330,    0,
    0,    0,    0,    0,    0,    0,    0,  330,  330,    0,
  330,  330,  330,  330,  330,    0,    0,    0,    0,    0,
    0,  289,    0,    0,    0,  326,    0,    0,    0,    0,
    0,  289,    0,    0,    0,    0,    0,  330,  330,  330,
  330,  330,  330,  330,  330,  330,  330,  330,  330,  330,
    0,    0,  330,  330,  330,  325,    0,  330,  289,  289,
    0,    0,  289,  289,  289,  289,  289,  289,  289,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  289,
  289,  289,    0,  289,  289,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  331,  331,  331,    0,    0,  331,  331,  331,    0,
  331,    0,  289,  289,    0,  289,    0,    0,    0,    0,
  331,    0,    0,    0,    0,    0,    0,    0,    0,  331,
  331,    0,  331,  331,  331,  331,  331,    0,    0,    0,
  454,    0,    0,    0,    0,  289,  289,    0,    0,    0,
  454,    0,    0,    0,    0,    0,    0,    0,    0,  331,
  331,  331,  331,  331,  331,  331,  331,  331,  331,  331,
  331,  331,    0,    0,  331,  331,  331,  454,  454,  331,
    0,  454,  454,  454,   76,  454,  454,  454,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  454,
  454,   84,  454,  454,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  454,  454,    0,  454,  277,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,    0,    0,    0,    0,
    0,  307,  308,  309,  310,  311,  312,  313,  314,  315,
  316,  317,  318,  319,  454,  454,  320,  321,    0,    0,
    0,    0,  277,  277,    0,    0,  277,  277,  277,   83,
  277,  277,  277,  289,  289,  289,    0,    0,  289,  289,
  289,    0,  289,    0,  277,  277,   91,  277,  277,    0,
    0,    0,  289,    0,    0,    0,    0,    0,    0,    0,
    0,  289,  289,    0,  289,  289,  289,  289,  289,    0,
    0,    0,    0,    0,    0,  285,  277,  277,    0,  277,
    0,    0,    0,    0,    0,  285,    0,    0,    0,    0,
    0,  289,  289,  289,  289,  289,  289,  289,  289,  289,
  289,  289,  289,  289,    0,    0,  289,  289,  289,  277,
  277,  289,  285,  285,    0,    0,  285,  285,  285,   82,
  285,  285,  285,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  285,  285,   90,  285,  285,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  454,  454,  454,    0,    0,    0,  454,  454,
    0,  454,    0,    0,    0,    0,  285,  285,    0,  285,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  454,  454,    0,  454,  454,  454,  454,  454,    0,    0,
    0,    0,   59,    0,    0,    0,    0,    0,    0,  285,
  285,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  454,  454,  454,  454,  454,  454,  454,  454,  454,  454,
  454,  454,  454,    0,    0,  454,  454,  454,    0,  455,
    0,    0,    0,    0,    0,    0,  332,  327,    0,    0,
    0,  330,  328,    0,  329,    0,  331,  277,  277,  277,
    0,    0,    0,  277,  277,    0,  277,    0,    0,  324,
    0,  323,  322,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  277,  277,    0,  277,  277,
  277,  277,  277,    0,    0,   60,    0,    0,    0,  286,
    0,    0,    0,  326,    0,    0,    0,    0,    0,  286,
    0,    0,    0,    0,    0,  277,  277,  277,  277,  277,
  277,  277,  277,  277,  277,  277,  277,  277,    0,    0,
  277,  277,  277,  325,    0,    0,  286,  286,    0,    0,
  286,  286,  286,   77,  286,  286,  286,  285,  285,  285,
    0,    0,    0,  285,  285,    0,  285,    0,  286,  286,
   85,  286,  286,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  285,  285,    0,  285,  285,
  285,  285,  285,    0,    0,    0,    0,    0,    0,    0,
  286,  286,    0,  286,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  285,  285,  285,  285,  285,
  285,  285,  285,  285,  285,  285,  285,  285,    0,   59,
  285,  285,  285,  286,  286,    3,    4,    5,    6,    7,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   15,   16,   17,   18,    0,    0,
    0,    0,   19,   20,   21,   22,   23,   24,   25,    0,
    0,   26,    0,    0,    0,    0,    0,   27,   28,   29,
   30,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,   44,   45,   46,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  671,   47,    0,    0,
   48,   49,   60,   50,   51,    0,   52,    0,   53,   54,
   55,   56,   57,   59,    0,    0,    0,   58,    0,  307,
  308,  309,  310,  311,  312,  313,  314,  315,  316,  317,
  318,  319,    0,    0,  320,  321,    0,    0,    0,  384,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  286,  286,  286,    0,    0,    0,  286,  286,    0,
  286,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  286,
  286,    0,  286,  286,  286,  286,  286,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   60,    0,    0,  286,
  286,  286,  286,  286,  286,  286,  286,  286,  286,  286,
  286,  286,   59,    0,  286,  286,  286,    0,    0,    0,
    0,    0,    3,    4,    5,    6,    7,    8,    0,    0,
    0,    9,   10,    0,    0,    0,   11,    0,   12,   13,
   14,   15,   16,   17,   18,    0,    0,    0,    0,   19,
   20,   21,   22,   23,   24,   25,    0,    0,   26,    0,
    0,    0,    0,    0,   27,   28,   29,   30,   31,   32,
   33,   34,   35,   36,   37,   38,   39,   40,   41,    0,
   42,   43,    0,   44,   45,   46,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   47,   60,    0,  260,   49,    0,
   50,   51,    0,   52,  221,   53,   54,   55,   56,   57,
    0,    0,  410,    0,   58,    0,    0,    4,    5,    6,
    7,    8,    0,    0,    0,    9,   10,    0,    0,    0,
   11,    0,   12,   13,   14,   15,   16,   17,   18,    0,
    0,    0,    0,   19,   20,   21,   22,   23,   24,   25,
    0,    0,   26,    0,    0,    0,    0,    0,   27,   28,
   29,   30,   31,   32,   33,   34,   35,   36,   37,   38,
   39,   40,   41,    0,   42,   43,    0,   44,   45,   46,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   60,   47,    0,
    0,   48,   49,    0,   50,   51,   59,   52,    0,   53,
   54,   55,   56,   57,    0,    0,    0,    0,   58,    0,
    0,    0,    0,    0,    0,    0,    4,    5,    6,    7,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   15,   16,   17,   18,    0,    0,
    0,    0,   19,   20,   21,   22,   23,   24,   25,    0,
    0,   26,    0,    0,    0,    0,    0,   27,   28,   29,
   30,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,   44,   45,   46,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   60,
    0,    0,    0,    0,  308,    0,    0,   47,    0,    0,
   48,   49,    0,   50,   51,    0,   52,    0,   53,   54,
   55,   56,   57,    0,    0,    0,    0,   58,    4,    5,
    6,    0,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   97,   98,   17,   18,
    0,    0,    0,    0,   99,   20,   21,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   28,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,   44,   45,
   46,    0,    0,    0,    0,    0,    0,  308,    0,    0,
    0,    0,  221,    0,    0,    0,    0,    0,    0,  218,
    0,    0,  107,   49,    0,   50,   51,    0,  219,  220,
   53,   54,   55,   56,   57,    0,    0,    0,    0,   58,
    4,    5,    6,    0,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   15,   16,
   17,   18,    0,    0,    0,    0,   19,   20,   21,   22,
   23,   24,   25,    0,    0,   26,    0,    0,    0,    0,
    0,    0,   28,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
   44,   45,   46,    0,    0,   60,    0,    0,    0,    0,
  221,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  218,    0,    0,  107,   49,    0,   50,   51,    0,
  611,  220,   53,   54,   55,   56,   57,    0,  308,  308,
  308,   58,  308,    0,    0,    0,  308,  308,    0,    0,
  513,  308,  513,  308,  308,  308,  308,  308,  308,  308,
    0,    0,    0,    0,  308,  308,  308,  308,  308,  308,
  308,    0,    0,  308,    0,    0,    0,    0,    0,    0,
  308,    0,    0,  308,  308,  308,  308,  308,  308,  308,
  308,  308,  308,  308,    0,  308,  308,    0,  308,  308,
  308,    0,    0,   60,    0,    0,    0,    0,  221,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  308,
    0,    0,  308,  308,    0,  308,  308,    0,    0,    0,
  308,  308,  308,  308,  308,    0,    4,    5,    6,  308,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   97,   98,   17,   18,    0,    0,
    0,    0,   99,   20,   21,   22,   23,   24,   25,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   28,    0,
    0,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,   44,   45,   46,    0,
    0,   60,    0,    0,    0,    0,  221,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  218,    0,    0,
  107,   49,    0,   50,   51,    0,  219,  220,   53,   54,
   55,   56,   57,    0,    4,    5,    6,   58,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   97,   98,   17,   18,    0,    0,    0,    0,
   99,   20,   21,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
    0,   42,   43,    0,   44,   45,   46,    0,    0,   60,
    0,    0,    0,    0,  221,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  218,    0,    0,  107,  412,
    0,   50,   51,    0,  219,  220,   53,   54,   55,   56,
   57,    0,    4,    5,    6,   58,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   97,   98,   17,   18,    0,    0,    0,    0,   99,  100,
  101,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   28,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,   40,   41,    0,   42,
   43,    0,   44,   45,   46,    0,    0,   60,    0,    0,
    0,    0,  221,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  218,    0,    0,  107,   49,    0,   50,
   51,    0,  602,  220,   53,   54,   55,   56,   57,    0,
    4,    5,    6,   58,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   97,   98,
   17,   18,    0,    0,    0,    0,   99,  100,  101,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   28,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
   44,   45,   46,    0,    0,   60,    0,    0,    0,    0,
  221,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  218,    0,    0,  107,   49,    0,   50,   51,    0,
  606,  220,   53,   54,   55,   56,   57,    0,    4,    5,
    6,   58,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   97,   98,   17,   18,
    0,    0,    0,    0,   99,   20,   21,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   28,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,   44,   45,
   46,    0,    0,   60,    0,    0,    0,    0,  221,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  218,
    0,    0,  107,   49,    0,   50,   51,    0,  602,  220,
   53,   54,   55,   56,   57,    0,    4,    5,    6,   58,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   97,   98,   17,   18,    0,    0,
    0,    0,   99,  100,  101,   22,   23,   24,   25,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   28,    0,
    0,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,   44,   45,   46,    0,
    0,   60,    0,    0,    0,    0,  221,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  218,    0,    0,
  107,   49,    0,   50,   51,    0,  805,  220,   53,   54,
   55,   56,   57,    0,    4,    5,    6,   58,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   97,   98,   17,   18,    0,    0,    0,    0,
   99,  100,  101,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
    0,   42,   43,    0,   44,   45,   46,    0,    0,   60,
    0,    0,    0,    0,  221,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  218,    0,    0,  107,   49,
    0,   50,   51,    0,  808,  220,   53,   54,   55,   56,
   57,    0,    4,    5,    6,   58,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   97,   98,   17,   18,    0,    0,    0,    0,   99,  100,
  101,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   28,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,   40,   41,    0,   42,
   43,    0,   44,   45,   46,    0,    0,   60,    0,    0,
    0,    0,  221,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  218,    0,    0,  107,   49,    0,   50,
   51,    0,  813,  220,   53,   54,   55,   56,   57,    0,
    4,    5,    6,   58,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   97,   98,
   17,   18,    0,    0,    0,    0,   99,  100,  101,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   28,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
   44,   45,   46,    0,    0,   60,    0,    0,    0,    0,
  255,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  218,    0,    0,  107,   49,    0,   50,   51,    0,
  884,  220,   53,   54,   55,   56,   57,    0,    4,    5,
    6,   58,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   97,   98,   17,   18,
    0,    0,    0,    0,   99,  100,  101,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   28,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,   44,   45,
   46,    0,    0,  255,    0,    0,    0,    0,   59,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  218,
    0,    0,  107,   49,    0,   50,   51,    0,  886,  220,
   53,   54,   55,   56,   57,    0,    4,    5,    6,   58,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   97,   98,   17,   18,    0,    0,
    0,    0,   99,  100,  101,   22,   23,   24,   25,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   28,    0,
    0,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,   44,   45,   46,    0,
    0,   60,    0,    0,    0,    0,  221,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  218,    0,    0,
  107,   49,    0,   50,   51,    0,  913,  220,   53,   54,
   55,   56,   57,    0,  255,  255,  255,   58,  255,    0,
    0,    0,  255,  255,    0,    0,    0,  255,    0,  255,
  255,  255,  255,  255,  255,  255,    0,    0,    0,    0,
  255,  255,  255,  255,  255,  255,  255,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  255,    0,    0,  255,
  255,  255,  255,  255,  255,  255,  255,  255,  255,  255,
    0,  255,  255,    0,  255,  255,  255,    0,    0,   60,
    0,    0,    0,    0,  221,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  255,    0,    0,  255,  255,
    0,  255,  255,    0,  255,  255,  255,  255,  255,  255,
  255,    0,    4,    5,    6,  255,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   15,   16,   17,   18,    0,    0,    0,    0,   19,   20,
   21,   22,   23,   24,   25,    0,    0,   26,    0,    0,
    0,    0,    0,    0,   28,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,   40,   41,    0,   42,
   43,    0,   44,   45,   46,    0,    0,   60,    0,    0,
    0,    0,  221,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  218,    0,    0,  107,   49,    0,   50,
   51,    0,    0,    0,   53,   54,   55,   56,   57,    0,
    4,    5,    6,   58,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   97,   98,
   17,   18,    0,    0,    0,    0,   99,   20,   21,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   28,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
   44,   45,   46,    0,    0,   60,    0,    0,    0,    0,
  221,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  218,    0,    0,  107,   49,    0,   50,   51,    0,
  265,    0,   53,   54,   55,   56,   57,    0,    4,    5,
    6,   58,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   15,   16,   17,   18,
    0,    0,    0,    0,   19,   20,   21,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   28,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,   44,   45,
   46,    0,    0,   60,    0,    0,    0,    0,  221,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  218,
    0,    0,  107,   49,    0,   50,   51,    0,  524,    0,
   53,   54,   55,   56,   57,    0,    4,    5,    6,   58,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   97,   98,   17,   18,    0,    0,
    0,    0,   99,  100,  101,   22,   23,   24,   25,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   28,    0,
    0,   31,   32,   33,   34,   35,   36,   37,   38,   39,
   40,   41,    0,   42,   43,    0,   44,   45,   46,    0,
    0,   60,    0,    0,    0,    0,  221,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  218,    0,    0,
  107,   49,    0,   50,   51,    0,  635,    0,   53,   54,
   55,   56,   57,    0,    4,    5,    6,   58,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   97,   98,   17,   18,    0,    0,    0,    0,
   99,  100,  101,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
    0,   42,   43,    0,   44,   45,   46,    0,    0,   60,
    0,    0,    0,    0,  221,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  218,    0,    0,  107,   49,
    0,   50,   51,    0,  524,    0,   53,   54,   55,   56,
   57,    0,    4,    5,    6,   58,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   97,   98,   17,   18,    0,    0,    0,    0,   99,  100,
  101,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   28,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,   40,   41,    0,   42,
   43,    0,   44,   45,   46,    0,    0,   60,    0,    0,
    0,    0,  515,    0,    0,    0,    0,    0,    0,  515,
    0,    0,    0,  218,    0,    0,  107,   49,    0,   50,
   51,    0,  705,    0,   53,   54,   55,   56,   57,    0,
    4,    5,    6,   58,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   97,   98,
   17,   18,    0,    0,    0,    0,   99,  100,  101,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   28,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
   44,   45,   46,    0,    0,  515,    0,    0,    0,    0,
  221,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  218,    0,    0,  107,   49,    0,   50,   51,    0,
  767,    0,   53,   54,   55,   56,   57,    0,    4,    5,
    6,   58,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   97,   98,   17,   18,
    0,    0,    0,    0,   99,  100,  101,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   28,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,   44,   45,
   46,    0,    0,   60,    0,    0,    0,    0,  221,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  218,
    0,    0,  107,   49,    0,   50,   51,    0,  846,    0,
   53,   54,   55,   56,   57,    0,  515,  515,  515,   58,
  515,    0,    0,    0,  515,  515,    0,    0,    0,  515,
    0,  515,  515,  515,  515,  515,  515,  515,    0,    0,
    0,    0,  515,  515,  515,  515,  515,  515,  515,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  515,    0,
    0,  515,  515,  515,  515,  515,  515,  515,  515,  515,
  515,  515,    0,  515,  515,    0,  515,  515,  515,    0,
    0,   60,    0,    0,    0,    0,  216,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  515,    0,    0,
  515,  515,    0,  515,  515,    0,    0,    0,  515,  515,
  515,  515,  515,    0,    4,    5,    6,  515,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   97,   98,   17,   18,    0,    0,    0,    0,
   99,  100,  101,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   28,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
    0,   42,   43,    0,   44,   45,   46,    0,    0,  216,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  218,    0,    0,  107,   49,
    0,   50,   51,    0,    0,    0,   53,   54,   55,   56,
   57,    0,    4,    5,    6,   58,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   15,   16,   17,   18,    0,    0,    0,    0,   19,   20,
   21,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   28,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,   40,   41,    0,   42,
   43,    0,   44,   45,   46,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  218,    0,    0,  107,   49,    0,   50,
   51,    0,    0,    0,   53,   54,   55,   56,   57,    0,
  216,  216,  216,   58,  216,    0,    0,    0,  216,  216,
    0,    0,    0,  216,    0,  216,  216,  216,  216,  216,
  216,  216,    0,    0,    0,    0,  216,  216,  216,  216,
  216,  216,  216,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  216,    0,    0,  216,  216,  216,  216,  216,
  216,  216,  216,  216,  216,  216,    0,  216,  216,    0,
  216,  216,  216,    0,    0,    0,  184,  179,    0,    0,
    0,  182,  180,    0,  181,    0,  183,    0,    0,    0,
    0,  216,    0,    0,  216,  216,    0,  216,  216,  176,
    0,  175,  216,  216,  216,  216,  216,    0,    0,    0,
    0,  216,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  178,    0,  186,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  177,    0,  185,    0,    0,    0,  184,
  179,    0,    0,    0,  182,  180,    0,  181,    0,  183,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  176,    0,  175,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  332,  327,    0,
    0,    0,  330,  328,    0,  329,  178,  331,  186,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  747,    0,
  324,    0,  323,  322,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  177,    0,  185,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  326,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  117,  118,  119,  120,
  121,  122,  123,  124,  325,    0,  125,  126,  127,  128,
  129,    0,    0,  130,  131,  132,  133,  134,  135,  136,
    0,    0,  137,  138,  139,  193,  194,  195,  196,  144,
  145,  146,  147,  148,  149,  150,  151,  152,  153,  154,
  155,  197,  198,  199,  159,  244,    0,  200,    0,    0,
    0,    0,    0,    0,    0,    0,  161,  162,    0,  163,
  164,  165,  166,    0,  167,  168,    0,    0,  169,    0,
    0,    0,  170,  171,  172,  173,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  174,    0,   53,  117,
  118,  119,  120,  121,  122,  123,  124,    0,    0,  125,
  126,  127,  128,  129,    0,    0,  130,  131,  132,  133,
  134,  135,  136,    0,    0,  137,  138,  139,  193,  194,
  195,  196,  144,  145,  146,  147,  148,  149,  150,  151,
  152,  153,  154,  155,  197,  198,  199,  159,    0,    0,
  200,    0,    0,    0,    0,    0,    0,    0,    0,  161,
  162,    0,  163,  164,  165,  166,    0,  167,  168,    0,
    0,  169,    0,    0,    0,  170,  171,  172,  173,    0,
  184,  179,    0,  187,    0,  182,  180,    0,  181,  174,
  183,   53,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  176,    0,  175,    0,    0,    0,    0,
  307,  308,  309,  310,  311,  312,  313,  314,  315,  316,
  317,  318,  319,    0,    0,  320,  321,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  178,    0,  186,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  177,    0,  185,
    0,  184,  179,    0,    0,    0,  182,  180,    0,  181,
    0,  183,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  176,    0,  175,    0,    0,    0,
  332,  327,    0,    0,    0,  330,  328,    0,  329,    0,
  331,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  324,    0,  323,    0,    0,  178,    0,
  186,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  326,  177,    0,
  185,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  325,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  117,  118,  119,  120,  121,  122,  123,  124,    0,    0,
  125,  126,  127,  128,  129,    0,    0,  130,  131,  132,
  133,  134,  135,  136,    0,    0,  137,  138,  139,  140,
  141,  142,  143,  144,  145,  146,  147,  148,  149,  150,
  151,  152,  153,  154,  155,  156,  157,  158,  159,   35,
   36,  160,   38,    0,    0,    0,    0,    0,    0,    0,
  161,  162,    0,  163,  164,  165,  166,    0,  167,  168,
    0,    0,  169,    0,    0,    0,  170,  171,  172,  173,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  174,  117,  118,  119,  120,  121,  122,  123,  124,    0,
    0,  125,  126,  127,  128,  129,    0,    0,  130,  131,
  132,  133,  134,  135,  136,    0,    0,  137,  138,  139,
  193,  194,  195,  196,  144,  145,  146,  147,  148,  149,
  150,  151,  152,  153,  154,  155,  197,  198,  199,  159,
  280,  281,  200,  282,    0,    0,    0,    0,    0,    0,
    0,  161,  162,    0,  163,  164,  165,  166,    0,  167,
  168,    0,    0,  169,    0,    0,    0,  170,  171,  172,
  173,    0,  184,  179,    0,    0,    0,  182,  180,    0,
  181,  174,  183,  307,  308,  309,  310,  311,  312,  313,
  314,    0,  316,  317,    0,  176,    0,  175,  320,  321,
    0,    0,    0,    0,    0,  184,  179,    0,    0,    0,
  182,  180,    0,  181,    0,  183,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  176,  178,
  175,  186,    0,    0,    0,    0,    0,    0,    0,    0,
  184,  179,    0,    0,    0,  182,  180,    0,  181,    0,
  183,    0,    0,    0,    0,    0,    0,    0,    0,  177,
    0,  185,  178,  176,  186,  175,    0,    0,    0,    0,
    0,    0,    0,  184,  179,    0,    0,    0,  182,  180,
    0,  181,    0,  183,    0,    0,    0,    0,    0,    0,
    0,    0,  177,    0,  185,    0,  176,  178,  175,  186,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  177,    0,  185,
  178,    0,  186,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  177,    0,  185,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  117,  118,  119,  120,  121,  122,  123,  124,
    0,    0,  125,  126,  127,  128,  129,    0,    0,  130,
  131,  132,  133,  134,  135,  136,    0,    0,  137,  138,
  139,  193,  194,  195,  196,  144,  145,  146,  147,  148,
  149,  150,  151,  152,  153,  154,  155,  197,  198,  199,
  159,    0,    0,  200,    0,    0,    0,    0,    0,    0,
    0,    0,  161,  162,    0,  163,  164,  165,  166,    0,
  167,  168,    0,    0,  169,    0,    0,    0,  170,  171,
  172,  173,  254,  255,    0,    0,  256,    0,    0,    0,
    0,    0,  174,    0,    0,  161,  162,    0,  163,  164,
  165,  166,    0,  167,  168,    0,    0,  169,    0,    0,
    0,  170,  171,  172,  173,    0,    0,  506,  507,    0,
    0,  508,    0,    0,    0,  174,    0,    0,    0,    0,
  161,  162,    0,  163,  164,  165,  166,    0,  167,  168,
    0,    0,  169,    0,    0,    0,  170,  171,  172,  173,
  512,  255,    0,    0,  513,    0,    0,    0,    0,    0,
  174,    0,    0,  161,  162,    0,  163,  164,  165,  166,
    0,  167,  168,    0,    0,  169,    0,    0,    0,  170,
  171,  172,  173,    0,  184,  179,    0,    0,    0,  182,
  180,    0,  181,  174,  183,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,  176,    0,  175,
    0,    0,    0,    0,    0,    0,    0,  184,  179,    0,
    0,    0,  182,  180,    0,  181,    0,  183,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  176,  178,  175,  186,    0,    0,    0,    0,    0,    0,
    0,    0,  184,  179,    0,    0,    0,  182,  180,    0,
  181,    0,  183,    0,    0,    0,    0,    0,    0,    0,
    0,  177,    0,  185,  178,  176,  186,  175,    0,    0,
    0,    0,    0,    0,    0,  184,  179,    0,    0,    0,
  182,  180,    0,  181,    0,  183,    0,    0,    0,    0,
    0,    0,    0,    0,  177,    0,  185,    0,  176,  178,
  175,  186,    0,    0,    0,    0,    0,    0,    0,    0,
  184,  179,    0,    0,    0,  182,  180,    0,  181,    0,
  183,    0,    0,    0,    0,    0,    0,    0,    0,  177,
    0,  185,  178,  176,  186,  175,    0,    0,    0,    0,
    0,    0,    0,  184,  179,    0,    0,    0,  182,  180,
    0,  181,    0,  183,    0,    0,    0,    0,    0,    0,
    0,    0,  177,    0,  185,    0,  176,  178,  175,  186,
    0,    0,    0,    0,    0,    0,    0,    0,  184,  179,
    0,    0,    0,  182,  180,    0,  181,    0,  183,    0,
    0,    0,    0,    0,    0,    0,    0,  177,    0,  185,
  178,  176,  186,  175,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  177,    0,  185,    0,    0,  178,    0,  186,    0,    0,
    0,  542,  507,    0,    0,  543,    0,    0,    0,    0,
    0,    0,    0,    0,  161,  162,    0,  163,  164,  165,
  166,    0,  167,  168,    0,  177,  169,  185,    0,    0,
  170,  171,  172,  173,  597,  507,    0,    0,  598,    0,
    0,    0,    0,    0,  174,    0,    0,  161,  162,    0,
  163,  164,  165,  166,    0,  167,  168,    0,    0,  169,
    0,    0,    0,  170,  171,  172,  173,    0,    0,  599,
  255,    0,    0,  600,    0,    0,    0,  174,    0,    0,
    0,    0,  161,  162,    0,  163,  164,  165,  166,    0,
  167,  168,    0,    0,  169,    0,    0,    0,  170,  171,
  172,  173,  637,  507,    0,    0,  638,    0,    0,    0,
    0,    0,  174,    0,    0,  161,  162,    0,  163,  164,
  165,  166,    0,  167,  168,    0,    0,  169,    0,    0,
    0,  170,  171,  172,  173,    0,    0,  639,  255,    0,
    0,  640,    0,    0,    0,  174,    0,    0,    0,    0,
  161,  162,    0,  163,  164,  165,  166,    0,  167,  168,
    0,    0,  169,    0,    0,    0,  170,  171,  172,  173,
  713,  507,    0,    0,  714,    0,    0,    0,    0,    0,
  174,    0,    0,  161,  162,    0,  163,  164,  165,  166,
    0,  167,  168,    0,    0,  169,    0,   55,    0,  170,
  171,  172,  173,    0,    0,  715,  255,   55,    0,  716,
    0,    0,    0,  174,    0,    0,    0,    0,  161,  162,
    0,  163,  164,  165,  166,    0,  167,  168,    0,    0,
  169,    0,    0,    0,  170,  171,  172,  173,   55,  184,
  179,    0,    0,    0,  182,  180,    0,  181,  174,  183,
    0,    0,    0,    0,    0,   55,   55,    0,    0,    0,
    0,    0,  176,    0,  175,    0,    0,    0,    0,    0,
    0,    0,  184,  179,    0,    0,    0,  182,  180,    0,
  181,    0,  183,    0,    0,    0,    0,    0,    0,    0,
   55,    0,    0,    0,    0,  176,  178,  175,  186,    0,
    0,    0,    0,    0,    0,    0,    0,  332,  327,    0,
    0,    0,  330,  328,  594,  329,    0,  331,    0,    0,
    0,    0,   55,    0,    0,    0,  177,    0,  185,  178,
  324,  186,  323,  322,    0,    0,    0,    0,    0,    0,
  332,  327,    0,    0,    0,  330,  328,    0,  329,    0,
  331,    0,    0,    0,    0,    0,    0,    0,    0,  177,
    0,  185,    0,  324,  326,  323,  322,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   40,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   40,   42,
    0,    0,    0,    0,  325,    0,    0,  326,    0,   42,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   40,
    0,    0,    0,    0,    0,    0,    0,  325,    0,    0,
   42,    0,    0,    0,    0,    0,   40,   40,    0,    0,
   47,    0,    0,    0,    0,    0,    0,   42,   42,    0,
   47,    0,    0,    0,    0,    0,    0,    0,    0,   55,
   55,   55,    0,    0,   55,   55,   55,    0,   55,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   55,   55,
    0,   47,    0,    0,    0,    0,    0,   55,   55,    0,
   55,   55,   55,   55,   55,    0,    0,    0,   47,   47,
    0,    0,    0,   40,    0,    0,  889,  507,    0,    0,
  890,    0,    0,    0,   42,    0,    0,    0,    0,  161,
  162,    0,  163,  164,  165,  166,    0,  167,  168,    0,
    0,  169,    0,    0,    0,  170,  171,  172,  173,  891,
  255,    0,    0,  892,    0,    0,    0,    0,    0,  174,
    0,    0,  161,  162,    0,  163,  164,  165,  166,    0,
  167,  168,    0,    0,  169,   47,    0,    0,  170,  171,
  172,  173,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  174,    0,    0,    0,    0,    0,    0,    0,
  307,  308,  309,  310,  311,  312,  313,  314,  315,  316,
  317,  318,  319,    0,    0,  320,  321,  750,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  307,  308,  309,  310,  311,  312,  313,
  314,  315,  316,  317,  318,  319,    0,    0,  320,  321,
   40,   40,   40,    0,    0,   40,   40,   40,    0,   40,
    0,   42,   42,   42,    0,    0,   42,   42,   42,   40,
   42,    0,    0,    0,    0,    0,    0,    0,   40,   40,
   42,   40,   40,   40,   40,   40,    0,    0,    0,   42,
   42,    0,   42,   42,   42,   42,   42,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   47,   47,   47,    0,    0,   47,   47,   47,
    0,   47,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   47,    0,    0,    0,    0,    0,    0,    0,    0,
   47,   47,    0,   47,   47,   47,   47,   47,    0,    0,
    4,    5,    6,    0,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   97,   98,
   17,   18,    0,    0,    0,    0,   99,  100,  101,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  102,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
    0,    0,   46,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  275,    0,    0,  349,   49,    0,   50,   51,    0,
  748,  749,   53,   54,   55,   56,   57,    0,    4,    5,
    6,  108,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   97,   98,   17,   18,
    0,    0,    0,    0,   99,  100,  101,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  102,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,    0,    0,
   46,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  275,
    0,    0,  349,   49,    0,   50,   51,    0,  865,  866,
   53,   54,   55,   56,   57,    0,    4,    5,    6,  108,
    8,    0,    0,    0,    9,   10,    0,    0,    0,   11,
    0,   12,   13,   14,   97,   98,   17,   18,    0,    0,
    0,    0,   99,  100,  101,   22,   23,   24,   25,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  102,    0,
    0,   31,   32,  103,   34,   35,   36,  104,   38,   39,
   40,   41,    0,   42,   43,    0,    0,    0,   46,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  105,    0,    0,  106,    0,    0,
  107,   49,    0,   50,   51,    0,    0,    0,   53,   54,
   55,   56,   57,    0,    4,    5,    6,  108,    8,    0,
    0,    0,    9,   10,    0,    0,    0,   11,    0,   12,
   13,   14,   97,   98,   17,   18,    0,    0,    0,    0,
   99,  100,  101,   22,   23,   24,   25,    0,    0,    0,
    0,    0,    0,    0,    0,    0,  102,    0,    0,   31,
   32,   33,   34,   35,   36,   37,   38,   39,   40,   41,
    0,   42,   43,    0,    0,    0,   46,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,  275,    0,    0,  349,   49,
    0,   50,   51,    0,  350,    0,   53,   54,   55,   56,
   57,    0,    4,    5,    6,  108,    8,    0,    0,    0,
    9,   10,    0,    0,    0,   11,    0,   12,   13,   14,
   97,   98,   17,   18,    0,    0,    0,    0,   99,  100,
  101,   22,   23,   24,   25,    0,    0,    0,    0,    0,
    0,    0,    0,    0,  102,    0,    0,   31,   32,   33,
   34,   35,   36,   37,   38,   39,   40,   41,    0,   42,
   43,    0,    0,    0,   46,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,  388,    0,    0,   48,   49,    0,   50,
   51,    0,   52,    0,   53,   54,   55,   56,   57,    0,
    4,    5,    6,  108,    8,    0,    0,    0,    9,   10,
    0,    0,    0,   11,    0,   12,   13,   14,   97,   98,
   17,   18,    0,    0,    0,    0,   99,  100,  101,   22,
   23,   24,   25,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  102,    0,    0,   31,   32,   33,   34,   35,
   36,   37,   38,   39,   40,   41,    0,   42,   43,    0,
    0,    0,   46,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  275,    0,    0,  107,   49,    0,   50,   51,    0,
    0,    0,   53,   54,   55,   56,   57,    0,    4,    5,
    6,  108,    8,    0,    0,    0,    9,   10,    0,    0,
    0,   11,    0,   12,   13,   14,   97,   98,   17,   18,
    0,    0,    0,    0,   99,  100,  101,   22,   23,   24,
   25,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  102,    0,    0,   31,   32,   33,   34,   35,   36,   37,
   38,   39,   40,   41,    0,   42,   43,    0,    0,    0,
   46,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  819,
    0,    0,  107,   49,    0,   50,   51,    0,    0,    0,
   53,   54,   55,   56,   57,    0,    0,    0,    0,  108,
/* start of table check */  2, 
    3,    4,    5,   27,   89,   21,   15,   16,  230,  240,
   19,    6,   15,   16,    7,    7,   19,    6,    7,   80,
   46,   72,    2,   26,  381,  375,  387,   52,   15,   16,
   44,   38,   19,   10,   27,   27,   10,    3,   27,   21,
   50,   51,  573,  583,  106,   48,   49,   50,   46,   52,
  111,  348,  124,   10,  123,  685,   59,   10,   53,    2,
    3,    4,    5,   50,   53,   93,  406,   44,   48,   48,
  370,   59,   41,   11,   50,   51,  269,   10,  271,   95,
   10,   58,   59,  208,   58,   59,   89,   10,  213,   46,
  215,  216,  260,   91,   92,    4,    5,  125,   46,   46,
   10,  123,  105,   10,  107,   48,  446,   46,   10,   52,
   32,   40,  313,   95,  286,   10,   40,  289,   41,  101,
   10,   46,   40,   46,   10,   58,   59,  107,   10,   59,
  685,    0,  114,   10,   72,  690,   10,   44,   10,  308,
  102,   10,   26,   52,   91,   92,   89,   46,   58,   59,
   44,   41,   46,   10,   44,  356,   58,   59,   44,  565,
   10,  567,   44,  350,  107,   49,   91,   92,   10,   44,
   10,  109,   32,  704,  396,  300,  301,  302,  303,   38,
   89,  349,   59,   42,   58,   59,   61,   59,   60,  313,
   59,   41,   91,   92,  123,  125,  304,   91,   92,   46,
  308,   41,   59,  206,  207,  208,   10,  837,   32,  559,
  213,  280,  215,  216,  217,  276,   46,   59,   44,   59,
   46,  105,    2,    3,    4,    5,    6,   10,  237,  282,
  239,  240,  356,  656,  237,  339,  239,  240,  661,   46,
   44,  541,   46,   40,   91,   92,  249,   44,   10,   46,
  237,   41,  239,  240,   58,   59,  328,  260,   10,  339,
  560,   91,   92,  206,  240,   91,   92,   61,   48,  249,
   61,   61,   52,   37,  217,  572,   59,   44,   42,   41,
  260,  260,  837,   47,   91,   92,   44,   91,   92,   41,
  267,  463,  340,  267,   91,   92,  299,  300,  301,  302,
  303,  304,  305,  306,  266,  367,  249,  847,  217,   89,
  268,  269,  338,  339,   59,  341,  847,  260,  344,  345,
  124,  290,  291,  207,  208,  350,  340,  107,  389,  213,
  340,  215,  216,  299,  385,    2,    3,  340,  304,  337,
  343,  348,  368,  346,  347,  348,  349,  350,  281,  338,
  707,  280,  269,  340,  271,  343,  299,  267,  346,  347,
  348,  304,  308,  340,  340,  267,  340,  370,  371,  349,
  349,  347,  348,  399,  400,  781,  782,  783,  367,   44,
  337,   48,  722,  605,  256,  279,   44,  618,  728,  337,
  337,  313,  402,  267,  616,  290,  291,  339,  337,  423,
  290,  291,  412,  412,  279,   10,  349,  350,  434,  412,
  399,  269,  337,  271,  337,  540,  300,  301,  302,  303,
   10,  305,  306,  426,  774,  363,  206,  370,  454,  786,
  423,  423,  354,  355,  423,   10,  339,  217,  337,   40,
  107,  350,  803,  337,  433,  304,  426,  385,  386,  387,
  340,   41,   37,  279,   59,  417,  418,   42,   43,    0,
   45,   10,   47,  267,  453,   61,   37,   38,  874,  249,
  770,   42,   43,   53,   45,   55,   47,  439,  440,  123,
  260,  264,  279,  426,   59,  320,  843,  371,  347,  348,
  337,   40,  125,  509,  339,   44,   41,   46,  514,  721,
   41,   41,  724,  725,  520,  521,   41,  337,   41,   58,
   59,  337,   10,  308,  517,  855,   61,  313,  356,  299,
  316,  504,  505,   94,  304,   41,   61,  509,  412,  517,
  337,  290,  291,  337,  834,   41,   93,  540,  541,  206,
  337,  356,   91,   92,  320,   61,  570,  269,  320,  271,
  217,  273,  339,  124,  549,   61,  320,  560,  354,  355,
  549,   59,  544,  648,  904,  591,   44,   10,   46,  349,
  350,  311,  312,  346,  577,  124,  579,  570,  570,  582,
   44,  570,  249,  264,  125,   41,  548,  809,   44,  811,
  370,  262,  717,  260,  620,  621,  572,  577,  541,  579,
  538,  590,  582,  271,  580,   61,   40,   41,  618,  269,
   44,  308,   46,   91,   92,  618,   59,  560,  279,  313,
  311,  312,  625,  561,  263,  264,  629,   61,   10,  851,
  852,  618,  299,  306,  577,  857,  579,  304,   44,  582,
  339,  314,  315,   41,   44,  648,  426,   41,  125,  629,
  653,   41,  520,  521,  657,  658,  540,   91,   92,  264,
  354,  355,  356,  885,  269,  653,  271,  792,   32,  657,
  658,  293,  294,  295,  296,  297,   38,   59,   40,   93,
   42,  903,  349,  313,  906,  339,  629,  630,  339,  264,
  124,  674,  339,  268,  269,  917,  699,  339,   44,   41,
  264,   61,  304,  370,   16,  648,   10,   19,  711,  712,
   44,   44,   44,  304,  717,  269,  726,  726,  267,  699,
  682,  262,  263,  264,  354,  355,  356,  268,  269,  267,
  271,  711,  712,  264,  279,  320,  348,   41,  676,  648,
   44,  625,  827,   44,  279,  748,  749,   10,   44,  320,
  726,  754,  755,  756,   38,   59,  699,  339,   42,  426,
  821,  541,  339,  279,  335,  336,  264,  770,  711,  712,
  732,  269,  339,  279,  754,  755,  756,   41,   41,   41,
  560,   44,  785,   41,  746,  125,  789,   41,  337,  792,
  264,   41,  263,  264,  264,  798,   59,  577,  269,  579,
  263,  279,  582,  340,   44,  748,  749,  306,  307,  789,
  309,  754,  755,  756,  264,  314,  315,  264,  798,  262,
  263,  264,   41,  125,  827,  124,  269,  770,   40,  832,
   44,  834,   44,  717,   46,   41,  862,   44,  841,  748,
  749,  264,  785,  853,  313,  279,  789,  264,  264,  629,
   44,  264,  832,   44,   41,  798,  308,  860,   10,  337,
  348,  841,  865,  866,   44,  803,  734,  735,  648,  264,
  125,  264,  861,  876,  541,  264,  785,  125,  881,   91,
   92,  263,  264,  348,  827,  354,  355,  356,   44,  832,
  893,  834,   44,  560,   46,  264,  876,  348,  841,   44,
   10,  881,  348,  337,   93,  908,   58,   59,  792,  264,
  577,  123,  579,  916,  125,  582,   40,  860,  827,  699,
   41,   93,  865,  866,   93,  237,  125,  239,  240,   41,
  271,  711,  712,  876,   44,   41,   46,  313,  881,   91,
   92,   44,  304,   46,  306,  307,  308,  309,   58,   59,
  893,  860,   93,   37,   38,  125,  865,  866,   42,   43,
  124,   45,  629,   47,  264,  908,   15,   16,  748,  749,
   19,  124,  124,  916,  754,  755,  756,  124,  354,  355,
  356,   91,   92,   62,  893,  347,  348,  124,   91,   92,
  770,  124,  124,   10,    5,   44,   45,  901,    6,  908,
  774,   50,   51,  340,  875,  785,  559,  916,  690,  789,
   59,   60,   79,  217,  124,  313,  690,   72,  798,   -1,
  304,   -1,  306,  307,  308,  309,   -1,   44,   37,   46,
   -1,   -1,  699,   42,   43,   10,   45,   -1,   47,   -1,
   -1,   58,   59,   -1,  711,  712,   -1,  827,   -1,   -1,
    0,   -1,  832,   -1,  834,   -1,  354,  355,  356,   -1,
   10,  841,   -1,  347,  348,   40,   -1,  279,  280,   44,
   -1,   46,   -1,   -1,   91,   92,   -1,   -1,   -1,   -1,
  860,  748,  749,   58,   59,  865,  866,  754,  755,  756,
   -1,   41,   -1,   -1,   44,   -1,  876,  313,   -1,   -1,
   -1,  881,   -1,  770,   -1,   -1,   -1,  124,   58,   59,
    0,   -1,   -1,  893,   10,  267,   91,   92,  785,   -1,
   10,   -1,  789,   -1,   -1,  337,   -1,   -1,  908,   40,
   -1,  798,   -1,   44,   -1,   46,  916,   -1,  354,  355,
  356,   -1,   38,   93,   40,   -1,   42,   -1,  123,  124,
   46,   41,   -1,   -1,   44,   -1,   -1,  267,   -1,   -1,
   -1,   -1,   -1,   59,   10,  832,   -1,  834,   58,   59,
  219,  220,  221,   63,  841,  125,  279,   -1,   -1,   -1,
   91,   92,   -1,   -1,   -1,  337,   -1,   -1,  237,   10,
  239,  240,   38,  860,   40,   -1,   42,   -1,  865,  866,
   46,   -1,   -1,   93,   -1,   -1,   -1,   -1,   -1,  876,
   -1,   -1,  123,   59,  881,   -1,  265,   38,   -1,   40,
   -1,   42,   -1,   -1,   -1,   46,  893,  337,   -1,   -1,
   -1,   -1,   -1,   40,  337,  125,  320,   44,   59,   46,
   -1,  908,    2,    3,   -1,   -1,   -1,   -1,   -1,  916,
  267,  335,  336,   -1,   -1,   -1,   -1,   -1,  307,  308,
  309,  310,  311,  312,  313,  314,  315,  316,  317,  318,
  319,  320,  321,   -1,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,   91,   92,   -1,   -1,   48,   -1,
   -1,  340,  267,   -1,  343,   -1,   -1,  346,  347,  348,
   -1,   -1,   -1,   -1,   -1,  280,   -1,   -1,   -1,   -1,
   -1,  320,  262,  263,  264,   -1,  123,  267,  268,  269,
  337,  271,   -1,   -1,   -1,   -1,  335,  336,   -1,    0,
   -1,  281,  282,   -1,   -1,   -1,   -1,    2,    3,   10,
  290,  291,   -1,  293,  294,  295,  296,  107,   -1,  398,
   15,   16,   -1,  402,   19,   -1,  405,  406,   -1,  408,
  409,   26,  337,  412,   -1,   -1,   -1,   -1,  279,  280,
   41,   -1,  262,  263,  264,   -1,  425,  267,  268,  269,
   -1,  271,   -1,   48,   49,   50,  435,   -1,   59,   -1,
  340,  281,  282,  442,   59,   10,  346,  446,   -1,  448,
  290,  291,   -1,  293,  294,  295,  296,  297,  304,   -1,
  306,  307,  308,  309,   -1,   -1,   -1,   -1,  467,  468,
   -1,   -1,   -1,   38,   -1,   40,  337,   42,   -1,   -1,
   -1,   46,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  105,  337,  107,  492,   59,   -1,  206,   -1,   -1,   -1,
  340,  347,  348,   -1,  125,   -1,  346,    0,  304,   -1,
  306,  307,  308,  309,   -1,   -1,   -1,   -1,  517,   -1,
   -1,   -1,  279,  280,    0,  524,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  304,   10,  306,  307,  308,  309,  249,
   -1,  337,   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,
  260,  347,  348,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   41,  337,   -1,   44,   -1,
   50,   51,   -1,  572,   -1,   -1,  347,  348,   -1,   -1,
  337,  580,   58,   59,   -1,   -1,   -1,   -1,   -1,  299,
   -1,  206,  207,  208,  304,   -1,   -1,   -1,  213,   -1,
  215,  216,  601,  602,   -1,   -1,   -1,  606,   -1,   -1,
   -1,   -1,  611,   41,   -1,   -1,   44,   -1,   46,  618,
    0,   -1,  237,   -1,  239,  240,   -1,   -1,   -1,   -1,
   10,   -1,  125,   61,  249,   -1,  635,   -1,   -1,  349,
   -1,  262,  263,  264,   -1,  260,   -1,  268,  269,  125,
  271,   -1,   -1,   -1,  653,  654,   -1,   -1,  657,  658,
  370,   41,   -1,   91,   92,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  671,  672,   -1,   -1,   -1,   -1,   -1,   59,
   -1,   10,  681,   -1,  299,  300,  301,  302,  303,  304,
  305,  306,   -1,   -1,   -1,   -1,  124,   -1,   -1,   -1,
   -1,   40,   41,   -1,   -1,   44,  705,   46,   -1,   38,
   -1,   40,   -1,   42,   -1,   -1,  426,   46,   -1,  718,
  719,  720,   61,  722,   -1,  340,   -1,  726,  343,  728,
   59,  346,  347,  348,  349,   -1,   -1,   -1,   -1,  304,
   -1,  306,  307,  308,  309,  125,  226,   -1,  747,   -1,
  230,   -1,   91,   92,   -1,  370,  371,   -1,   -1,   -1,
  240,   -1,   -1,   -1,   -1,  219,  220,   -1,  767,  262,
  263,  264,  337,   -1,   -1,  268,  269,   -1,  271,   -1,
   -1,   -1,  347,  348,  123,  124,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,  412,  797,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  805,   -1,   -1,  808,
   -1,  426,   -1,   -1,  813,   -1,   10,  293,  294,  295,
  296,  297,   -1,   41,   -1,   -1,   44,   -1,   46,   -1,
   -1,  541,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   61,   38,   -1,   40,  846,   42,   -1,
  560,  279,   46,   -1,  853,   -1,  855,   -1,   -1,   -1,
  340,   -1,   -1,   -1,  340,   59,   -1,  577,   -1,  579,
   -1,   10,  582,   91,   92,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,  884,   -1,  886,  268,  269,
   -1,  271,   -1,  347,  348,   -1,   -1,   -1,  378,   38,
   -1,   40,  517,   42,   -1,  904,  124,   46,   -1,  337,
   -1,   -1,   -1,   -1,  913,   -1,  396,   -1,   -1,  629,
   59,   -1,   -1,   -1,   -1,  540,  541,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  416,   -1,   -1,   -1,
  279,  280,   -1,   -1,  398,  560,   -1,   -1,  402,   -1,
   -1,   -1,  406,   -1,   41,   -1,   -1,   44,  412,   46,
   37,   38,  577,   -1,  579,   42,   43,  582,   45,   -1,
   47,   -1,   -1,   -1,   61,  304,   -1,  306,  307,  308,
  309,   -1,   -1,   60,   -1,   62,   -1,   -1,  442,  699,
   -1,   -1,  446,   -1,  448,   -1,   -1,   -1,  337,   -1,
   10,  711,  712,  618,   91,   92,   -1,   -1,  337,   -1,
  625,   -1,   -1,   -1,  629,   -1,   -1,   94,  347,  348,
   -1,   -1,   -1,  503,  504,  505,   -1,   -1,   38,   -1,
   40,   -1,   42,   -1,  514,   -1,   46,  124,  653,   -1,
  520,  521,  657,  658,  754,  755,  756,  124,   -1,   59,
   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,   46,
  770,  279,   -1,   -1,   -1,   -1,  546,   -1,   -1,   -1,
  524,   -1,   -1,   -1,   61,   -1,   -1,   -1,   -1,  789,
   -1,   -1,  562,   -1,  699,  565,   -1,  567,  798,   -1,
   -1,   -1,  572,  573,   -1,   -1,  711,  712,   10,   -1,
   -1,   -1,  717,   -1,   91,   92,   -1,   -1,   -1,   -1,
  304,   -1,  306,  307,  308,  309,   -1,   -1,  572,  337,
   -1,   -1,  832,   -1,  834,  605,  580,   -1,   40,   -1,
   -1,  841,   44,   -1,   46,   -1,  616,  124,   -1,  754,
  755,  756,   -1,  337,   -1,   -1,   58,   59,  602,   -1,
   -1,   -1,  606,  347,  348,  770,   -1,  611,   -1,   -1,
   -1,   -1,   -1,   -1,  618,  304,  876,  306,  307,  308,
  309,  881,   -1,   -1,  789,   -1,   -1,  792,   -1,   91,
   92,   -1,   -1,  798,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  279,   -1,  674,   -1,   -1,   -1,  337,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  347,  348,
   -1,  123,  124,   -1,   -1,   -1,  696,  832,  672,  834,
  700,    0,  702,   -1,  704,   -1,  841,  681,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  721,   -1,  320,  724,  725,   -1,   -1,  325,  326,
  337,  705,   -1,   -1,  734,  735,   -1,   -1,  335,  336,
   -1,  876,   41,   -1,   -1,   44,  881,   -1,  722,   40,
   41,   -1,   -1,   44,  728,   46,   -1,   -1,   -1,   58,
   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   61,   -1,  279,   -1,  304,   -1,  306,  307,  308,  309,
   10,  781,  782,  783,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  767,   93,   -1,    0,   -1,   -1,   -1,
   91,   92,   -1,   -1,   -1,   -1,   10,  337,   -1,  809,
   40,  811,   -1,   -1,   44,   -1,   46,  347,  348,   -1,
   -1,   -1,   -1,  797,   -1,   -1,  125,   -1,   58,   59,
  337,   -1,  123,  124,  808,  267,   -1,   41,   -1,  813,
   44,   -1,   -1,   -1,   -1,   -1,   -1,  847,  280,   -1,
   -1,  851,  852,   -1,   58,   59,   60,  857,   62,   63,
   -1,   91,   92,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  846,   -1,  874,  875,   -1,   -1,   -1,  853,
   -1,  855,    0,   -1,   -1,  885,   -1,   -1,   -1,   93,
   -1,   -1,   10,  123,  124,   -1,   -1,   -1,   -1,   -1,
   -1,  901,   -1,  903,   -1,  337,  906,   -1,   -1,   -1,
  884,   -1,  886,   -1,   -1,   33,   -1,  917,   -1,   37,
   38,  125,   40,   41,   42,   43,   44,   45,   46,   47,
  904,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  913,
   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   91,   92,   93,   94,   -1,   -1,   -1,
    0,   -1,  281,  282,   -1,   -1,   -1,   -1,  279,  280,
   10,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,
   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,   38,   -1,
   40,   41,   42,   43,   44,   45,   46,   47,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  267,   58,   59,
   60,   61,   62,   63,   -1,   -1,  337,  346,   -1,   -1,
  280,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   -1,   91,   92,   93,   94,   -1,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  123,  124,  125,  126,  337,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  321,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,  257,
  258,  259,  346,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,  279,  280,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,   -1,  314,  315,   -1,  317,
  318,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,   -1,   -1,  335,  336,  337,
  338,  339,  340,  341,  342,   -1,  344,  345,  346,  347,
  348,  349,  350,  351,  352,  353,   -1,  257,  258,  259,
  358,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,  273,  274,  275,  276,  277,  278,  279,
  280,  281,  282,  283,  284,  285,  286,  287,  288,  289,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,  299,
   -1,   -1,  302,  303,  304,  305,  306,  307,  308,  309,
  310,  311,  312,   -1,  314,  315,   -1,  317,  318,  319,
  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,    0,   -1,  335,  336,  337,  338,  339,
  340,  341,  342,   10,  344,  345,  346,  347,  348,  349,
  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   37,   38,   -1,   40,   41,   42,   43,   44,   45,   46,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,
   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,
   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,   38,
   -1,   40,   41,   42,   43,   44,   45,   46,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   91,   92,   93,   94,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  123,  124,  125,  126,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  257,  258,  259,   -1,  261,  262,  263,  264,  265,  266,
  267,  268,  269,  270,  271,  272,  273,  274,  275,  276,
  277,  278,   -1,  280,  281,  282,  283,  284,  285,  286,
  287,  288,  289,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,  320,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
  337,  338,   -1,  340,  341,  342,   -1,  344,  345,  346,
  347,  348,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,  262,  263,  264,  265,  266,  267,  268,
  269,  270,  271,  272,  273,  274,  275,  276,  277,  278,
   -1,  280,  281,  282,  283,  284,  285,  286,  287,  288,
  289,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,  320,  321,  322,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,    0,   -1,  335,  336,  337,  338,
   -1,  340,  341,  342,   10,  344,  345,  346,  347,  348,
  349,  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   33,   -1,   -1,
   -1,   37,   38,   -1,   40,   41,   42,   43,   44,   45,
   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   61,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   91,   92,   93,   94,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,  124,  125,
  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   37,
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
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  257,  258,  259,   -1,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,  271,  272,  273,  274,  275,
  276,  277,  278,   -1,  280,  281,  282,  283,  284,  285,
  286,  287,  288,  289,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,  299,   -1,   -1,  302,  303,  304,  305,
  306,  307,  308,  309,  310,  311,  312,   -1,  314,  315,
   -1,  317,  318,  319,  320,  321,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,   -1,   -1,  335,
  336,  337,  338,   -1,  340,  341,  342,   -1,  344,  345,
  346,  347,  348,  349,  350,  351,  352,  353,   -1,  257,
  258,  259,  358,  261,  262,  263,  264,  265,  266,  267,
  268,  269,  270,  271,  272,  273,  274,  275,  276,  277,
  278,   -1,  280,  281,  282,  283,  284,  285,  286,  287,
  288,  289,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,  307,
  308,  309,  310,  311,  312,   -1,  314,  315,   -1,  317,
  318,  319,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,    0,   -1,  335,  336,  337,
  338,   -1,  340,  341,  342,   10,  344,  345,  346,  347,
  348,  349,  350,  351,  352,  353,   -1,   -1,   -1,   -1,
  358,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   37,   38,   -1,   40,   41,   42,   43,   44,
   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   93,   94,
   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,  124,
  125,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   37,   38,   -1,   40,   41,   42,   43,   44,   45,   46,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   91,   92,   93,   94,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  123,  124,  125,  126,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  257,  258,  259,   -1,  261,  262,  263,  264,
  265,  266,  267,  268,  269,  270,  271,  272,  273,  274,
  275,  276,  277,  278,   -1,  280,  281,  282,  283,  284,
  285,  286,  287,  288,  289,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,  320,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,   -1,   -1,
  335,  336,  337,  338,   -1,  340,  341,  342,   -1,  344,
  345,  346,  347,  348,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,  262,  263,  264,  265,  266,
  267,  268,  269,  270,  271,  272,  273,  274,  275,  276,
  277,  278,   -1,  280,  281,  282,  283,  284,  285,  286,
  287,  288,  289,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,  320,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,    0,   -1,  335,  336,
  337,  338,   -1,  340,  341,  342,   10,  344,  345,  346,
  347,  348,  349,  350,  351,  352,  353,   -1,   -1,   -1,
   -1,  358,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   33,
   -1,   -1,   -1,   37,   38,   -1,   -1,   41,   42,   43,
   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   93,
   94,   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,
  124,  125,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,
   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,   45,
   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   91,   92,   93,   94,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,  124,  125,
  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,  263,
  264,  265,  266,  267,  268,  269,  270,  271,  272,  273,
  274,  275,  276,  277,  278,   -1,  280,  281,  282,  283,
  284,  285,  286,  287,  288,  289,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,  303,
  304,  305,  306,  307,  308,  309,  310,  311,  312,   -1,
  314,  315,   -1,  317,  318,  319,  320,  321,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,   -1,
   -1,  335,  336,  337,  338,   -1,  340,  341,  342,   -1,
  344,  345,  346,  347,  348,  349,  350,  351,  352,  353,
   -1,  257,  258,  259,  358,  261,  262,  263,  264,  265,
  266,  267,  268,  269,  270,  271,  272,  273,  274,  275,
  276,  277,  278,   -1,  280,  281,  282,  283,  284,  285,
  286,  287,  288,  289,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,  299,   -1,   -1,  302,  303,  304,  305,
  306,  307,  308,  309,  310,  311,  312,   -1,  314,  315,
   -1,  317,  318,  319,  320,  321,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,    0,   -1,  335,
  336,  337,  338,   -1,  340,  341,  342,   10,  344,  345,
  346,  347,  348,  349,  350,  351,  352,  353,   -1,   -1,
   -1,   -1,  358,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  124,  125,  126,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,
   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   93,   94,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,
  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,  262,
  263,  264,  265,  266,  267,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,  281,  282,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
   -1,   -1,  335,  336,  337,  338,   -1,  340,  341,  342,
   -1,  344,  345,  346,  347,  348,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,  262,  263,  264,
  265,  266,  267,  268,  269,  270,  271,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,  281,  282,  283,  284,
  285,  286,  287,  288,  289,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,  320,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,    0,   -1,
  335,  336,  337,  338,   -1,  340,  341,  342,   10,  344,
  345,  346,  347,  348,  349,  350,  351,  352,  353,   -1,
   -1,   -1,   -1,  358,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,
   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,   61,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,
   92,   -1,   94,   -1,   -1,   -1,    0,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  123,  124,  125,  126,   -1,   -1,   -1,   -1,   33,
   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,   43,
   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   59,   60,   61,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,    0,
   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,
  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   -1,
   -1,   -1,   -1,   -1,   -1,  257,  258,  259,   -1,  261,
  262,  263,  264,  265,  266,   -1,  268,  269,  270,  271,
  272,  273,  274,  275,  276,  277,  278,   -1,  280,   -1,
   -1,  283,  284,  285,  286,  287,  288,  289,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,
  302,  303,  304,  305,  306,  307,  308,  309,  310,  311,
  312,   -1,  314,  315,  125,  317,  318,  319,  320,  321,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,   -1,   -1,  335,  336,  337,  338,   -1,   -1,  341,
  342,   -1,  344,  345,   -1,  347,  348,  349,  350,  351,
  352,  353,   -1,  257,  258,  259,  358,  261,  262,  263,
  264,  265,  266,   -1,  268,  269,  270,  271,  272,  273,
  274,  275,  276,  277,  278,   -1,  280,   -1,   -1,  283,
  284,  285,  286,  287,  288,  289,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,  303,
  304,  305,  306,  307,  308,  309,  310,  311,  312,   -1,
  314,  315,   -1,  317,  318,  319,  320,  321,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,    0,
   -1,  335,  336,  337,  338,   -1,   -1,  341,  342,   10,
  344,  345,   -1,  347,  348,  349,  350,  351,  352,  353,
   -1,  262,  263,  264,  358,   -1,   -1,  268,  269,   -1,
  271,   -1,   33,   -1,   -1,   -1,   37,   38,   -1,   40,
   41,   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,
   -1,   -1,  293,  294,  295,  296,  297,   -1,   59,   60,
   61,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   91,   92,   -1,   94,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  123,  124,  125,  126,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   37,   38,   -1,   40,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   59,   60,   61,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,
    0,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  124,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   41,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,
   -1,   -1,   -1,   44,   -1,   -1,  257,  258,  259,   -1,
  261,  262,  263,  264,  265,  266,   -1,  268,  269,  270,
  271,  272,  273,  274,  275,  276,  277,  278,   -1,  280,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,  125,  317,  318,  319,  320,
  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,   -1,   -1,  335,  336,  337,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,  347,  348,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,  262,
  263,  264,  265,  266,   -1,  268,  269,  270,  271,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
    0,   -1,  335,  336,  337,  338,   -1,   -1,  341,  342,
   10,  344,  345,   -1,  347,  348,  349,  350,  351,  352,
  353,   -1,  262,  263,  264,  358,   -1,   -1,  268,  269,
   -1,  271,   -1,   33,   -1,   -1,  257,  258,  259,   -1,
  261,   41,   -1,   -1,  265,  266,   46,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   58,   59,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,   -1,   -1,  319,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,  125,  126,   -1,  349,  350,
  351,  352,  353,   -1,   -1,   -1,   33,  358,   -1,   -1,
   -1,   -1,   -1,   40,   -1,   -1,   -1,   -1,   -1,   46,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   40,   41,   59,   60,   44,   -1,   46,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   61,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   91,   92,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   91,   92,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  123,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  123,  124,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  257,  258,  259,
   -1,  261,  262,  263,  264,  265,  266,  267,  268,  269,
  270,  271,  272,  273,  274,  275,  276,  277,  278,   -1,
   -1,  281,   -1,  283,  284,  285,  286,  287,  288,  289,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,  299,
   -1,   -1,  302,  303,  304,  305,  306,  307,  308,  309,
  310,  311,  312,   -1,  314,  315,   -1,  317,  318,  319,
   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  337,  338,   -1,
   -1,  341,  342,   -1,  344,  345,   -1,  347,  348,  349,
  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,  256,
  257,  258,  259,  260,  261,  262,  263,  264,  265,  266,
   -1,   -1,  269,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,  280,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   10,   -1,  292,   -1,   -1,   -1,  279,
  280,  298,  299,  300,  301,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   33,  314,  315,   -1,
  317,  318,  319,   40,   -1,   -1,   -1,   -1,   -1,   46,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  337,  338,   59,   60,  341,  342,   -1,  344,  345,   -1,
  347,   -1,  349,  350,  351,  352,  353,  337,   -1,   -1,
   -1,  358,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   91,   92,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  123,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   33,
   -1,   -1,   -1,   -1,   -1,   -1,   40,   -1,   -1,   -1,
   -1,   -1,   46,   -1,  257,  258,  259,   -1,  261,   -1,
   -1,   -1,  265,  266,   -1,   59,   60,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   91,   92,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,   -1,   -1,  319,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  123,
   -1,   -1,  126,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,   -1,   -1,  349,  350,  351,  352,
  353,   -1,   -1,   -1,   -1,  358,   -1,   -1,   -1,  256,
  257,  258,  259,  260,  261,  262,  263,  264,  265,  266,
   -1,   -1,  269,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,  280,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,
   -1,  298,  299,  300,  301,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  337,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,   -1,  349,  350,  351,  352,  353,   -1,   -1,   -1,
   -1,  358,  256,  257,  258,  259,  260,  261,  262,  263,
  264,  265,  266,   -1,   -1,  269,  270,   -1,  272,  273,
  274,  275,  276,  277,  278,   -1,  280,   -1,   -1,  283,
  284,  285,  286,  287,  288,  289,   10,   -1,  292,   -1,
   -1,   -1,   -1,   -1,  298,  299,  300,  301,  302,  303,
  304,  305,  306,  307,  308,  309,  310,  311,  312,   33,
  314,  315,   -1,  317,  318,  319,   40,   -1,   -1,   -1,
   -1,   -1,   46,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  337,  338,   59,   60,  341,  342,   -1,
  344,  345,   -1,  347,   -1,  349,  350,  351,  352,  353,
   -1,   -1,   -1,   -1,  358,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   46,   -1,  257,  258,  259,
   -1,  261,   -1,   -1,   -1,  265,  266,   -1,   59,   60,
  270,   -1,  272,  273,  274,  275,  276,  277,  278,   -1,
   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,  289,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,
   91,   92,  302,  303,  304,  305,  306,  307,  308,  309,
  310,  311,  312,   -1,  314,  315,   -1,   -1,   -1,  319,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  126,   -1,   -1,  338,   -1,
   -1,  341,  342,   -1,  344,  345,   -1,   33,   -1,  349,
  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,   -1,
   -1,   -1,  256,  257,  258,  259,  260,  261,  262,  263,
  264,  265,  266,   59,   -1,  269,  270,   -1,  272,  273,
  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,
  284,  285,  286,  287,  288,  289,   -1,   -1,  292,   -1,
   -1,   -1,   -1,   -1,  298,  299,  300,  301,  302,  303,
  304,  305,  306,  307,  308,  309,  310,  311,  312,   -1,
  314,  315,   -1,  317,  318,  319,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,
  126,   -1,   -1,  337,  338,   -1,   -1,  341,  342,   -1,
  344,  345,   -1,  347,   -1,  349,  350,  351,  352,  353,
   -1,   -1,   33,   -1,  358,  256,  257,  258,  259,  260,
  261,  262,  263,  264,  265,  266,   -1,   -1,  269,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   59,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,  292,   -1,   -1,   -1,   -1,   -1,  298,  299,  300,
  301,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  337,  338,   -1,   -1,
  341,  342,   -1,  344,  345,  126,  347,   -1,  349,  350,
  351,  352,  353,   -1,   -1,   -1,   -1,  358,   -1,   -1,
  256,  257,  258,  259,  260,  261,  262,  263,  264,  265,
  266,   10,  268,  269,  270,  271,  272,  273,  274,  275,
  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,
  286,  287,  288,  289,   33,   -1,  292,   -1,   -1,   -1,
   -1,   -1,  298,  299,  300,  301,  302,  303,  304,  305,
  306,  307,  308,  309,  310,  311,  312,   -1,  314,  315,
   59,  317,  318,  319,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,
   -1,  347,   -1,  349,  350,  351,  352,  353,   -1,   -1,
   -1,   -1,  358,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  256,  257,  258,  259,  260,
  261,   -1,   -1,  264,  265,  266,  125,  126,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,    0,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   10,
   -1,  292,   -1,   -1,   -1,   -1,   -1,  298,  299,  300,
  301,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   41,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,   59,   -1,
  341,  342,   -1,  344,  345,   -1,  347,   -1,  349,  350,
  351,  352,  353,   -1,   -1,   -1,   -1,  358,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,  256,  257,  258,
  259,  260,  261,   60,  125,   62,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,  292,   -1,   -1,   -1,   94,   -1,  298,
  299,  300,  301,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,    0,   -1,   -1,   -1,  124,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,   -1,
  349,  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,
   37,   38,   -1,   -1,   41,   42,   43,   44,   45,   46,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,   -1,  268,  269,   -1,
  271,   -1,    0,   -1,   91,   92,   93,   94,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  123,  124,  125,   37,
   38,   -1,   -1,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,   -1,   -1,  320,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,   -1,   -1,  124,  125,  335,  336,
   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   91,   92,
   93,   94,   -1,  280,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,
  123,  124,  125,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  320,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
  337,   -1,   -1,  340,   -1,   -1,   41,   -1,   -1,  346,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   59,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  320,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,   -1,   -1,  335,  336,  337,
  125,  339,  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  280,  281,  282,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
   -1,   -1,  335,  336,  337,   -1,   -1,  340,   -1,   -1,
   37,   38,   -1,  346,   41,   42,   43,   44,   45,   46,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   61,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,
   -1,   -1,   -1,  268,  269,   -1,  271,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   91,   92,   93,   94,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   37,   38,  124,  125,   41,
   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   61,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,
   92,   93,   94,   -1,    0,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,   45,
   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   61,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   91,   92,   93,   94,   -1,
   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,    0,   -1,   -1,  124,  125,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  320,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
  337,   -1,   -1,  340,   -1,   -1,   41,   -1,   -1,  346,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   -1,   58,   59,   -1,   -1,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  320,  321,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,   -1,   -1,  335,  336,  337,    0,   -1,  340,   -1,
  125,   -1,   -1,   -1,  346,   -1,   10,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  281,  282,   41,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   58,   59,   -1,   -1,   -1,   -1,
    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   10,   -1,   -1,   -1,  320,  321,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,   -1,   -1,  335,
  336,  337,   -1,   -1,  340,   -1,   -1,   37,   38,   -1,
  346,   41,   42,   43,   44,   45,   46,   47,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,
   60,  125,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,    0,  271,   -1,   -1,   -1,
   -1,   91,   92,   93,   94,   10,  281,  282,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   37,   38,  124,  125,   41,   42,   43,   44,
   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  346,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   91,   92,   93,   94,
   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,  124,
  125,   -1,   -1,   -1,   -1,   -1,   -1,  281,   37,   38,
   -1,   -1,   41,   42,   43,   44,   45,   46,   47,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   58,
   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   91,   92,   93,   94,   -1,   -1,   -1,   -1,
   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,    0,   -1,   -1,  124,  125,   -1,   -1,   -1,
   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,   -1,   -1,  335,  336,  337,   -1,   -1,
  340,   -1,   -1,   41,   -1,   -1,  346,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
   58,   59,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  320,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,   -1,   -1,
  335,  336,  337,   -1,   -1,  340,   -1,  125,   -1,   -1,
   -1,  346,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,
   -1,  320,  321,  322,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,   -1,   -1,  335,  336,  337,   -1,
   -1,  340,   -1,   -1,   37,   38,   -1,  346,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,
   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,    0,  271,   -1,   -1,   -1,   -1,   91,   92,
   93,   94,   10,  281,  282,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,
   38,  124,  125,   41,   42,   43,   44,   45,   46,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   91,   92,   93,   94,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   41,
   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   91,
   92,   93,   94,   -1,   -1,   -1,   -1,   -1,  281,  282,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,    0,   -1,
   -1,   -1,  124,  125,   -1,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
   -1,   -1,  335,  336,  337,   -1,   -1,  340,   -1,   41,
   -1,   -1,   44,  346,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   58,   59,   -1,   -1,
   -1,   63,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   93,    0,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   10,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,   -1,   -1,  335,  336,  337,
   -1,   -1,  340,  125,   -1,   -1,   -1,   -1,  346,   37,
   38,   -1,   -1,   41,   42,   43,   44,   45,   -1,   47,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,    0,   93,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,  321,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,   -1,   -1,  335,  336,  337,  124,  125,  340,   -1,
   -1,   37,   38,   -1,  346,   41,   42,   43,   44,   45,
   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   -1,    0,   -1,   -1,   -1,   93,   94,  281,
  282,   -1,   -1,   10,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,
   37,   38,   -1,   -1,   41,   42,   43,   44,   45,   -1,
   47,   -1,   -1,   -1,   -1,  327,  328,   -1,   -1,  331,
  332,   58,   59,   60,   -1,   62,   63,   -1,  340,   -1,
   -1,   -1,   -1,   -1,  346,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   93,   94,   -1,   -1,
   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  321,  322,  323,  324,  325,  326,  327,
  328,  329,  330,  331,  332,   -1,   -1,  335,  336,   -1,
   -1,   -1,  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  321,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,   -1,   -1,  335,
  336,   -1,   -1,   -1,  340,   -1,   -1,   -1,   -1,   -1,
  346,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
   37,   38,   -1,  340,   41,   42,   43,   44,   45,  346,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   93,   94,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   37,   38,  124,  125,   41,
   42,   43,   44,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   93,   94,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   41,   42,   43,   44,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   93,   94,   -1,   -1,
   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,    0,   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
   -1,   -1,   41,  340,   -1,   44,   -1,   -1,   -1,  346,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   59,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  321,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,   -1,   -1,  335,  336,   -1,  125,   -1,  340,   -1,
   -1,   -1,   -1,   -1,  346,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
   37,   38,   -1,  340,   41,   42,   43,   44,   45,  346,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  262,  263,  264,   -1,   -1,    0,  268,
  269,   -1,  271,   -1,   -1,   -1,   93,   94,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   38,  124,  125,   41,
   -1,   43,   44,   45,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,   -1,
   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   93,   94,    0,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   38,   -1,   -1,   41,   -1,   43,   44,   45,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   93,   94,   -1,   -1,
   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,   -1,   -1,  124,  125,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,    0,   -1,  335,  336,
   -1,   -1,   -1,  340,   -1,   -1,   10,   -1,   -1,  346,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,
  282,   -1,   -1,   -1,   38,   -1,   -1,   41,  290,  291,
   44,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  321,
  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,
  332,   -1,   -1,  335,  336,   -1,   -1,   -1,  340,   93,
   94,    0,   -1,   -1,  346,   -1,   -1,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,
  124,  125,   -1,   -1,  281,  282,   -1,   -1,   -1,   38,
   -1,   -1,   41,  290,  291,   44,  293,  294,  295,  296,
  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,    0,   -1,  335,  336,
   -1,   -1,   -1,  340,   93,   94,   10,   -1,   -1,  346,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   38,  124,  125,   41,   -1,   -1,
   44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   93,
   94,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
  124,  125,   -1,   -1,   -1,    0,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   10,   -1,  321,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,   -1,
   -1,  335,  336,   -1,   -1,   -1,  340,   -1,   -1,   -1,
   -1,   -1,  346,   -1,   -1,   -1,   41,   -1,   -1,   44,
   -1,   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   94,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  321,  322,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,    0,   -1,  335,  336,   -1,  124,
  125,  340,   -1,   -1,   10,   -1,   -1,  346,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,
   -1,   -1,   -1,   -1,   -1,   41,  290,  291,   44,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  321,  322,  323,
  324,  325,  326,  327,  328,  329,  330,  331,  332,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  340,   93,   94,    0,
   -1,   -1,  346,   -1,   -1,   -1,   -1,   -1,   -1,   10,
   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,
   43,   -1,   45,   -1,   47,   -1,   -1,   -1,  124,  125,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   62,
   41,   -1,   -1,   44,   -1,   -1,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   58,   59,   60,
   -1,   62,   63,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   94,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,    0,   -1,   -1,   -1,   -1,
   -1,   -1,   93,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,  124,   -1,   -1,   -1,   -1,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,   -1,   -1,
   -1,   -1,   -1,   -1,  125,  340,   41,   -1,   -1,   44,
   -1,  346,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   59,   60,   -1,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   93,    0,
   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   10,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   41,   -1,   -1,   44,   -1,  321,  322,  323,  324,  325,
  326,  327,  328,  329,  330,  331,  332,   58,   59,   60,
   -1,   62,   63,   -1,  340,   -1,   -1,   -1,   -1,   -1,
  346,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   93,   -1,   -1,    0,   -1,   -1,   -1,   -1,
  281,  282,   -1,   -1,   -1,   10,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,  320,  321,  322,
  323,  324,  325,  326,  125,   -1,  329,  330,   -1,   -1,
   -1,   -1,  335,  336,   -1,   -1,   41,   -1,   -1,   44,
  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,   -1,   58,   59,   -1,   -1,   -1,   63,  340,
   -1,   -1,   -1,   -1,   -1,  346,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
    0,   -1,   -1,   -1,   -1,   -1,  281,  282,   93,   -1,
   10,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  125,   41,   -1,   -1,   44,   -1,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,   58,   59,
   -1,   -1,   -1,   63,   -1,  340,   -1,   -1,   -1,   -1,
   -1,  346,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   -1,   93,    0,   -1,   -1,   -1,   -1,   -1,
  281,  282,   -1,   -1,   10,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,
  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,   58,   59,   -1,   -1,   -1,   63,   -1,  340,
   -1,   -1,   -1,   -1,   -1,  346,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,
   -1,    0,  267,  268,  269,   -1,  271,   93,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,  125,
   -1,   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   -1,  327,  328,   63,   -1,  331,  332,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,   -1,   -1,
   -1,  346,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   93,    0,   -1,   -1,   -1,   -1,
   -1,  281,  282,   -1,   -1,   10,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  327,  328,   -1,
   -1,  331,  332,   58,   59,   -1,   -1,   -1,   63,   -1,
  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,
    0,  267,  268,  269,   -1,  271,   -1,   -1,   93,   -1,
   10,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  125,   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,
   -1,  327,  328,   63,   -1,  331,  332,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  340,   -1,   -1,   -1,   -1,   -1,
  346,   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   93,   -1,   -1,    0,   -1,   -1,   -1,
   -1,   -1,  281,  282,   -1,   -1,   10,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,
   44,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  327,  328,
   -1,   -1,  331,  332,   58,   59,   -1,   -1,   -1,   63,
   -1,  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,  263,  264,
   -1,    0,  267,  268,  269,   -1,  271,   -1,   -1,   93,
   -1,   10,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  125,   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   -1,  327,  328,   63,   -1,  331,  332,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,   -1,   -1,
   -1,  346,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   93,    0,   -1,   -1,   -1,   -1,
   -1,  281,  282,   -1,   -1,   10,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,
   -1,   -1,    0,   -1,   -1,   -1,   41,   -1,   -1,   44,
   -1,   -1,   10,   -1,   -1,   -1,   -1,  327,  328,   -1,
   -1,  331,  332,   58,   59,   -1,   -1,   -1,   -1,   -1,
  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,   -1,   -1,
   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   93,   -1,
   58,   59,    0,   -1,   -1,   -1,   -1,  281,  282,   -1,
   -1,   -1,   10,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
  125,   -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,   -1,
    0,   -1,   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,
   10,   -1,   -1,   -1,  328,   -1,   -1,  331,  332,   -1,
   58,   59,   -1,   -1,   -1,   -1,  340,  125,   -1,   -1,
   -1,   -1,  346,  262,  263,  264,   -1,   -1,  267,  268,
  269,   41,  271,   -1,   44,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  281,  282,   -1,   93,   -1,   -1,   58,   59,
    0,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,
   -1,   -1,   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,
   -1,  340,   -1,   -1,   -1,   -1,   -1,  346,   58,   59,
   -1,   -1,   -1,   -1,   -1,  125,   -1,  262,  263,  264,
   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,
   -1,   -1,   -1,   93,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   -1,   -1,   -1,    0,
   -1,   -1,   -1,  281,  282,  125,   -1,   -1,   -1,   10,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,   -1,   -1,   -1,  340,   -1,   -1,   -1,   -1,
   -1,  346,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   41,   -1,   -1,   44,  262,  263,  264,   -1,   -1,  267,
  268,  269,   -1,  271,   -1,   -1,   -1,   58,   59,   -1,
   -1,   -1,  340,  281,  282,   -1,   -1,   -1,  346,   -1,
   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,  297,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   93,   -1,    0,   -1,   -1,   -1,   -1,   -1,
   -1,  281,  282,   -1,   10,   -1,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,  340,   -1,  125,   -1,   -1,   -1,  346,   -1,
   -1,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   58,   59,    0,   -1,   -1,   -1,   -1,   -1,
  340,  281,  282,   -1,   10,   -1,  346,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,
    0,   -1,   -1,   -1,   -1,   41,   -1,   -1,   44,   -1,
   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   59,   -1,   -1,   -1,   -1,   -1,  125,
  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,    0,   -1,
   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,   58,   59,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   41,
  271,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,  125,
  281,  282,   -1,   93,   -1,   -1,   58,   59,    0,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  125,   -1,   -1,   -1,   -1,
   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,
   -1,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,  340,
   -1,   -1,   -1,   -1,   -1,  346,   58,   59,   -1,   -1,
   -1,   -1,   -1,  125,   -1,   -1,  262,  263,  264,   -1,
   -1,  267,  268,  269,   -1,  271,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  281,  282,   -1,   -1,   -1,
   -1,   93,   -1,    0,  290,  291,   -1,  293,  294,  295,
  296,  297,   -1,   10,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  125,   -1,   -1,  262,  263,  264,    0,
   -1,  267,  268,  269,   41,  271,   -1,   44,   -1,   10,
   -1,   -1,   -1,   -1,  340,  281,  282,   -1,   -1,   -1,
  346,   58,   59,   -1,  290,  291,   -1,  293,  294,  295,
  296,  297,  262,  263,  264,   -1,   -1,  267,  268,  269,
   41,  271,   -1,   44,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  281,  282,   -1,   -1,   -1,   93,   58,   59,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
  262,  263,  264,   -1,  340,  267,  268,  269,   -1,  271,
  346,   -1,   -1,   -1,   -1,   -1,    0,   -1,  125,  281,
  282,   -1,   93,   -1,   -1,   -1,   10,   -1,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
  340,   -1,   -1,   -1,   -1,   -1,  346,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  125,   -1,   -1,   41,   -1,   -1,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,   -1,   -1,   -1,   58,   59,   -1,    0,  340,  281,
  282,   -1,   -1,   -1,  346,   -1,   -1,   10,  290,  291,
   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   93,
   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   41,   42,
   43,   44,   45,   46,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   59,   60,  340,   62,
   63,  125,   -1,   -1,  346,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  262,  263,  264,   -1,   -1,
  267,  268,  269,   -1,  271,   -1,   -1,   -1,   91,   92,
   -1,   94,   -1,   -1,  281,  282,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  290,  291,   -1,  293,  294,  295,  296,
  297,  262,  263,  264,   -1,   -1,  267,  268,  269,    0,
  271,  124,  125,   -1,   -1,   -1,   -1,   -1,   -1,   10,
  281,  282,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,  340,   -1,   -1,   37,   38,   -1,  346,
   41,   42,   43,   44,   45,   46,   47,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   60,
   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  346,   -1,   -1,  262,  263,
  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,   -1,
   91,   92,   -1,   94,   -1,   -1,   -1,  281,  282,   -1,
   -1,   10,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,
  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  124,  125,   -1,   -1,   -1,   37,   38,
   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,
   -1,   60,   -1,   62,   63,   -1,   -1,   -1,  281,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   -1,   -1,
   -1,    0,   -1,   -1,   -1,   94,   -1,   -1,   -1,   -1,
   -1,   10,   -1,   -1,   -1,   -1,   -1,  320,  321,  322,
  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,
   -1,   -1,  335,  336,  337,  124,   -1,  340,   37,   38,
   -1,   -1,   41,   42,   43,   44,   45,   46,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   59,   60,   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,
  271,   -1,   91,   92,   -1,   94,   -1,   -1,   -1,   -1,
  281,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
    0,   -1,   -1,   -1,   -1,  124,  125,   -1,   -1,   -1,
   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  320,
  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,   -1,   -1,  335,  336,  337,   37,   38,  340,
   -1,   41,   42,   43,   44,   45,   46,   47,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,
   60,   61,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   91,   92,   -1,   94,    0,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,  320,  321,  322,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,  124,  125,  335,  336,   -1,   -1,
   -1,   -1,   37,   38,   -1,   -1,   41,   42,   43,   44,
   45,   46,   47,  262,  263,  264,   -1,   -1,  267,  268,
  269,   -1,  271,   -1,   59,   60,   61,   62,   63,   -1,
   -1,   -1,  281,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  290,  291,   -1,  293,  294,  295,  296,  297,   -1,
   -1,   -1,   -1,   -1,   -1,    0,   91,   92,   -1,   94,
   -1,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   -1,
   -1,  320,  321,  322,  323,  324,  325,  326,  327,  328,
  329,  330,  331,  332,   -1,   -1,  335,  336,  337,  124,
  125,  340,   37,   38,   -1,   -1,   41,   42,   43,   44,
   45,   46,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   59,   60,   61,   62,   63,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,   -1,  268,  269,
   -1,  271,   -1,   -1,   -1,   -1,   91,   92,   -1,   94,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,  124,
  125,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,   -1,   -1,  335,  336,  337,   -1,  339,
   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,
   -1,   42,   43,   -1,   45,   -1,   47,  262,  263,  264,
   -1,   -1,   -1,  268,  269,   -1,  271,   -1,   -1,   60,
   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,  126,   -1,   -1,   -1,    0,
   -1,   -1,   -1,   94,   -1,   -1,   -1,   -1,   -1,   10,
   -1,   -1,   -1,   -1,   -1,  320,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,   -1,   -1,
  335,  336,  337,  124,   -1,   -1,   37,   38,   -1,   -1,
   41,   42,   43,   44,   45,   46,   47,  262,  263,  264,
   -1,   -1,   -1,  268,  269,   -1,  271,   -1,   59,   60,
   61,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,  293,  294,
  295,  296,  297,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   91,   92,   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  320,  321,  322,  323,  324,
  325,  326,  327,  328,  329,  330,  331,  332,   -1,   33,
  335,  336,  337,  124,  125,  256,  257,  258,  259,  260,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,  292,   -1,   -1,   -1,   -1,   -1,  298,  299,  300,
  301,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  297,  338,   -1,   -1,
  341,  342,  126,  344,  345,   -1,  347,   -1,  349,  350,
  351,  352,  353,   33,   -1,   -1,   -1,  358,   -1,  320,
  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,   -1,   -1,  335,  336,   -1,   -1,   -1,   59,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  262,  263,  264,   -1,   -1,   -1,  268,  269,   -1,
  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,   -1,  320,
  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,
  331,  332,   33,   -1,  335,  336,  337,   -1,   -1,   -1,
   -1,   -1,  256,  257,  258,  259,  260,  261,   -1,   -1,
   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,
  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,
  284,  285,  286,  287,  288,  289,   -1,   -1,  292,   -1,
   -1,   -1,   -1,   -1,  298,  299,  300,  301,  302,  303,
  304,  305,  306,  307,  308,  309,  310,  311,  312,   -1,
  314,  315,   -1,  317,  318,  319,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  338,  126,   -1,  341,  342,   -1,
  344,  345,   -1,  347,   33,  349,  350,  351,  352,  353,
   -1,   -1,   41,   -1,  358,   -1,   -1,  257,  258,  259,
  260,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,
  270,   -1,  272,  273,  274,  275,  276,  277,  278,   -1,
   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,  289,
   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,  298,  299,
  300,  301,  302,  303,  304,  305,  306,  307,  308,  309,
  310,  311,  312,   -1,  314,  315,   -1,  317,  318,  319,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,  338,   -1,
   -1,  341,  342,   -1,  344,  345,   33,  347,   -1,  349,
  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  257,  258,  259,  260,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,  292,   -1,   -1,   -1,   -1,   -1,  298,  299,  300,
  301,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   33,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,  347,   -1,  349,  350,
  351,  352,  353,   -1,   -1,   -1,   -1,  358,  257,  258,
  259,   -1,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,   -1,   -1,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,  348,
  349,  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,
  257,  258,  259,   -1,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,  348,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
  269,  270,  271,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,  292,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,   -1,   -1,
  349,  350,  351,  352,  353,   -1,  257,  258,  259,  358,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,  347,  348,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,  347,  348,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,
  345,   -1,  347,  348,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,  348,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,  348,
  349,  350,  351,  352,  353,   -1,  257,  258,  259,  358,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,  347,  348,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,  347,  348,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,
  345,   -1,  347,  348,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,  348,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,  348,
  349,  350,  351,  352,  353,   -1,  257,  258,  259,  358,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,  347,  348,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,  347,  348,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,  292,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,
  345,   -1,   -1,   -1,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,   -1,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,   -1,
  349,  350,  351,  352,  353,   -1,  257,  258,  259,  358,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,  347,   -1,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,  347,   -1,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   33,   -1,   -1,   -1,   -1,   -1,   -1,   40,
   -1,   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,
  345,   -1,  347,   -1,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,
   33,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,   -1,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,  317,  318,
  319,   -1,   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,   -1,
  349,  350,  351,  352,  353,   -1,  257,  258,  259,  358,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,  317,  318,  319,   -1,
   -1,  126,   -1,   -1,   -1,   -1,   33,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,   -1,   -1,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,  317,  318,  319,   -1,   -1,  126,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,   -1,   -1,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,  317,  318,  319,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,
  345,   -1,   -1,   -1,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
  317,  318,  319,   -1,   -1,   -1,   37,   38,   -1,   -1,
   -1,   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   60,
   -1,   62,  349,  350,  351,  352,  353,   -1,   -1,   -1,
   -1,  358,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   94,   -1,   96,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  124,   -1,  126,   -1,   -1,   -1,   37,
   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   60,   -1,   62,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,
   -1,   -1,   42,   43,   -1,   45,   94,   47,   96,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   -1,
   60,   -1,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   94,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  257,  258,  259,  260,
  261,  262,  263,  264,  124,   -1,  267,  268,  269,  270,
  271,   -1,   -1,  274,  275,  276,  277,  278,  279,  280,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,  290,
  291,  292,  293,  294,  295,  296,  297,  298,  299,  300,
  301,  302,  303,  304,  305,  306,   -1,  308,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  317,  318,   -1,  320,
  321,  322,  323,   -1,  325,  326,   -1,   -1,  329,   -1,
   -1,   -1,  333,  334,  335,  336,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  347,   -1,  349,  257,
  258,  259,  260,  261,  262,  263,  264,   -1,   -1,  267,
  268,  269,  270,  271,   -1,   -1,  274,  275,  276,  277,
  278,  279,  280,   -1,   -1,  283,  284,  285,  286,  287,
  288,  289,  290,  291,  292,  293,  294,  295,  296,  297,
  298,  299,  300,  301,  302,  303,  304,  305,   -1,   -1,
  308,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  317,
  318,   -1,  320,  321,  322,  323,   -1,  325,  326,   -1,
   -1,  329,   -1,   -1,   -1,  333,  334,  335,  336,   -1,
   37,   38,   -1,   40,   -1,   42,   43,   -1,   45,  347,
   47,  349,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   60,   -1,   62,   -1,   -1,   -1,   -1,
  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,   -1,   -1,  335,  336,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   94,   -1,   96,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,
   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,
   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   60,   -1,   62,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   60,   -1,   62,   -1,   -1,   94,   -1,
   96,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   94,  124,   -1,
  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  257,  258,  259,  260,  261,  262,  263,  264,   -1,   -1,
  267,  268,  269,  270,  271,   -1,   -1,  274,  275,  276,
  277,  278,  279,  280,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,  290,  291,  292,  293,  294,  295,  296,
  297,  298,  299,  300,  301,  302,  303,  304,  305,  306,
  307,  308,  309,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  317,  318,   -1,  320,  321,  322,  323,   -1,  325,  326,
   -1,   -1,  329,   -1,   -1,   -1,  333,  334,  335,  336,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  347,  257,  258,  259,  260,  261,  262,  263,  264,   -1,
   -1,  267,  268,  269,  270,  271,   -1,   -1,  274,  275,
  276,  277,  278,  279,  280,   -1,   -1,  283,  284,  285,
  286,  287,  288,  289,  290,  291,  292,  293,  294,  295,
  296,  297,  298,  299,  300,  301,  302,  303,  304,  305,
  306,  307,  308,  309,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  317,  318,   -1,  320,  321,  322,  323,   -1,  325,
  326,   -1,   -1,  329,   -1,   -1,   -1,  333,  334,  335,
  336,   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,
   45,  347,   47,  320,  321,  322,  323,  324,  325,  326,
  327,   -1,  329,  330,   -1,   60,   -1,   62,  335,  336,
   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,
   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   60,   94,
   62,   96,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,
   -1,  126,   94,   60,   96,   62,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,   -1,  126,   -1,   60,   94,   62,   96,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,
   94,   -1,   96,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  124,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  257,  258,  259,  260,  261,  262,  263,  264,
   -1,   -1,  267,  268,  269,  270,  271,   -1,   -1,  274,
  275,  276,  277,  278,  279,  280,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,  290,  291,  292,  293,  294,
  295,  296,  297,  298,  299,  300,  301,  302,  303,  304,
  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  317,  318,   -1,  320,  321,  322,  323,   -1,
  325,  326,   -1,   -1,  329,   -1,   -1,   -1,  333,  334,
  335,  336,  304,  305,   -1,   -1,  308,   -1,   -1,   -1,
   -1,   -1,  347,   -1,   -1,  317,  318,   -1,  320,  321,
  322,  323,   -1,  325,  326,   -1,   -1,  329,   -1,   -1,
   -1,  333,  334,  335,  336,   -1,   -1,  304,  305,   -1,
   -1,  308,   -1,   -1,   -1,  347,   -1,   -1,   -1,   -1,
  317,  318,   -1,  320,  321,  322,  323,   -1,  325,  326,
   -1,   -1,  329,   -1,   -1,   -1,  333,  334,  335,  336,
  304,  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,   -1,
  347,   -1,   -1,  317,  318,   -1,  320,  321,  322,  323,
   -1,  325,  326,   -1,   -1,  329,   -1,   -1,   -1,  333,
  334,  335,  336,   -1,   37,   38,   -1,   -1,   -1,   42,
   43,   -1,   45,  347,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   60,   -1,   62,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,
   -1,   -1,   42,   43,   -1,   45,   -1,   47,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   60,   94,   62,   96,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  124,   -1,  126,   94,   60,   96,   62,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,
   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  124,   -1,  126,   -1,   60,   94,
   62,   96,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,
   -1,  126,   94,   60,   96,   62,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  124,   -1,  126,   -1,   60,   94,   62,   96,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,
   -1,   -1,   -1,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,  126,
   94,   60,   96,   62,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  124,   -1,  126,   -1,   -1,   94,   -1,   96,   -1,   -1,
   -1,  304,  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  317,  318,   -1,  320,  321,  322,
  323,   -1,  325,  326,   -1,  124,  329,  126,   -1,   -1,
  333,  334,  335,  336,  304,  305,   -1,   -1,  308,   -1,
   -1,   -1,   -1,   -1,  347,   -1,   -1,  317,  318,   -1,
  320,  321,  322,  323,   -1,  325,  326,   -1,   -1,  329,
   -1,   -1,   -1,  333,  334,  335,  336,   -1,   -1,  304,
  305,   -1,   -1,  308,   -1,   -1,   -1,  347,   -1,   -1,
   -1,   -1,  317,  318,   -1,  320,  321,  322,  323,   -1,
  325,  326,   -1,   -1,  329,   -1,   -1,   -1,  333,  334,
  335,  336,  304,  305,   -1,   -1,  308,   -1,   -1,   -1,
   -1,   -1,  347,   -1,   -1,  317,  318,   -1,  320,  321,
  322,  323,   -1,  325,  326,   -1,   -1,  329,   -1,   -1,
   -1,  333,  334,  335,  336,   -1,   -1,  304,  305,   -1,
   -1,  308,   -1,   -1,   -1,  347,   -1,   -1,   -1,   -1,
  317,  318,   -1,  320,  321,  322,  323,   -1,  325,  326,
   -1,   -1,  329,   -1,   -1,   -1,  333,  334,  335,  336,
  304,  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,   -1,
  347,   -1,   -1,  317,  318,   -1,  320,  321,  322,  323,
   -1,  325,  326,   -1,   -1,  329,   -1,    0,   -1,  333,
  334,  335,  336,   -1,   -1,  304,  305,   10,   -1,  308,
   -1,   -1,   -1,  347,   -1,   -1,   -1,   -1,  317,  318,
   -1,  320,  321,  322,  323,   -1,  325,  326,   -1,   -1,
  329,   -1,   -1,   -1,  333,  334,  335,  336,   41,   37,
   38,   -1,   -1,   -1,   42,   43,   -1,   45,  347,   47,
   -1,   -1,   -1,   -1,   -1,   58,   59,   -1,   -1,   -1,
   -1,   -1,   60,   -1,   62,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   37,   38,   -1,   -1,   -1,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   93,   -1,   -1,   -1,   -1,   60,   94,   62,   96,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   37,   38,   -1,
   -1,   -1,   42,   43,   44,   45,   -1,   47,   -1,   -1,
   -1,   -1,  125,   -1,   -1,   -1,  124,   -1,  126,   94,
   60,   96,   62,   63,   -1,   -1,   -1,   -1,   -1,   -1,
   37,   38,   -1,   -1,   -1,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,
   -1,  126,   -1,   60,   94,   62,   63,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,    0,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,    0,
   -1,   -1,   -1,   -1,  124,   -1,   -1,   94,   -1,   10,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  124,   -1,   -1,
   41,   -1,   -1,   -1,   -1,   -1,   58,   59,   -1,   -1,
    0,   -1,   -1,   -1,   -1,   -1,   -1,   58,   59,   -1,
   10,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  262,
  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  281,  282,
   -1,   41,   -1,   -1,   -1,   -1,   -1,  290,  291,   -1,
  293,  294,  295,  296,  297,   -1,   -1,   -1,   58,   59,
   -1,   -1,   -1,  125,   -1,   -1,  304,  305,   -1,   -1,
  308,   -1,   -1,   -1,  125,   -1,   -1,   -1,   -1,  317,
  318,   -1,  320,  321,  322,  323,   -1,  325,  326,   -1,
   -1,  329,   -1,   -1,   -1,  333,  334,  335,  336,  304,
  305,   -1,   -1,  308,   -1,   -1,   -1,   -1,   -1,  347,
   -1,   -1,  317,  318,   -1,  320,  321,  322,  323,   -1,
  325,  326,   -1,   -1,  329,  125,   -1,   -1,  333,  334,
  335,  336,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  347,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,
  330,  331,  332,   -1,   -1,  335,  336,  124,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  320,  321,  322,  323,  324,  325,  326,
  327,  328,  329,  330,  331,  332,   -1,   -1,  335,  336,
  262,  263,  264,   -1,   -1,  267,  268,  269,   -1,  271,
   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,  281,
  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  290,  291,
  281,  293,  294,  295,  296,  297,   -1,   -1,   -1,  290,
  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  262,  263,  264,   -1,   -1,  267,  268,  269,
   -1,  271,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  281,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  290,  291,   -1,  293,  294,  295,  296,  297,   -1,   -1,
  257,  258,  259,   -1,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
   -1,   -1,  319,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
  347,  348,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,   -1,   -1,
  319,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,  347,  348,
  349,  350,  351,  352,  353,   -1,  257,  258,  259,  358,
  261,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   -1,  272,  273,  274,  275,  276,  277,  278,   -1,   -1,
   -1,   -1,  283,  284,  285,  286,  287,  288,  289,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,
   -1,  302,  303,  304,  305,  306,  307,  308,  309,  310,
  311,  312,   -1,  314,  315,   -1,   -1,   -1,  319,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  335,   -1,   -1,  338,   -1,   -1,
  341,  342,   -1,  344,  345,   -1,   -1,   -1,  349,  350,
  351,  352,  353,   -1,  257,  258,  259,  358,  261,   -1,
   -1,   -1,  265,  266,   -1,   -1,   -1,  270,   -1,  272,
  273,  274,  275,  276,  277,  278,   -1,   -1,   -1,   -1,
  283,  284,  285,  286,  287,  288,  289,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,
  303,  304,  305,  306,  307,  308,  309,  310,  311,  312,
   -1,  314,  315,   -1,   -1,   -1,  319,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  338,   -1,   -1,  341,  342,
   -1,  344,  345,   -1,  347,   -1,  349,  350,  351,  352,
  353,   -1,  257,  258,  259,  358,  261,   -1,   -1,   -1,
  265,  266,   -1,   -1,   -1,  270,   -1,  272,  273,  274,
  275,  276,  277,  278,   -1,   -1,   -1,   -1,  283,  284,
  285,  286,  287,  288,  289,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  299,   -1,   -1,  302,  303,  304,
  305,  306,  307,  308,  309,  310,  311,  312,   -1,  314,
  315,   -1,   -1,   -1,  319,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  338,   -1,   -1,  341,  342,   -1,  344,
  345,   -1,  347,   -1,  349,  350,  351,  352,  353,   -1,
  257,  258,  259,  358,  261,   -1,   -1,   -1,  265,  266,
   -1,   -1,   -1,  270,   -1,  272,  273,  274,  275,  276,
  277,  278,   -1,   -1,   -1,   -1,  283,  284,  285,  286,
  287,  288,  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  299,   -1,   -1,  302,  303,  304,  305,  306,
  307,  308,  309,  310,  311,  312,   -1,  314,  315,   -1,
   -1,   -1,  319,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  338,   -1,   -1,  341,  342,   -1,  344,  345,   -1,
   -1,   -1,  349,  350,  351,  352,  353,   -1,  257,  258,
  259,  358,  261,   -1,   -1,   -1,  265,  266,   -1,   -1,
   -1,  270,   -1,  272,  273,  274,  275,  276,  277,  278,
   -1,   -1,   -1,   -1,  283,  284,  285,  286,  287,  288,
  289,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  299,   -1,   -1,  302,  303,  304,  305,  306,  307,  308,
  309,  310,  311,  312,   -1,  314,  315,   -1,   -1,   -1,
  319,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  338,
   -1,   -1,  341,  342,   -1,  344,  345,   -1,   -1,   -1,
  349,  350,  351,  352,  353,   -1,   -1,   -1,   -1,  358,
};
enum { YYTABLESIZE = 19890 };
enum { YYFINAL = 1 };
#ifndef YYDEBUG
#define YYDEBUG 1
#endif
enum { YYMAXTOKEN = 359 };
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
"tGVAR","tIVAR","tCONSTANT","tCVAR","tXSTRING_BEG","tINTEGER","tFLOAT",
"tSTRING_CONTENT","tNTH_REF","tBACK_REF","tREGEXP_END","tUPLUS","tUMINUS",
"tUBS","tPOW","tCMP","tEQ","tEQQ","tNEQ","tGEQ","tLEQ","tANDOP","tOROP",
"tMATCH","tNMATCH","tDOT2","tDOT3","tAREF","tASET","tLSHFT","tRSHFT","tCOLON2",
"tCOLON3","tOP_ASGN","tASSOC","tLPAREN","tLPAREN_ARG","tRPAREN","tLBRACK",
"tLBRACE","tLBRACE_ARG","tSTAR","tAMPER","tSYMBEG","tSTRING_BEG","tREGEXP_BEG",
"tWORDS_BEG","tQWORDS_BEG","tSTRING_DBEG","tSTRING_DVAR","tSTRING_END",
"tLOWEST","tUMINUS_NUM","tLAST_TOKEN",
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
  lenBASE = 525, 
  defredBASE = 1050, 
  dgotoBASE = 1970, 
  sindexBASE = 2122, 
  rindexBASE = 3042, 
  gindexBASE = 3962, 
  tableBASE = 4114, 
  checkBASE = 24005, 
  unifiedSIZE = 43896 
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

/* # line 3227 "grammar.y" */ 


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
        c &= 0xff;
        SET_lexState( EXPR_END);
        *ps->lexvalH = int64ToSi( (intptr_t)c);
        return tINTEGER;

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

/* # line 8939 "rubygrammar.c" */ 

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
/* # line 656 "grammar.y" */ 
	{
                        yTrace(vps,  "program: " );
                        vps->lex_state = EXPR_BEG;
                        vps->variables = LocalState::allocate(vps);
                        vps->class_nest = 0;
                    }
break;
case 2:
/* # line 663 "grammar.y" */ 
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
/* # line 677 "grammar.y" */ 
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
/* # line 698 "grammar.y" */ 
	{
                        /* void_stmts($1, vps);*/
                      yTrace(vps, "comp_stamt: sttmts opt_termms");
                        yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 6:
/* # line 707 "grammar.y" */ 
	{
                        /* $$  =  newline_node(vps, $1);*/
                        yyvalO = yymarkPtr[0].obj; /* maglev does not use newline nodes*/
                    }
break;
case 7:
/* # line 712 "grammar.y" */ 
	{
                        /* $$  =  block_append(vps, $1, newline_node(vps, $3));*/
                        yTrace(vps, "sttmts: | sttmts terms stmt ");
                        yyvalO = RubyParser::block_append(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 8:
/* # line 718 "grammar.y" */ 
	{
                        /* $$  = remove_begin($2, vps);*/
                      yTrace(vps, "sttmts: | error stmt");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 9:
/* # line 725 "grammar.y" */ 
	{vps->lex_state = EXPR_FNAME;}
break;
case 10:
/* # line 726 "grammar.y" */ 
	{
                        /* $$  = NEW_ALIAS($2, $4);*/
                      yTrace(vps, "stmt: kALIAS fitem");
                      yTrace(vps, "stmt: fitem");
                        yyvalO = RubyAliasNode::s(& yymarkPtr[-2].obj, & yymarkPtr[0].obj, yymarkPtr[-3].obj/*alias token*/, vps);
                    }
break;
case 11:
/* # line 733 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | kALIAS tGVAR tGVAR");
                        OmScopeType aScope(vps->omPtr);
                        NODE **aH = aScope.add( quidToSymbolObj(yymarkPtr[-1].obj, vps));
                        NODE **bH = aScope.add( quidToSymbolObj(yymarkPtr[0].obj, vps));
                        yyvalO = RubyGlobalVarAliasNode::s(*aH, *bH, vps);
                    }
break;
case 12:
/* # line 741 "grammar.y" */ 
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
/* # line 758 "grammar.y" */ 
	{
                        rb_compile_error(vps, "can't make alias for the number variables");
                        yyvalO = 0;
                    }
break;
case 14:
/* # line 763 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | kUNDEF undef_list");
                        yyvalO = yymarkPtr[0].obj;
                    }
break;
case 15:
/* # line 768 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kIF_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_if( yymarkPtr[0].obj , yymarkPtr[-2].obj, ram_OOP_NIL, srcOfs, vps );
                    }
break;
case 16:
/* # line 774 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kWHILE_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_if( yymarkPtr[0].obj, ram_OOP_NIL, yymarkPtr[-2].obj , srcOfs, vps );
                    }
break;
case 17:
/* # line 780 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kWHILE_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_while( yymarkPtr[-2].obj , yymarkPtr[0].obj , srcOfs, vps);
                    }
break;
case 18:
/* # line 786 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | stmt kUNTIL_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::new_until( yymarkPtr[-2].obj , yymarkPtr[0].obj , srcOfs, vps);
                    }
break;
case 19:
/* # line 792 "grammar.y" */ 
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
/* # line 801 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | klBEGIN");
                        if (vps->in_def || vps->in_single) {
                            rb_compile_error(vps, "BEGIN in method");
                        }
                        local_push(vps, 0);
                    }
break;
case 21:
/* # line 809 "grammar.y" */ 
	{
                       /* ruby_eval_tree_begin = block_append(ruby_eval_tree_begin, NEW_PREEXE($4));*/
                       yTrace(vps, "stmt: ___ tLCURLY comp_stamt tRCURLY");
                       rParenLexPop(vps);
                       local_pop(vps);
                       yyvalO = ram_OOP_NIL ;
                    }
break;
case 22:
/* # line 817 "grammar.y" */ 
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
/* # line 827 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | lhs tEQL command_call");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                      yyvalO = RubyParser::node_assign(& yymarkPtr[-2].obj, srcOfs, yymarkPtr[0].obj, vps);
                    }
break;
case 24:
/* # line 833 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | mLhs tEQL command_call");
                        yyvalO = RubyParser::masgn_append_arg( yymarkPtr[-2].obj , yymarkPtr[0].obj, vps );
                    }
break;
case 25:
/* # line 838 "grammar.y" */ 
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
/* # line 848 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | primary_value tLBRACK_STR aref__args tRBRACK tOP_ASGN command_call");
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[-2].obj, 0);
                      yyvalO = RubyOpElementAsgnNode::s(yymarkPtr[-3].obj, aref_args, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 27:
/* # line 854 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | primary_value tDOT tIDENTIFIER tOP_ASGN command_call");
                      /* not seen with Ryan's grammar and 1.8.7*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 28:
/* # line 860 "grammar.y" */ 
	{   
                      yTrace(vps, "stmt: | primary_value tDOT tCONSTANT tOP_ASGN command_call");
                      /* not seen with Ryan's grammar and 1.8.7*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 29:
/* # line 866 "grammar.y" */ 
	{
                      yTrace(vps, "stmt: | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call");
                      /* not seen with Ryan's grammar and 1.8.7*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 30:
/* # line 872 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | backref tOP_ASGN command_call");
                        rb_backref_error(yymarkPtr[-2].obj, vps);
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 31:
/* # line 878 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | lhs tEQL mrhs");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubySValueNode::s(yymarkPtr[0].obj, vps));
                        omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-1].obj);
                        yyvalO = RubyParser::node_assign(& yymarkPtr[-2].obj, srcOfs, *valH, vps);
                    }
break;
case 32:
/* # line 886 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | mLhs tEQL arg_value");
                        yyvalO = RubyParser::masgn_append_arg(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 33:
/* # line 891 "grammar.y" */ 
	{
                        yTrace(vps, "stmt: | mLhs tEQL mrhs");
			yyvalO = RubyParser::masgn_append_mrhs(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);   		      
                    }
break;
case 36:
/* # line 900 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | expr kAND expr");
                        OmScopeType aScope(vps->omPtr);
                        NODE **clsH = aScope.add( RubyAndNode::cls(vps));
                        yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 37:
/* # line 907 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | expr kOR expr");
                        OmScopeType aScope(vps->omPtr);
                        NODE **clsH = aScope.add( RubyOrNode::cls(vps));
                        yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 38:
/* # line 914 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | kNOT expr");
                        yyvalO = RubyNotNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 39:
/* # line 919 "grammar.y" */ 
	{
                        yTrace(vps, "expr: | tBANG command_call");
                        yyvalO = RubyNotNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 41:
/* # line 927 "grammar.y" */ 
	{
                        yTrace(vps, "expr_value: expr");
                        yyvalO = RubyParser::value_expr(yymarkPtr[0].obj, vps);
                    }
break;
case 44:
/* # line 936 "grammar.y" */ 
	{
                        yTrace(vps, "command_call: kRETURN call_args");
                        OmScopeType aScope(vps->omPtr);
			NODE **valH = aScope.add(RubyParser::ret_args(yymarkPtr[0].obj, vps));
                        yyvalO = RubyReturnNode::s(valH, yymarkPtr[-1].obj/*kRETURN token*/, vps);
                    }
break;
case 45:
/* # line 943 "grammar.y" */ 
	{
                        yTrace(vps, "command_call: | kBREAK call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args(yymarkPtr[0].obj, vps));
                        yyvalO = RubyBreakNode::s( valH, yymarkPtr[-1].obj/*kBREAK token*/, vps);
                    }
break;
case 46:
/* # line 950 "grammar.y" */ 
	{
                        yTrace(vps, "command_call: | kNEXT call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args(yymarkPtr[0].obj, vps));
                        yyvalO = RubyNextNode::s( valH, yymarkPtr[-1].obj/*kNEXT token*/, vps);
                    }
break;
case 48:
/* # line 960 "grammar.y" */ 
	{
                        yTrace(vps, "block_command: block_call...");
                        yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 49:
/* # line 965 "grammar.y" */ 
	{
                        yTrace(vps, "block_command: | block_call tCOLON2 operation2 command_args");
                        yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 50:
/* # line 972 "grammar.y" */ 
	{
                        yTrace(vps, "cmd_brace_block: tLBRACE_ARG");
                        reset_block(vps);
                        /* $1 = int64ToSi(vps->ruby_sourceline() );*/
                    }
break;
case 51:
/* # line 978 "grammar.y" */ 
	{ 
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 52:
/* # line 983 "grammar.y" */ 
	{
		      yTrace(vps, "cmd_brace_block: ___ comp_stamt tRCURLY");
                      rParenLexPop(vps);
		      popBlockVars(vps);
		      yyvalO = RubyIterRpNode::s( yymarkPtr[-3].obj/*masgn from opt_block_var*/ , yymarkPtr[-1].obj/*compstmp*/, yymarkPtr[-5].obj/*srcOffsetSi*/, 
						vps, 1/* strlen( '}' ) */ );
                    }
break;
case 53:
/* # line 993 "grammar.y" */ 
	{
                      yTrace(vps, "command: operation command_args =tLOWEST");
                        yyvalO = RubyParser::new_fcall(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                   }
break;
case 54:
/* # line 998 "grammar.y" */ 
	{
                      yTrace(vps, "command: | operation command_args cmd_brace_block");
                      yyvalO = RubyParser::new_fcall_braceBlock(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                   }
break;
case 55:
/* # line 1003 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tDOT operation2 command_args =tLOWEST");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 56:
/* # line 1008 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tDOT operation2 command_args cmd_brace_block");
                      yyvalO = RubyParser::new_call_braceBlock(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 57:
/* # line 1013 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tCOLON2 operation2 command_args =tLOWEST");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 58:
/* # line 1018 "grammar.y" */ 
	{
                      yTrace(vps, "command: | primary_value tCOLON2 operation2 command_args cmd_brace_block");
                      yyvalO = RubyParser::new_call_braceBlock(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                   }
break;
case 59:
/* # line 1023 "grammar.y" */ 
	{
                      yTrace(vps, "command: | kSUPER command_args");
                      yyvalO = RubyParser::new_super(& yymarkPtr[0].obj, yymarkPtr[-1].obj/*super token*/, vps);
                    }
break;
case 60:
/* # line 1028 "grammar.y" */ 
	{
                      yTrace(vps, "command: | kYIELD command_args");
                      yyvalO = RubyParser::new_yield(& yymarkPtr[0].obj, yymarkPtr[-1].obj/*yield token*/, vps);
                    }
break;
case 62:
/* # line 1036 "grammar.y" */ 
	{
                      yTrace(vps, "mLhs: | tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 64:
/* # line 1045 "grammar.y" */ 
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
/* # line 1056 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: mlhs_head ");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[0].obj, ofsO, vps);
                    }
break;
case 66:
/* # line 1062 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | mlhs_head mlhs_item");
                      OmScopeType aScope(vps->omPtr);
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      NODE *valO = RubyArrayNode::append_for_mlhs( yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn(valO, ofsO, vps);
                    }
break;
case 67:
/* # line 1070 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | mlhs_head tSTAR mlhs_node");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      *valH = RubyArrayNode::append_for_mlhs( yymarkPtr[-2].obj, *valH, vps);
                      yyvalO = RubyParser::new_parasgn(yymarkPtr[-2].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 68:
/* # line 1078 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | mlhs_head tSTAR");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      *valH = RubyArrayNode::append_for_mlhs( yymarkPtr[-1].obj, *valH, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-1].obj, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 69:
/* # line 1086 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | tSTAR mlhs_node");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      *valH = RubyArrayNode::s( *valH, vps);
                      yyvalO = RubyParser::new_parasgn( *valH, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 70:
/* # line 1094 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_basic: | tSTAR");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      *valH = RubyArrayNode::s( *valH, vps);
                      yyvalO = RubyParser::new_parasgn( *valH, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 72:
/* # line 1105 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_item: tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 73:
/* # line 1113 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_head: mlhs_item tCOMMA");
                      yyvalO = RubyArrayNode::s( yymarkPtr[-1].obj, vps);
                    }
break;
case 74:
/* # line 1118 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_head: | mlhs_head mlhs_item tCOMMA");
                      yyvalO = RubyArrayNode::append_for_mlhs(yymarkPtr[-2].obj, yymarkPtr[-1].obj, vps); /* result is $1*/
                    }
break;
case 75:
/* # line 1125 "grammar.y" */ 
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
/* # line 1136 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_node: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = assignable(& yymarkPtr[0].obj, ofsO, vps->nilH(), vps);
                    }
break;
case 77:
/* # line 1142 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_node: | primary_value tLBRACK_STR aref__args tRBRACK");
                      omObjSType *srcOfs = om::FetchOop(yymarkPtr[0].obj, 1); /* no gc*/
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[0].obj, 0);
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-1].obj, ram_OOP_NIL/*"[]="*/, aref_args, srcOfs, vps);
                    }
break;
case 78:
/* # line 1149 "grammar.y" */ 
	{
                      yTrace(vps, "mlhs_node: | primary_value tDOT tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, 
							ram_OOP_NIL, vps);
                    }
break;
case 79:
/* # line 1155 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 80:
/* # line 1160 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tDOT tCONSTANT");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 81:
/* # line 1165 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tCONSTANT");
                      if (vps->in_def || vps->in_single) {
                         rb_compile_error(vps, "dynamic constant assignment");
                      }
                      yyvalO = RubyConstDeclNode::colon2(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 82:
/* # line 1173 "grammar.y" */ 
	{
                      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "dynamic constant assignment");
                      }
                      yyvalO = RubyConstDeclNode::colon3( yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 83:
/* # line 1180 "grammar.y" */ 
	{
                      rb_backref_error(yymarkPtr[0].obj, vps);
                      yyvalO = ram_OOP_NIL;
                    }
break;
case 84:
/* # line 1187 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = assignable(& yymarkPtr[0].obj, ofsO, vps->nilH(), vps);
                    }
break;
case 85:
/* # line 1193 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tLBRACK_STR aref__args tRBRACK");
                      /* rParenLexPop(vps); */ /* Fix GitHub issue #148, ary_ref pops already */
                      omObjSType *srcOfs = om::FetchOop(yymarkPtr[0].obj, 1); /* no gc*/
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[0].obj, 0);
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-1].obj, ram_OOP_NIL/*"[]="*/, aref_args, srcOfs, vps);
                    }
break;
case 86:
/* # line 1201 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tDOT tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 87:
/* # line 1206 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tIDENTIFIER");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 88:
/* # line 1211 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tDOT tCONSTANT");
                      yyvalO = RubyAttrAssignNode::s(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
break;
case 89:
/* # line 1216 "grammar.y" */ 
	{
                      yTrace(vps, "lhs: | primary_value tCOLON2 tCONSTANT");
		      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "dynamic constant assignment");
                      }
                      yyvalO = RubyConstDeclNode::colon2(yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 90:
/* # line 1224 "grammar.y" */ 
	{
                      if (vps->in_def || vps->in_single) {
			  rb_compile_error(vps, "dynamic constant assignment");
                      }
                      OmScopeType aScope(vps->omPtr);
                      yyvalO = RubyConstDeclNode::colon3( yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 91:
/* # line 1232 "grammar.y" */ 
	{
                        rb_backref_error(yymarkPtr[0].obj, vps);
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 92:
/* # line 1239 "grammar.y" */ 
	{
                      yTrace(vps, "cname: tIDENTIFIER");
                      rb_compile_error(vps, "class/module name must be CONSTANT");
                    }
break;
case 94:
/* # line 1247 "grammar.y" */ 
	{
                      yTrace(vps, "cpath: tCOLON3 cname");
                      /* $$  = NEW_COLON3($2);*/
   		      yyvalO = RubyColon3Node::s(yymarkPtr[0].obj/*RpNameToken*/, vps);
                    }
break;
case 95:
/* # line 1253 "grammar.y" */ 
	{
                      yTrace(vps, "cpath: | cname");
                      /* $$  = NEW_COLON2(0, $$);*/
                      yyvalO = yymarkPtr[0].obj ; /* a RpNameToken*/
                    }
break;
case 96:
/* # line 1259 "grammar.y" */ 
	{
                      yTrace(vps, "cpath: | primary_value tCOLON2 cname");
                      /* $$  = NEW_COLON2($1, $3);*/
                      yyvalO = RubyColon2Node::s(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps); 
                    }
break;
case 100:
/* # line 1270 "grammar.y" */ 
	{
                      yTrace(vps, "fname: tIDENTIFIER | tCONSTANT | tFID | op");
                      vps->lex_state = EXPR_END;
                      /* $$  = convert_op($1);*/
                      yyvalO = yymarkPtr[0].obj; /* a RpNameToken*/
                    }
break;
case 101:
/* # line 1277 "grammar.y" */ 
	{
                      yTrace(vps, "fname: | reswords");
                      vps->lex_state = EXPR_END;
                      /* $$  = $<id>1;*/
                      yyvalO = yymarkPtr[0].obj; /* a RpNameToken or a String*/
                    }
break;
case 102:
/* # line 1286 "grammar.y" */ 
	{  /* deleted  fsym  : fname  */
		       /*                | symbol*/
                       /*                ; */
	               yTrace(vps, "fitem: fname");
                       yyvalO = RubySymbolNode::s( RpNameToken::symval(yymarkPtr[0].obj/*RpNameToken*/, vps), vps);
		    }
break;
case 103:
/* # line 1294 "grammar.y" */ 
	{
                       yTrace(vps, "fitem: | symbol");
                       /* $$  = NEW_LIT(QUID2SYM($1));*/
                       yyvalO = RubySymbolNode::s( yymarkPtr[0].obj/*a Symbol*/, vps);
                    }
break;
case 105:
/* # line 1303 "grammar.y" */ 
	{
                      yTrace(vps, "undef_list: fitem");
                      yyvalO = RubyParser::new_undef( yymarkPtr[0].obj/*a RubySymbolNode*/, vps); 
                    }
break;
case 106:
/* # line 1307 "grammar.y" */ 
	{vps->lex_state = EXPR_FNAME;}
break;
case 107:
/* # line 1308 "grammar.y" */ 
	{
                      yTrace(vps, "undef_list: ___ fitem");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::new_undef( yymarkPtr[0].obj, vps));
                      yyvalO = RubyParser::block_append(yymarkPtr[-3].obj, *valH, vps);
                    }
break;
case 108:
/* # line 1316 "grammar.y" */ 
	{ yTrace(vps, "op |");    yyvalO = RpNameToken::s(a_sym_orOp, yymarkPtr[0].obj, vps); }
break;
case 109:
/* # line 1317 "grammar.y" */ 
	{ yTrace(vps, "op ^");    yyvalO = RpNameToken::s( a_sym_upArrow, yymarkPtr[0].obj, vps); }
break;
case 110:
/* # line 1318 "grammar.y" */ 
	{ yTrace(vps, "op &");    yyvalO = RpNameToken::s(a_sym_andOp, yymarkPtr[0].obj, vps); }
break;
case 111:
/* # line 1319 "grammar.y" */ 
	{ yTrace(vps, "op tCMP"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 112:
/* # line 1320 "grammar.y" */ 
	{ yTrace(vps, "op tEQ");  yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 113:
/* # line 1321 "grammar.y" */ 
	{ yTrace(vps, "op tEQQ"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 114:
/* # line 1322 "grammar.y" */ 
	{ yTrace(vps, "op tMATCH"); yyvalO = RpNameToken::s(a_sym_tMATCH, yymarkPtr[0].obj, vps); }
break;
case 115:
/* # line 1323 "grammar.y" */ 
	{ yTrace(vps, "op >");    yyvalO = RpNameToken::s(a_sym_gt, yymarkPtr[0].obj, vps); }
break;
case 116:
/* # line 1324 "grammar.y" */ 
	{ yTrace(vps, "op tGEQ"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 117:
/* # line 1325 "grammar.y" */ 
	{ yTrace(vps, "op <");    yyvalO = RpNameToken::s( a_sym_lt, yymarkPtr[0].obj, vps); }
break;
case 118:
/* # line 1326 "grammar.y" */ 
	{ yTrace(vps, "op tLEQ"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 119:
/* # line 1327 "grammar.y" */ 
	{ yTrace(vps, "op tLSHFT"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 120:
/* # line 1328 "grammar.y" */ 
	{ yTrace(vps, "op tRSHFT"); yyvalO = yymarkPtr[0].obj/*a RpNameToken*/; }
break;
case 121:
/* # line 1329 "grammar.y" */ 
	{ yTrace(vps, "op +");    yyvalO = RpNameToken::s(a_sym_plus, yymarkPtr[0].obj, vps); }
break;
case 122:
/* # line 1330 "grammar.y" */ 
	{ yTrace(vps, "op -");    yyvalO = RpNameToken::s(a_sym_minus, yymarkPtr[0].obj, vps); }
break;
case 123:
/* # line 1331 "grammar.y" */ 
	{ yTrace(vps, "op *");    yyvalO = RpNameToken::s( a_sym_star, yymarkPtr[0].obj, vps); }
break;
case 124:
/* # line 1332 "grammar.y" */ 
	{ yTrace(vps, "op tSTAR"); yyvalO = RpNameToken::s( a_sym_star, yymarkPtr[0].obj, vps); }
break;
case 125:
/* # line 1333 "grammar.y" */ 
	{ yTrace(vps, "op /");    yyvalO = RpNameToken::s( a_sym_div, yymarkPtr[0].obj, vps); }
break;
case 126:
/* # line 1334 "grammar.y" */ 
	{ yTrace(vps, "op %");    yyvalO = RpNameToken::s( a_sym_percent, yymarkPtr[0].obj, vps); }
break;
case 127:
/* # line 1335 "grammar.y" */ 
	{ yTrace(vps, "op tPOW"); yyvalO = RpNameToken::s( a_sym_tPOW, yymarkPtr[0].obj, vps); }
break;
case 128:
/* # line 1336 "grammar.y" */ 
	{ yTrace(vps, "op ~");    yyvalO = RpNameToken::s(a_sym_tilde, yymarkPtr[0].obj, vps); }
break;
case 129:
/* # line 1337 "grammar.y" */ 
	{ yTrace(vps, "op tUPLUS"); yyvalO = RpNameToken::s( a_sym_tUPLUS, yymarkPtr[0].obj, vps);}
break;
case 130:
/* # line 1338 "grammar.y" */ 
	{ yTrace(vps, "op tUMINUS"); yyvalO = RpNameToken::s(a_sym_tUMINUS, yymarkPtr[0].obj, vps);; }
break;
case 131:
/* # line 1339 "grammar.y" */ 
	{ yTrace(vps, "op tAREF"); yyvalO = RpNameToken::s(a_sym_tAREF, yymarkPtr[0].obj, vps); }
break;
case 132:
/* # line 1340 "grammar.y" */ 
	{ yTrace(vps, "op tASET"); yyvalO = RpNameToken::s(a_sym_tASET, yymarkPtr[0].obj, vps); }
break;
case 133:
/* # line 1341 "grammar.y" */ 
	{ yTrace(vps, "op `");    yyvalO = RpNameToken::s( a_sym_backtick, yymarkPtr[0].obj, vps); }
break;
case 175:
/* # line 1354 "grammar.y" */ 
	{
                      yTrace(vps, "arg: lhs tEQL arg");
                      yyvalO = RubyParser::node_assign( & yymarkPtr[-2].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, yymarkPtr[0].obj, vps);
                    }
break;
case 176:
/* # line 1359 "grammar.y" */ 
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
/* # line 1369 "grammar.y" */ 
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
/* # line 1380 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tLBRACK_STR aref__args tRBRACK tOP_ASGN arg");
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[-2].obj, 0);
                      yyvalO = RubyOpElementAsgnNode::s(yymarkPtr[-3].obj, aref_args, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 179:
/* # line 1386 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tDOT tIDENTIFIER tOP_ASGN arg");
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 180:
/* # line 1391 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tDOT tCONSTANT tOP_ASGN arg");
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 181:
/* # line 1396 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg");
                      /* not seen with Ryan's grammar*/
                      yyvalO = RubyOpAsgnNode::s(yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 182:
/* # line 1402 "grammar.y" */ 
	{
                        rb_compile_error(vps, "constant re-assignment");
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 183:
/* # line 1407 "grammar.y" */ 
	{
                        rb_compile_error(vps, "constant re-assignment");
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 184:
/* # line 1412 "grammar.y" */ 
	{
                        rb_backref_error(yymarkPtr[-2].obj, vps);
                        yyvalO = ram_OOP_NIL;
                    }
break;
case 185:
/* # line 1417 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tDOT2 arg");
                      yyvalO = RubyDotNode::s(2, yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 186:
/* # line 1422 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tDOT3 arg");
                      yyvalO = RubyDotNode::s(3, yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 187:
/* # line 1427 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPLUS arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_plus, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 188:
/* # line 1432 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tMINUS arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_minus, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 189:
/* # line 1437 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tSTAR2 arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_star, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 190:
/* # line 1442 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tDIVIDE arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_div, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 191:
/* # line 1447 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPERCENT arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_percent, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 192:
/* # line 1452 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPOW arg");
                      yyvalO = RubyParser::new_call_1( & yymarkPtr[-2].obj, a_sym_tPOW, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 193:
/* # line 1457 "grammar.y" */ 
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
/* # line 1468 "grammar.y" */ 
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
/* # line 1478 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tUPLUS arg");
                      yyvalO = RubyParser::uplus_production( yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 196:
/* # line 1483 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tUMINUS arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, yymarkPtr[-1].obj, vps));
                      yyvalO = RubyParser::new_vcall( yymarkPtr[0].obj, *selH, vps);
                    }
break;
case 197:
/* # line 1490 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tPIPE arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_orOp, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 198:
/* # line 1495 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tCARET arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_upArrow, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 199:
/* # line 1500 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tAMPER2 arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_andOp, & yymarkPtr[0].obj,  yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 200:
/* # line 1505 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tCMP arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 201:
/* # line 1510 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tGT arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_gt, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 202:
/* # line 1515 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tGEQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 203:
/* # line 1520 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tLT arg");
                      yyvalO = RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_lt, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 204:
/* # line 1525 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tLEQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 205:
/* # line 1530 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tEQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 206:
/* # line 1535 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tEQQ arg");
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 207:
/* # line 1540 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tNEQ arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::new_call_1(& yymarkPtr[-2].obj, a_sym_tEQ, & yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps));
                      yyvalO = RubyNotNode::s( *valH, vps);
                    }
break;
case 208:
/* # line 1547 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tMATCH arg");
                      yyvalO = RubyParser::get_match_node(yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 209:
/* # line 1552 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tNMATCH arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::get_match_node(yymarkPtr[-2].obj, yymarkPtr[0].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps));
                      yyvalO = RubyNotNode::s( *valH, vps);
                    }
break;
case 210:
/* # line 1559 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tBANG arg");
                      yyvalO = RubyNotNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 211:
/* # line 1564 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | tTILDE arg");
                      OmScopeType aScope(vps->omPtr);	/* try it without value_expr*/
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tilde, yymarkPtr[-1].obj, vps));
                      yyvalO = RubyParser::new_vcall( yymarkPtr[0].obj,  *selH, vps);
                    }
break;
case 212:
/* # line 1571 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tRSHFT arg"); /* try without value_expr*/
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 213:
/* # line 1576 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tRSHFT arg"); /* try without value_expr*/
                      yyvalO = RubyParser::new_call_1(yymarkPtr[-2].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 214:
/* # line 1581 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tANDOP arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **clsH = aScope.add( RubyAndNode::cls(vps));
                      yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 215:
/* # line 1588 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tOROP arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **clsH = aScope.add( RubyOrNode::cls(vps));
                      yyvalO = RubyParser::logop(*clsH, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 216:
/* # line 1594 "grammar.y" */ 
	{vps->in_defined = 1;}
break;
case 217:
/* # line 1595 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | kDEFINED opt_nl arg");
                      vps->in_defined = 0;
                      yyvalO = RubyDefinedNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 218:
/* # line 1600 "grammar.y" */ 
	{vps->ternary_colon++;}
break;
case 219:
/* # line 1601 "grammar.y" */ 
	{
                      yTrace(vps, "arg: | arg tEH arg tCOLON arg");
                      yyvalO = RubyIfNode::s(yymarkPtr[-5].obj, yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                      vps->ternary_colon--;
                    }
break;
case 220:
/* # line 1607 "grammar.y" */ 
	{
                        yTrace(vps, "arg: | primary");
                        yyvalO = yymarkPtr[0].obj;
                    }
break;
case 221:
/* # line 1614 "grammar.y" */ 
	{
                      yTrace(vps, "arg_value: arg");
                      yyvalO = RubyParser::value_expr(yymarkPtr[0].obj, vps);
                    }
break;
case 223:
/* # line 1622 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | command opt_nl");
                      yyvalO = RubyRpCallArgs::s(yymarkPtr[-1].obj, vps);
                    }
break;
case 224:
/* # line 1627 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | args trailer");
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 225:
/* # line 1632 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | args tCOMMA tSTAR arg opt_nl");
                      /* value_expr($4);  was in rubinius, try without*/
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyArrayNode::append( yymarkPtr[-4].obj , *valH, vps /*returns first arg*/);
                    }
break;
case 226:
/* # line 1640 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | assocs trailer");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s(*hashNodeH, vps);
                    }
break;
case 227:
/* # line 1647 "grammar.y" */ 
	{
                      yTrace(vps, "aref__args: | tSTAR arg opt_nl");
                      yymarkPtr[-1].obj = RubyParser::value_expr(yymarkPtr[-1].obj, vps);
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s( yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s( *valH, vps);
                    }
break;
case 228:
/* # line 1657 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: tLPAREN2 none tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 229:
/* # line 1663 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: | tLPAREN2 call_args opt_nl tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = yymarkPtr[-2].obj;
                    }
break;
case 230:
/* # line 1669 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: | tLPAREN2 block_call opt_nl tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyRpCallArgs::s( yymarkPtr[-2].obj, vps);
                    }
break;
case 231:
/* # line 1675 "grammar.y" */ 
	{
                      yTrace(vps, "paren_args: | tLPAREN2 args tCOMMA block_call opt_nl tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyArrayNode::append( yymarkPtr[-4].obj, yymarkPtr[-2].obj, vps);
                    }
break;
case 234:
/* # line 1687 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: command");
		      yyvalO = RubyRpCallArgs::s( yymarkPtr[0].obj, vps);
                    }
break;
case 235:
/* # line 1692 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args opt_block_arg");
                        yyvalO = RubyRpCallArgs::append_blkArg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps /*returns first arg*/);
                    }
break;
case 236:
/* # line 1697 "grammar.y" */ 
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
/* # line 1708 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_blkArg(*hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 238:
/* # line 1715 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_splatArg_blkArg(*hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 239:
/* # line 1722 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::append_arg_blkArg(yymarkPtr[-3].obj, *hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 240:
/* # line 1729 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | args tCOMMA assocs tCOMMA tSTAR arg opt_block_arg");
                      /* rubinius had   value_expr($6);*/
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::append_arg_splatArg_blkArg(yymarkPtr[-6].obj, *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);

                    }
break;
case 241:
/* # line 1738 "grammar.y" */ 
	{
                      yTrace(vps, "call_args: | tSTAR arg_value opt_block_arg");
                      yyvalO = RubyRpCallArgs::s_splatArg_blkArg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 243:
/* # line 1746 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: arg_value tCOMMA args opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callArgsH = aScope.add( RubyParser::list_prepend(yymarkPtr[-1].obj, yymarkPtr[-3].obj, vps));
                      RubyRpCallArgs::append_blkArg( *callArgsH, yymarkPtr[0].obj, vps);
                      yyvalO = *callArgsH;
                    }
break;
case 244:
/* # line 1754 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA block_arg");
                      yyvalO = RubyRpCallArgs::append_blkArg( yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 245:
/* # line 1759 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA tSTAR arg_value opt_block_arg");
                      yyvalO = RubyRpCallArgs::s_arg_splatArg_blkArg( yymarkPtr[-4].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 246:
/* # line 1764 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callArgsH = aScope.add( RubyParser::list_prepend(yymarkPtr[-4].obj, yymarkPtr[-6].obj, vps));
                      yyvalO = RubyRpCallArgs::append_arg_blkArg(*callArgsH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 247:
/* # line 1771 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_blkArg(*hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 248:
/* # line 1778 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_splatArg_blkArg( *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 249:
/* # line 1785 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_arg_blkArg( yymarkPtr[-3].obj, *hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 250:
/* # line 1792 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_addAll_arg_blkArg(yymarkPtr[-5].obj, yymarkPtr[-3].obj, *hashNodeH, yymarkPtr[0].obj, vps);
                    }
break;
case 251:
/* # line 1799 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
		      yyvalO = RubyRpCallArgs::s_arg_arg_splatArg_blkArg(yymarkPtr[-6].obj, *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 252:
/* # line 1806 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s(yymarkPtr[-4].obj, vps));
                      yyvalO = RubyRpCallArgs::s_arg_addAll_arg_splatArg_blkArg(yymarkPtr[-8].obj, yymarkPtr[-6].obj, *hashNodeH, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 253:
/* # line 1813 "grammar.y" */ 
	{
                      yTrace(vps, "call_args2: | tSTAR arg_value opt_block_arg");
                      yyvalO = RubyRpCallArgs::s_splatArg_blkArg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 255:
/* # line 1820 "grammar.y" */ 
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
/* # line 1832 "grammar.y" */ 
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
/* # line 1847 "grammar.y" */ 
	{vps->lex_state = EXPR_ENDARG;}
break;
case 259:
/* # line 1848 "grammar.y" */ 
	{
                      yTrace(vps, "open_args: tLPAREN_ARG");
                      rParenLexPop(vps);
                      rb_warning(vps, "don't put space before argument parentheses");
                      yyvalO = ram_OOP_NIL;
                    }
break;
case 260:
/* # line 1854 "grammar.y" */ 
	{vps->lex_state = EXPR_ENDARG;}
break;
case 261:
/* # line 1855 "grammar.y" */ 
	{
                      yTrace(vps, "open_args: ___ tRPAREN");
                      rParenLexPop(vps);
		      rb_warning(vps, "don't put space before argument parentheses");
		      yyvalO = yymarkPtr[-2].obj;
                    }
break;
case 262:
/* # line 1864 "grammar.y" */ 
	{
                      yTrace(vps, "block_arg: tAMPER arg_value");
                      yyvalO = RubyBlockPassNode::s( yymarkPtr[0].obj , vps);
                    }
break;
case 263:
/* # line 1869 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_arg: tCOMMA block_arg");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 265:
/* # line 1877 "grammar.y" */ 
	{
                      yTrace(vps, "args: arg_value");
                      yyvalO = RubyRpCallArgs::s( yymarkPtr[0].obj, vps);
                    }
break;
case 266:
/* # line 1882 "grammar.y" */ 
	{
                      yTrace(vps, " args: | args tCOMMA arg_value");
                      yyvalO = RubyRpCallArgs::append_arg(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps /*return first arg*/);
                    }
break;
case 267:
/* # line 1889 "grammar.y" */ 
	{
                      yTrace(vps, "mrhs: args tCOMMA arg_value");
                      yyvalO = RubyRpCallArgs::append_arg(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps /*return first arg*/);
                    }
break;
case 268:
/* # line 1894 "grammar.y" */ 
	{
                      yTrace(vps, "mrhs: | args tCOMMA tSTAR arg_value");
                      yyvalO = RubyRpCallArgs::append_splatArg(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 269:
/* # line 1899 "grammar.y" */ 
	{
                      yTrace(vps, "mrhs: | tSTAR arg_value");
                      yyvalO = RubySplatNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 278:
/* # line 1914 "grammar.y" */ 
	{
                      yTrace(vps, "primary: tFID");
                      yyvalO = RubyParser::new_fcall(yymarkPtr[0].obj, ram_OOP_NIL, vps);
                    }
break;
case 279:
/* # line 1919 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kBEGIN");
                        /* $<num>1 = ruby_sourceline;*/
                      PUSH_LINE(vps, "begin");
                    }
break;
case 280:
/* # line 1926 "grammar.y" */ 
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
/* # line 1936 "grammar.y" */ 
	{vps->lex_state = EXPR_ENDARG;}
break;
case 282:
/* # line 1937 "grammar.y" */ 
	{
                      yTrace(vps, "primary: ___ opt_nl tRPAREN");
                      rParenLexPop(vps);
		      rb_warning(vps, "(...) interpreted as grouped expression");
		      yyvalO = yymarkPtr[-3].obj;
                    }
break;
case 283:
/* # line 1944 "grammar.y" */ 
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
/* # line 1957 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | primary_value tCOLON2 tCONSTANT");
                      yyvalO = RubyColon2Node::s(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 285:
/* # line 1962 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | tCOLON3 tCONSTANT");
                      yyvalO = RubyColon3Node::s( yymarkPtr[0].obj, vps);
                    }
break;
case 286:
/* # line 1967 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | primary_value tLBRACK_STR aref__args tRBRACK");
                      omObjSType *srcOfs = om::FetchOop(yymarkPtr[0].obj, 1); /* no gc*/
                      omObjSType *aref_args = om::FetchOop(yymarkPtr[0].obj, 0);
                      yyvalO = RubyParser::new_aref(yymarkPtr[-1].obj, aref_args, srcOfs, vps);
                    }
break;
case 287:
/* # line 1974 "grammar.y" */ 
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
/* # line 1984 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | tLBRACE assoc_list tRCURLY");
                      rParenLexPop(vps);
                      yyvalO = RubyHashNode::s(yymarkPtr[-1].obj, vps);
                    }
break;
case 289:
/* # line 1990 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kRETURN");
                      yyvalO = RubyReturnNode::s( vps->nilH(), yymarkPtr[0].obj/*return token*/, vps);
                    }
break;
case 290:
/* # line 1995 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kYIELD tLPAREN2 call_args tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyParser::new_yield(& yymarkPtr[-1].obj, yymarkPtr[-3].obj/*yield token*/, vps);
                    }
break;
case 291:
/* # line 2001 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kYIELD tLPAREN2 tRPAREN");
                      rParenLexPop(vps);
                      yyvalO = RubyParser::new_yield(vps->nilH(), yymarkPtr[-2].obj/*yield token*/, vps);
                    }
break;
case 292:
/* # line 2007 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kYIELD");
                      yyvalO = RubyParser::new_yield(vps->nilH(), yymarkPtr[0].obj/*yield token*/, vps);
                    }
break;
case 293:
/* # line 2011 "grammar.y" */ 
	{vps->in_defined = 1;}
break;
case 294:
/* # line 2012 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEFINED opt_nl tLPAREN2 expr tRPAREN");
                      rParenLexPop(vps);
                      vps->in_defined = 0;
                      yyvalO = RubyDefinedNode::s(yymarkPtr[-1].obj, vps);
                    }
break;
case 295:
/* # line 2019 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | operation brace_blck");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callH = aScope.add( RubyParser::new_fcall( yymarkPtr[-1].obj, ram_OOP_NIL, vps));
                      RubyIterRpNode::set_call( yymarkPtr[0].obj, *callH, vps);
                      yyvalO = yymarkPtr[0].obj; /* $2 is a RubyIterRpNode*/
                    }
break;
case 297:
/* # line 2028 "grammar.y" */ 
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
/* # line 2036 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "if");
                  }
break;
case 299:
/* # line 2042 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kIF expr_value then comp_stamt if_tail kEND");
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* kIF*/
                      yyvalO = RubyParser::new_if( yymarkPtr[-4].obj, yymarkPtr[-2].obj, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 300:
/* # line 2048 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "unless");
                  }
break;
case 301:
/* # line 2054 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kUNLESS expr_value then comp_stamt opt_else kEND");
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* kUNLESS*/
		      yyvalO = RubyParser::new_if( yymarkPtr[-4].obj, yymarkPtr[-1].obj, yymarkPtr[-2].obj, srcOfs, vps);
                    }
break;
case 302:
/* # line 2060 "grammar.y" */ 
	{
                    yTrace(vps, "primary: | kWHILE");
                    PUSH_LINE(vps, "while");
                    COND_PUSH(vps, 1);
                  }
break;
case 303:
/* # line 2064 "grammar.y" */ 
	{ COND_POP(vps);}
break;
case 304:
/* # line 2067 "grammar.y" */ 
	{
                      yTrace(vps, "primary: kWHILE ___ comp_stamt kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* of kWHILE*/
                      yyvalO = RubyParser::new_while( yymarkPtr[-1].obj, yymarkPtr[-4].obj, srcOfs, vps);
                    }
break;
case 305:
/* # line 2073 "grammar.y" */ 
	{
                    yTrace(vps, "primary: | kUNTIL");
                    PUSH_LINE(vps, "until");
                    COND_PUSH(vps, 1);
                  }
break;
case 306:
/* # line 2077 "grammar.y" */ 
	{ COND_POP(vps);}
break;
case 307:
/* # line 2080 "grammar.y" */ 
	{
                      yTrace(vps, "kUNTIL ___ comp_stamt kEND");
		      /* maglev had premature_eof() check*/
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-6].obj); /* of kUNTIL*/
		      yyvalO = RubyParser::new_until( yymarkPtr[-1].obj, yymarkPtr[-4].obj, srcOfs, vps);
                    }
break;
case 308:
/* # line 2087 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "case");
                  }
break;
case 309:
/* # line 2092 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCASE expr_value opt_termms case_body kEND");
		      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj); /* of kCASE*/
		      yyvalO = RubyCaseNode::s(yymarkPtr[-3].obj, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 310:
/* # line 2098 "grammar.y" */ 
	{ 
                    push_start_line(vps, vps->ruby_sourceline() - 1, "case");
                  }
break;
case 311:
/* # line 2101 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCASE opt_termms case_body kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-4].obj); /* of kCASE*/
                      yyvalO = RubyCaseNode::s(ram_OOP_NIL, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 312:
/* # line 2107 "grammar.y" */ 
	{
                    push_start_line(vps, vps->ruby_sourceline() - 1, "case");
                  }
break;
case 313:
/* # line 2110 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCASE opt_termms kELSE comp_stamt kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj); /* of kCASE*/
                      yyvalO = RubyCaseNode::s(ram_OOP_NIL, yymarkPtr[-1].obj, srcOfs, vps);
                    }
break;
case 314:
/* # line 2116 "grammar.y" */ 
	{
                    PUSH_LINE(vps, "for");
                  }
break;
case 315:
/* # line 2118 "grammar.y" */ 
	{ COND_PUSH(vps, 1);}
break;
case 316:
/* # line 2118 "grammar.y" */ 
	{ COND_POP(vps);}
break;
case 317:
/* # line 2121 "grammar.y" */ 
	{
                      yTrace(vps, "primary: kFOR ___ comp_stamt kEND");
                      POP_LINE(vps);
                      yyvalO = RubyForNode::s( & yymarkPtr[-4].obj, & yymarkPtr[-7].obj, & yymarkPtr[-1].obj, yymarkPtr[-9].obj/*for token*/, 
						vps, 3/* strlen( 'end' ) */ );
                    }
break;
case 318:
/* # line 2128 "grammar.y" */ 
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
/* # line 2140 "grammar.y" */ 
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
/* # line 2153 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kCLASS tLSHFT expr");
		      PUSH_LINE(vps, "class");
		      yyvalO = int64ToSi( vps->in_def );
		      vps->in_def = 0;
                    }
break;
case 321:
/* # line 2160 "grammar.y" */ 
	{
                      yTrace(vps, "primary | kCLASS ___ Term");
		      yyvalO = int64ToSi( vps->in_single );
		      vps->in_single = 0;
		      vps->class_nest++;
		      local_push(vps, 0);
                    }
break;
case 322:
/* # line 2169 "grammar.y" */ 
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
/* # line 2181 "grammar.y" */ 
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
/* # line 2193 "grammar.y" */ 
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
/* # line 2205 "grammar.y" */ 
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
/* # line 2216 "grammar.y" */ 
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
/* # line 2229 "grammar.y" */ 
	{vps->lex_state = EXPR_FNAME;}
break;
case 328:
/* # line 2230 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kDEF ___ fname");
		      PUSH_LINE(vps, "def");
		      vps->in_single++;
		      local_push(vps, 0);
		      vps->lex_state = EXPR_END; /* force for args */
                    }
break;
case 329:
/* # line 2240 "grammar.y" */ 
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
/* # line 2254 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kBREAK");
                      yyvalO = RubyBreakNode::s(vps->nilH(), yymarkPtr[0].obj/*break token*/, vps);
                    }
break;
case 331:
/* # line 2259 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kNEXT");
                      yyvalO = RubyNextNode::s(vps->nilH(), yymarkPtr[0].obj/*next token*/, vps);
                    }
break;
case 332:
/* # line 2264 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kREDO");
                      yyvalO = RubyRedoNode::s(yymarkPtr[0].obj/*redo token*/, vps);
                    }
break;
case 333:
/* # line 2269 "grammar.y" */ 
	{
                      yTrace(vps, "primary: | kRETRY");
                      yyvalO = RubyRetryNode::s(yymarkPtr[0].obj/*retry token*/, vps);
                    }
break;
case 334:
/* # line 2276 "grammar.y" */ 
	{
                      yTrace(vps, "primary_value: primary");
                      yyvalO = RubyParser::value_expr(yymarkPtr[0].obj, vps);
                    }
break;
case 343:
/* # line 2297 "grammar.y" */ 
	{
                      yTrace(vps, "if_tail: opt_else| kELSIF___if_tail ");
                      yyvalO = RubyIfNode::s(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 345:
/* # line 2305 "grammar.y" */ 
	{
                      yTrace(vps, "opt_else: | kELSE comp_stamt");
		      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 348:
/* # line 2316 "grammar.y" */ 
	{
		      yTrace(vps, "block_par : mlhs_item");
		      yyvalO = RubyArrayNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 349:
/* # line 2321 "grammar.y" */ 
	{
		      yTrace(vps, "block_par : block_par , mlhs_item");
		      yyvalO = RubyArrayNode::append(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 350:
/* # line 2328 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var : block_par x");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
		      yyvalO = RubyParser::new_parasgn( yymarkPtr[0].obj, ofsO, vps);
                    }
break;
case 351:
/* # line 2334 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var | block_par , x");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
		      yyvalO = RubyParser::new_parasgn_trailingComma( yymarkPtr[-1].obj, ofsO, vps);
                    }
break;
case 352:
/* # line 2340 "grammar.y" */ 
	{
		      yTrace(vps, "blck_var | block_par , & lhs x");
                      RubyArrayNode::append_amperLhs(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-3].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);   
                    }
break;
case 353:
/* # line 2346 "grammar.y" */ 
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
/* # line 2355 "grammar.y" */ 
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
/* # line 2364 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | block_par , STAR lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      RubyArrayNode::append(yymarkPtr[-3].obj, *splatH, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-3].obj, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 356:
/* # line 2372 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | block_par , STAR x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      RubyArrayNode::append(yymarkPtr[-2].obj, *splatH, vps);
                      yyvalO = RubyParser::new_parasgn( yymarkPtr[-2].obj, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 357:
/* # line 2380 "grammar.y" */ 
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
/* # line 2389 "grammar.y" */ 
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
/* # line 2398 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | STAR lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(yymarkPtr[0].obj, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 360:
/* # line 2406 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | STAR x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[0].obj/*srcOffsetSi*/, vps);
                    }
break;
case 361:
/* # line 2414 "grammar.y" */ 
	{
                      yTrace(vps, "blck_var | & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **arrH = aScope.add(RubyArrayNode::new_(vps));
                      RubyArrayNode::append_amperLhs(*arrH, yymarkPtr[0].obj, vps);
                      yyvalO = RubyParser::new_parasgn( *arrH, yymarkPtr[-1].obj/*srcOffsetSi*/, vps);
                    }
break;
case 363:
/* # line 2425 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_var: | tPIPE tPIPE");
                      yyvalO = ram_OOP_NIL ;
                    }
break;
case 364:
/* # line 2430 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_var: | tOROP");
                      yyvalO = ram_OOP_NIL ;
                    }
break;
case 365:
/* # line 2435 "grammar.y" */ 
	{
                      yTrace(vps, "opt_block_var: | tPIPE blck_var tPIPE");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 366:
/* # line 2442 "grammar.y" */ 
	{
                      yTrace(vps, "do_block: kDO_BLOCK");
		      PUSH_LINE(vps, "do");
		      reset_block(vps);
                      /* $1 = int64ToSi(vps->ruby_sourceline() );*/
                    }
break;
case 367:
/* # line 2449 "grammar.y" */ 
	{
                      yTrace(vps, "do_block: ___ opt_block_var");
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 368:
/* # line 2455 "grammar.y" */ 
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
/* # line 2466 "grammar.y" */ 
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
/* # line 2475 "grammar.y" */ 
	{
                      yTrace(vps, "block_call: | block_call tDOT operation2 opt_paren_args");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 371:
/* # line 2480 "grammar.y" */ 
	{
                      yTrace(vps, "block_call: block_call tCOLON2 operation2 opt_paren_args");
		      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 372:
/* # line 2487 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: operation  paren_args");
                      yyvalO = RubyParser::new_fcall(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 373:
/* # line 2492 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | primary_value tDOT operation2 opt_paren_args");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 374:
/* # line 2497 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | primary_value tCOLON2 operation2 paren_args");
                      yyvalO = RubyParser::new_call(yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 375:
/* # line 2502 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | primary_value tCOLON2 operation3");
		      yyvalO = RubyParser::new_vcall(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 376:
/* # line 2508 "grammar.y" */ 
	{
                        rb_compile_error(vps, "\\ operator is rubinius-specific get_reference");
                    }
break;
case 377:
/* # line 2512 "grammar.y" */ 
	{
                        rb_compile_error(vps, "\\ operator is rubinius-specific get_reference");
                    }
break;
case 378:
/* # line 2517 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | kSUPER paren_args");
                      yyvalO = RubyParser::new_super(&  yymarkPtr[0].obj, yymarkPtr[-1].obj/*super token*/, vps);
                    }
break;
case 379:
/* # line 2522 "grammar.y" */ 
	{
                      yTrace(vps, "method_call: | kSUPER");
                      yyvalO = RubyZSuperNode::s( yymarkPtr[0].obj/*super token*/ , vps);
                    }
break;
case 380:
/* # line 2529 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: tLCURLY");
		      reset_block(vps);
		      /* $1 is srcOffsetSi */
                    }
break;
case 381:
/* # line 2535 "grammar.y" */ 
	{ 
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 382:
/* # line 2539 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: tLCURLY ___ comp_stamt tRCURLY");
                      rParenLexPop(vps);
                      popBlockVars(vps);
                      yyvalO = RubyIterRpNode::s(yymarkPtr[-3].obj/*masgn from opt_block_var*/, yymarkPtr[-1].obj/*compstmt*/, yymarkPtr[-5].obj/*srcOffsetSi*/, 
						vps, 1/* strlen( '}' ) */ );
                    }
break;
case 383:
/* # line 2547 "grammar.y" */ 
	{
                      yTrace(vps, "brace_blck: | kDO");
		      PUSH_LINE(vps, "do");
		      /* $1 is RpNameToken of 'do'*/
		      reset_block(vps);
                    }
break;
case 384:
/* # line 2554 "grammar.y" */ 
	{
                       yyvalO = ram_OOP_NIL; /* getBlockVars not used*/
                    }
break;
case 385:
/* # line 2558 "grammar.y" */ 
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
/* # line 2571 "grammar.y" */ 
	{
                      yTrace(vps, "case_body: kWHEN when_args then comp_stamt cases");
                      yyvalO = RubyWhenNode::s( & yymarkPtr[-3].obj, & yymarkPtr[-1].obj, & yymarkPtr[0].obj, yymarkPtr[-4].obj/*when token*/, vps);
                    }
break;
case 388:
/* # line 2578 "grammar.y" */ 
	{
                      yTrace(vps, "when_args: args | args tCOMMA tSTAR arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE **whenH = aScope.add( RubyWhenNode::s( & yymarkPtr[0].obj, vps->nilH(), vps->nilH(), 
									yymarkPtr[-1].obj/*srcOffsetSi of tSTAR*/, vps));
                      yyvalO = RubyParser::list_append(yymarkPtr[-3].obj, *whenH, vps);
                    }
break;
case 389:
/* # line 2586 "grammar.y" */ 
	{
                      yTrace(vps, "when_args: | tSTAR arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE **whenH = aScope.add( RubyWhenNode::s( & yymarkPtr[0].obj, vps->nilH(), vps->nilH(),
                                                                        yymarkPtr[-1].obj/*srcOffsetSi of tSTAR*/, vps));
                      yyvalO = RubyRpCallArgs::s( *whenH, vps);
                    }
break;
case 392:
/* # line 2602 "grammar.y" */ 
	{
                      yTrace(vps, "opt_rescue: kRESCUE exc_list exc_var then comp_stamt opt_rescue");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, yymarkPtr[-5].obj);
                      yyvalO = RubyParser::opt_rescue( yymarkPtr[-4].obj, yymarkPtr[-3].obj, yymarkPtr[-1].obj, yymarkPtr[0].obj, srcOfs, vps);
                    }
break;
case 394:
/* # line 2611 "grammar.y" */ 
	{
                      yTrace(vps, "exc_list: arg_value");
                      yyvalO = RubyArrayNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 397:
/* # line 2620 "grammar.y" */ 
	{
                      yTrace(vps, "exc_var: tASSOC lhs");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 399:
/* # line 2628 "grammar.y" */ 
	{
                      yTrace(vps, "opt_ensure: kENSURE comp_stamt");
                      /* $2 is argument block to rubyEnsure:*/
                      yyvalO = RubyEnsureNode::s(& yymarkPtr[0].obj/*nil arg coerced to RubyNilNode::new_*/ , yymarkPtr[-1].obj/*ensure token*/, vps);
                    }
break;
case 401:
/* # line 2637 "grammar.y" */ 
	{
                      yTrace(vps, "literal: numeric");
                      yyvalO= RubyAbstractNumberNode::s( yymarkPtr[0].obj , vps);
                    }
break;
case 402:
/* # line 2642 "grammar.y" */ 
	{
                      yTrace(vps, "literal: | symbol");
                      yyvalO = RubySymbolNode::s( quidToSymbolObj(yymarkPtr[0].obj, vps), vps);
                    }
break;
case 404:
/* # line 2650 "grammar.y" */ 
	{
                      yTrace(vps, "strings: string");
                      yyvalO = RubyParser::new_string(yymarkPtr[0].obj, vps);
                    }
break;
case 406:
/* # line 2658 "grammar.y" */ 
	{
                      yTrace(vps, "string: | string string1");
                      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 407:
/* # line 2665 "grammar.y" */ 
	{
                      yTrace(vps, "string1: tSTRING_BEG string_contents tSTRING_END");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 408:
/* # line 2672 "grammar.y" */ 
	{
                      yTrace(vps, "xstring: tXSTRING_BEG xstring_contents tSTRING_END");
                      yyvalO = RubyParser::new_xstring(yymarkPtr[-1].obj, vps);
                    }
break;
case 409:
/* # line 2679 "grammar.y" */ 
	{
                      yTrace(vps, "regexp: tREGEXP_BEG xstring_contents tREGEXP_END");
                      yyvalO = RubyParser::new_regexp( yymarkPtr[-1].obj, yymarkPtr[0].obj/*regexp options Si*/, vps);
                    }
break;
case 410:
/* # line 2686 "grammar.y" */ 
	{
                      yTrace(vps, "words: tWORDS_BEG tSPACE tSTRING_END");
                      yyvalO = RubyArrayNode::new_(vps);
                    }
break;
case 411:
/* # line 2691 "grammar.y" */ 
	{
                      yTrace(vps, "words: | tWORDS_BEG word_list tSTRING_END");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 412:
/* # line 2698 "grammar.y" */ 
	{
                      yTrace(vps, "word_list: none");
                      yyvalO = RubyArrayNode::new_(vps); /* $$  = 0;*/
                    }
break;
case 413:
/* # line 2703 "grammar.y" */ 
	{
                      yTrace(vps, "word_list: | word_list word tSPACE");
                      yyvalO = RubyParser::append_evstr2dstr( yymarkPtr[-2].obj , yymarkPtr[-1].obj, vps);
                    }
break;
case 415:
/* # line 2711 "grammar.y" */ 
	{
                      yTrace(vps, "word: | word string_content");
		      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 416:
/* # line 2718 "grammar.y" */ 
	{
		      yTrace(vps, "tQWORDS_BEG tSPACE tSTRING_END");
                      yyvalO = RubyArrayNode::new_(vps);
                    }
break;
case 417:
/* # line 2723 "grammar.y" */ 
	{
		      yTrace(vps, "tQWORDS_BEG qword_list tSTRING_END");
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 418:
/* # line 2730 "grammar.y" */ 
	{
                      yTrace(vps, "qword_list: none");
		      yyvalO = RubyArrayNode::new_(vps); /* $$  = 0;*/
                    }
break;
case 419:
/* # line 2735 "grammar.y" */ 
	{
                      yTrace(vps, "qword_list: | qword_list tSTRING_CONTENT tSPACE");
                      OmScopeType aScope(vps->omPtr);
                      NODE **strH = aScope.add(RubyStrNode::s(yymarkPtr[-1].obj, vps));
                      yyvalO = RubyArrayNode::append(yymarkPtr[-2].obj, *strH, vps);/* returns first arg*/
                    }
break;
case 420:
/* # line 2744 "grammar.y" */ 
	{
		      yTrace(vps, "string_contents: none");
		      yyvalO = RubyStrNode::s( om::NewString(vps->omPtr , 0), vps);
                    }
break;
case 421:
/* # line 2749 "grammar.y" */ 
	{
                      yTrace(vps, "string_contents: | string_contents string_content");
		      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 422:
/* # line 2756 "grammar.y" */ 
	{
                      yTrace(vps, "xstring_contents: none");
		      yyvalO = ram_OOP_NIL;
                    }
break;
case 423:
/* # line 2761 "grammar.y" */ 
	{
                      yTrace(vps, "xstring_contents: | xstring_contents string_content");
		      yyvalO = RubyParser::literal_concat(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 424:
/* # line 2768 "grammar.y" */ 
	{
                      yTrace(vps,  "string_content: tSTRING_CONTENT" );
	              yyvalO = RubyStrNode::s( yymarkPtr[0].obj, vps );
                    }
break;
case 425:
/* # line 2773 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | tSTRING_DVAR");
		      vps->lex_state = EXPR_BEG;
                      yyvalO = vps->clear_lex_strterm();
                    }
break;
case 426:
/* # line 2779 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | string_dvar");
		      vps->set_lex_strterm( yymarkPtr[-1].obj);
		      yyvalO = RubyEvStrNode::s(yymarkPtr[0].obj, vps);
                    }
break;
case 427:
/* # line 2785 "grammar.y" */ 
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
case 428:
/* # line 2795 "grammar.y" */ 
	{
                      yTrace(vps, "string_content: | tSTRING_DBEG ___ comp_stamt tRCURLY");
		      vps->set_lex_strterm( yymarkPtr[-2].obj);
                      rParenLexPop(vps);
		      yyvalO = RubyParser::new_evstr(yymarkPtr[-1].obj, vps);
                    }
break;
case 429:
/* # line 2804 "grammar.y" */ 
	{
                      yTrace(vps, "string_dvar: tGVAR");
                      yyvalO = RubyGlobalVarNode::s( quidToSymbolObj( yymarkPtr[0].obj, vps), vps);
                   }
break;
case 430:
/* # line 2809 "grammar.y" */ 
	{
                      yTrace(vps, "string_dvar: | tIVAR");
                      yyvalO = RubyInstVarNode::s( quidToSymbolObj( yymarkPtr[0].obj, vps), vps);
                   }
break;
case 431:
/* # line 2814 "grammar.y" */ 
	{
                      yTrace(vps, "string_dvar: | tCVAR");
                      yyvalO = RubyClassVarNode::s( quidToSymbolObj( yymarkPtr[0].obj, vps), vps);
                   }
break;
case 433:
/* # line 2822 "grammar.y" */ 
	{
                      yTrace(vps, "symbol: tSYMBEG sym");
		      vps->lex_state = EXPR_END;
		      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 438:
/* # line 2836 "grammar.y" */ 
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
case 441:
/* # line 2850 "grammar.y" */ 
	{
                      yTrace(vps, "numeric: tUMINUS_NUM tINTEGER");
                      om *omPtr = vps->omPtr;
                      OmScopeType aScope(omPtr);
                      NODE **aH = aScope.add(yymarkPtr[0].obj);
                      yyvalO = LrgNegate(omPtr, aH);
                    }
break;
case 442:
/* # line 2858 "grammar.y" */ 
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
case 448:
/* # line 2876 "grammar.y" */ 
	{ yyvalO = int64ToSi( kNIL) ; }
break;
case 449:
/* # line 2877 "grammar.y" */ 
	{ yyvalO = int64ToSi(kSELF); }
break;
case 450:
/* # line 2878 "grammar.y" */ 
	{ yyvalO = int64ToSi(kTRUE); }
break;
case 451:
/* # line 2879 "grammar.y" */ 
	{yyvalO = int64ToSi(kFALSE); }
break;
case 452:
/* # line 2880 "grammar.y" */ 
	{  yyvalO = int64ToSi(k__FILE__); }
break;
case 453:
/* # line 2881 "grammar.y" */ 
	{  yyvalO = int64ToSi(k__LINE__); }
break;
case 454:
/* # line 2885 "grammar.y" */ 
	{
                      yTrace(vps, "var_ref: variable");
                      yyvalO = gettable(vps, & yymarkPtr[0].obj);
                    }
break;
case 455:
/* # line 2892 "grammar.y" */ 
	{
                      yTrace(vps, "varLhs: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      yyvalO = assignable(& yymarkPtr[0].obj, ofsO, vps->nilH(), vps);
                    }
break;
case 456:
/* # line 2900 "grammar.y" */ 
	{
                    NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
		    yyvalO = RubyNthRefNode::s(yymarkPtr[0].obj/*a SmallInt*/, ofsO, vps);
                  }
break;
case 457:
/* # line 2905 "grammar.y" */ 
	{
		    yyvalO = RubyBackRefNode::s(yymarkPtr[0].obj/*a Character*/, vps);
                  }
break;
case 458:
/* # line 2911 "grammar.y" */ 
	{
                      yTrace(vps, "superclass: Term");
		      yyvalO = ram_OOP_NIL;
                    }
break;
case 459:
/* # line 2916 "grammar.y" */ 
	{
		      vps->lex_state = EXPR_BEG;
                    }
break;
case 460:
/* # line 2920 "grammar.y" */ 
	{
                      yTrace(vps, "superclass: | tLT expr_value Term");
                      yyvalO = yymarkPtr[-1].obj; 
                    }
break;
case 461:
/* # line 2924 "grammar.y" */ 
	{ yyerrflag = 0; yyvalO = ram_OOP_NIL;}
break;
case 462:
/* # line 2928 "grammar.y" */ 
	{
                      yTrace(vps, "f_arglist: tLPAREN2 f_args opt_nl tRPAREN");
                      rParenLexPop(vps);
		      yyvalO = yymarkPtr[-2].obj;
		      vps->lex_state = EXPR_BEG;
		      vps->command_start = TRUE;
                    }
break;
case 463:
/* # line 2936 "grammar.y" */ 
	{
                      yTrace(vps, "f_arglist: | f_args Term");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 464:
/* # line 2943 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: f_arg tCOMMA f_optarg tCOMMA f_rest_arg opt_f_block_arg");
		      RubyArgsNode::add_optional_arg(yymarkPtr[-5].obj, yymarkPtr[-3].obj, vps);
		      RubyArgsNode::add_star_arg(yymarkPtr[-5].obj, yymarkPtr[-1].obj, vps);
		      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-5].obj, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 465:
/* # line 2950 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_arg tCOMMA f_optarg  opt_f_block_arg");
                      RubyArgsNode::add_optional_arg(yymarkPtr[-3].obj, yymarkPtr[-1].obj, vps);
		      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 466:
/* # line 2956 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_arg tCOMMA  f_rest_arg opt_f_block_arg");
                      RubyArgsNode::add_star_arg(yymarkPtr[-3].obj, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-3].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 467:
/* # line 2962 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_arg  opt_f_block_arg");
                      yyvalO = RubyArgsNode::add_block_arg(yymarkPtr[-1].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 468:
/* # line 2967 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_optarg tCOMMA f_rest_arg opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_optional_arg(*argsH, yymarkPtr[-3].obj, vps);
                      RubyArgsNode::add_star_arg(*argsH, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 469:
/* # line 2976 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: |  f_optarg  opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_optional_arg(*argsH, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 470:
/* # line 2984 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | f_rest_arg opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_star_arg(*argsH, yymarkPtr[-1].obj, vps);
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 471:
/* # line 2992 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: |  f_blck_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      yyvalO = RubyArgsNode::add_block_arg(*argsH, yymarkPtr[0].obj, vps);
                    }
break;
case 472:
/* # line 2999 "grammar.y" */ 
	{
                      yTrace(vps, "f_args: | <nothing>");
		      yyvalO = RubyArgsNode::new_(vps);
                    }
break;
case 473:
/* # line 3006 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be a constant");
                    }
break;
case 474:
/* # line 3010 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be an instance variable");
                    }
break;
case 475:
/* # line 3014 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be a global variable");
                    }
break;
case 476:
/* # line 3018 "grammar.y" */ 
	{
                        rb_compile_error(vps, "formal argument cannot be a class variable");
                    }
break;
case 477:
/* # line 3022 "grammar.y" */ 
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
case 478:
/* # line 3037 "grammar.y" */ 
	{ yTrace(vps, "f_arg: f_norm_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      yyvalO = RubyArgsNode::add_arg(argsH, yymarkPtr[0].obj/*RpNameToken*/, vps);  /* returns first arg*/
                    }
break;
case 479:
/* # line 3044 "grammar.y" */ 
	{
                      yTrace(vps, "f_arg: | f_arg tCOMMA f_norm_arg");
                      yyvalO = RubyArgsNode::add_arg(& yymarkPtr[-2].obj, yymarkPtr[0].obj/*RpNameToken*/, vps); 
                    }
break;
case 480:
/* # line 3051 "grammar.y" */ 
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
case 481:
/* # line 3066 "grammar.y" */ 
	{
                      yTrace(vps, "f_optarg: f_opt");
                      yyvalO = RubyBlockNode::s( yymarkPtr[0].obj, vps);
                    }
break;
case 482:
/* # line 3071 "grammar.y" */ 
	{
                      yTrace(vps, "f_optarg: | f_optarg tCOMMA f_opt");
                      yyvalO = RubyBlockNode::append_to_block(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 485:
/* # line 3082 "grammar.y" */ 
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
case 486:
/* # line 3094 "grammar.y" */ 
	{
                      yTrace(vps, "f_rest_arg: | restarg_mark");
                      yyvalO = RpNameToken::s(a_sym_rest_args, vps);
                    }
break;
case 489:
/* # line 3105 "grammar.y" */ 
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
case 490:
/* # line 3119 "grammar.y" */ 
	{
                      yTrace(vps, "opt_f_block_arg: tCOMMA f_blck_arg");
                      yyvalO = yymarkPtr[0].obj;
                    }
break;
case 491:
/* # line 3124 "grammar.y" */ 
	{
                      yTrace(vps, "opt_f_block_arg: | <nothing>");
                      yyvalO = ram_OOP_NIL;
                    }
break;
case 492:
/* # line 3131 "grammar.y" */ 
	{
                        yTrace(vps, "singleton : var_ref");
                        yyvalO = yymarkPtr[0].obj;
                    }
break;
case 493:
/* # line 3135 "grammar.y" */ 
	{ vps->lex_state = EXPR_BEG;}
break;
case 494:
/* # line 3136 "grammar.y" */ 
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
case 495:
/* # line 3149 "grammar.y" */ 
	{
                      yTrace(vps, "assoc_list: none");
                      yyvalO = RubyArrayNode::new_(vps);
                    }
break;
case 496:
/* # line 3154 "grammar.y" */ 
	{
                      yTrace(vps, "assoc_list: | assocs trailer");
		      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 497:
/* # line 3159 "grammar.y" */ 
	{
                      yTrace(vps, "assoc_list: | args trailer");
                      if ((RubyArrayNode::arrayLength(yymarkPtr[-1].obj, vps) & 1) != 0) {
                        rb_compile_error("odd number list for Hash", vps);
                      }
                      yyvalO = yymarkPtr[-1].obj;
                    }
break;
case 499:
/* # line 3170 "grammar.y" */ 
	{
                      yTrace(vps, "assocs: | assocs tCOMMA assoc");
                      yyvalO = RubyArrayNode::appendAll(yymarkPtr[-2].obj, yymarkPtr[0].obj, vps); /* returns first arg*/
                    }
break;
case 500:
/* # line 3177 "grammar.y" */ 
	{
                      yTrace(vps, "assoc: arg_value tASSOC arg_value");
                      yyvalO = RubyArrayNode::s_a_b( yymarkPtr[-2].obj, yymarkPtr[0].obj, vps);
                    }
break;
case 520:
/* # line 3216 "grammar.y" */ 
	{ yyerrflag = 0 ;}
break;
case 523:
/* # line 3221 "grammar.y" */ 
	{ yyerrflag = 0;}
break;
case 524:
/* # line 3224 "grammar.y" */ 
	{  yTrace(vps, "none:");  yyvalO = ram_OOP_NIL; }
break;
/* # line 12205 "rubygrammar.c" */ 
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
