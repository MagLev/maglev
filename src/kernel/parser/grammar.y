/**********************************************************************

  parse.y -

  $Author: matz $
  $Date: 2004/11/29 06:13:51 $
  created at: Fri May 28 18:02:42 JST 1993

  Copyright (C) 1993-2003 Yukihiro Matsumoto

  Copied from Rubinius source distribution to Maglev sources on 30 July 2010

  Maglev notes:
    file grammar.y
    To be used with Maglev's modified version of byacc

**********************************************************************/

%{

#define YYDEBUG 1
#define YYERROR_VERBOSE 1

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <limits.h>

// include <assert.h>
//   use Maglev VM assertion support
#define assert UTL_ASSERT

#include "rubyom.hf"
#include "rubyparser.h"
#include "rubyast.hf"
#include "gcifloat.hf"
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

// ID_SCOPE_MASK , ID_LOCAL..ID_INTERNAL moved to parser.h


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
    UTL_ASSERT( om::isSymbol(symO)) ;
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

// static BoolType is_attrset_id(QUID id)
// {
//   int64 val = QUID_to_id(id);
//   return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_ATTRSET && v_is_notop_id(val);
// }

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

// static BoolType is_junk_id(QUID id)
// {
//   int64 val = QUID_to_id(id);
//   return ((val >> ID_SCOPE_SHIFT) & ID_SCOPE_MASK)== ID_JUNK && v_is_notop_id(val);
// }

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
  YyStackElement *base = (YyStackElement*)malloc(numBytes);
  int64 depth = -1;
  if (stk->stacksize > 0) {
    depth = stk->mark - stk->base;
    memcpy(base, stk->base, sizeof(YyStackElement)*stk->stacksize);
    free(stk->base);
  };
  stk->base = base;
  stk->mark = base + depth;
  stk->last = base + newSize - 1 ;
  stk->stacksize = newSize;
  for (YyStackElement *elem = stk->mark + 1; elem <= stk->last; elem++) {
    // initialize newly allocated memory
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
  return; // place to set breakpoint
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
  // no underflow check
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
  ps->cmdarg_stack.lexPop(); // no underflow check
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
  // maglev had premature_eof at call sites
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
// -----------------------------------------

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
  int64 v = om::FetchSmallInt__(*objH, nest_ofs);
  v += delta;
  om::StoreSmallInt_(ps->omPtr, objH, nest_ofs, v);
  return v;
}

static NODE* NEW_STRTERM(short func, int term, int paren, rb_parse_state *ps)
{
  return RubyLexStrTerm::newStrTerm(func, term, paren, ps);  // nest set to zero
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

%}

%pure-parser

// union not used

%token  kCLASS
        kMODULE
        kDEF
        kUNDEF
        kBEGIN
        kRESCUE
        kENSURE
        kEND
        kIF
        kUNLESS
        kTHEN
        kELSIF
        kELSE
        kCASE
        kWHEN
        kWHILE
        kUNTIL
        kFOR
        kBREAK
        kNEXT
        kREDO
        kRETRY
        kIN
        kDO
        kDO_COND
        kDO_BLOCK
        kRETURN
        kYIELD
        kSUPER
        kSELF
        kNIL
        kTRUE
        kFALSE
        kAND
        kOR
        kNOT
        kIF_MOD
        kUNLESS_MOD
        kWHILE_MOD
        kUNTIL_MOD
        kRESCUE_MOD
        kALIAS
        kDEFINED
        klBEGIN
        klEND
        k__LINE__
        k__FILE__

%token <id>   tIDENTIFIER tFID tGVAR tIVAR tCONSTANT tCVAR tXSTRING_BEG tLABEL
%token <node> tINTEGER tFLOAT tSTRING_CONTENT tCHAR
%token <node> tNTH_REF tBACK_REF
%token <num>  tREGEXP_END
%type <node> singleton strings string string1 xstring regexp
%type <node> string_contents xstring_contents string_content
%type <node> words qwords word_list qword_list word
%type <node> literal numeric dsym cpath
%type <node> bodystmt compstmt stmts stmt expr arg primary command command_call method_call
%type <node> expr_value arg_value primary_value
%type <node> if_tail opt_else case_body cases opt_rescue exc_list exc_var opt_ensure
%type <node> args when_args call_args call_args2 open_args paren_args opt_paren_args
%type <node> command_args aref_args opt_block_arg block_arg var_ref var_lhs
%type <node> mrhs superclass block_call block_command
%type <node> f_arglist f_args f_optarg f_opt f_block_arg opt_f_block_arg
%type <node> assoc_list assocs assoc undef_list backref string_dvar
%type <node> for_var block_var opt_block_var block_par
%type <node> brace_block cmd_brace_block do_block lhs none fitem
%type <node> mlhs mlhs_head mlhs_basic mlhs_entry mlhs_item mlhs_node
// deleted fsym from next line
%type <id>   variable sym symbol operation operation2 operation3
%type <id>   cname fname op f_rest_arg
%type <num>  f_norm_arg f_arg
%token tUPLUS           /* unary+ */
%token tUMINUS          /* unary- */
%token tPOW             /* ** */
%token tCMP             /* <=> */
%token tEQ              /* == */
%token tEQQ             /* === */
%token tNEQ             /* != */
%token tGEQ             /* >= */
%token tLEQ             /* <= */
%token tANDOP tOROP     /* && and || */
%token tMATCH tNMATCH   /* =~ and !~ */
%token tDOT2 tDOT3      /* .. and ... */
%token tAREF tASET      /* [] and []= */
%token tLSHFT tRSHFT    /* << and >> */
%token tCOLON2          /* :: */
%token tCOLON3          /* :: at EXPR_BEG */
%token <id> tOP_ASGN    /* +=, -=  etc. */
%token tASSOC           /* => */
%token tLPAREN          /* ( */
%token tLPAREN_ARG      /* ( */
%token tRPAREN          /* ) */
%token tLBRACK          /* [ */
%token tLBRACE          /* { */
%token tLBRACE_ARG      /* { */
%token tSTAR            /* * */
%token tAMPER           /* & */
%token tSYMBEG tSTRING_BEG tREGEXP_BEG tWORDS_BEG tQWORDS_BEG
%token tSTRING_DBEG tSTRING_DVAR tSTRING_END

/* RubyParser uses tAWORDS_BEG instead of tQWORDS_BEG */


/*
 *      precedence table
 */

%nonassoc tLOWEST
%nonassoc tLBRACE_ARG

%nonassoc  kIF_MOD kUNLESS_MOD kWHILE_MOD kUNTIL_MOD
%left  kOR kAND
%right kNOT
%nonassoc kDEFINED
%right '=' tOP_ASGN
%left kRESCUE_MOD
%right '?' ':'
%nonassoc tDOT2 tDOT3
%left  tOROP
%left  tANDOP
%nonassoc  tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
%left  '>' tGEQ '<' tLEQ
%left  '|' '^'
%left  '&'
%left  tLSHFT tRSHFT
%left  '+' '-'
%left  '*' '/' '%'
%right tUMINUS_NUM tUMINUS
%right tPOW
%right '!' '~' tUPLUS

%token tLAST_TOKEN

%%
program         :  {
                        yTrace(vps,  "program: " );
                        vps->lex_state = EXPR_BEG;
                        vps->variables = LocalState::allocate(vps);
                        vps->class_nest = 0;
                    }
                  compstmt
                    {
                        //if ($2 && !compile_for_eval) ...
                        //     last expression should not be void  ...
                        //    maglev does this in AST to IR generation
                        yTrace(vps,  "program: comp_stamt");
                        vps->class_nest = 0;
                        $$  =  $2;
                    }
                ;

bodystmt        : compstmt
                  opt_rescue
                  opt_else
                  opt_ensure
                    {
                        yTrace(vps, "body_stamt: comp_stamt ");
                        OmScopeType scp(vps->omPtr);
                        NODE **resH = scp.add($1);
                        if ($2 != ram_OOP_NIL) {
                            *resH = RubyRescueNode::s($1, $2, $3, ram_OOP_NIL, vps);
                        } else if ($3 != ram_OOP_NIL) {
                            rb_warning(vps, "else without rescue is useless");
                            *resH = RubyParser::block_append(*resH, $3, vps);
                        }
                        if ($4 != ram_OOP_NIL) {  // 4 is a RubyEnsureNode
                            // $4 is receiver block of rubyEnsure:
                            RubyEnsureNode::set_body( $4, *resH, vps );
                            *resH = $4;
                        }
                        $$ = *resH;
                        // fixpos($$, $1);
                    }
                ;

compstmt        : stmts opt_terms
                    {
                        // void_stmts($1, vps);
                      yTrace(vps, "comp_stamt: sttmts opt_termms");
                        $$ = $1;
                    }
                ;

stmts           : none
                | stmt
                    {
                        // $$  =  newline_node(vps, $1);
                        $$ = $1; // maglev does not use newline nodes
                    }
                | stmts terms stmt
                    {
                        // $$  =  block_append(vps, $1, newline_node(vps, $3));
                        yTrace(vps, "sttmts: | sttmts terms stmt ");
                        $$ = RubyParser::block_append($1, $3, vps);
                    }
                | error stmt
                    {
                        // $$  = remove_begin($2, vps);
                      yTrace(vps, "sttmts: | error stmt");
                      $$ = $2;
                    }
                ;

stmt            : kALIAS fitem {vps->lex_state = EXPR_FNAME;} fitem
                    {
                        // $$  = NEW_ALIAS($2, $4);
                      yTrace(vps, "stmt: kALIAS fitem");
                      yTrace(vps, "stmt: fitem");
                        $$ = RubyAliasNode::s(& $2, & $4, $1/*alias token*/, vps);
                    }
                | kALIAS tGVAR tGVAR
                    {
                      yTrace(vps, "stmt: | kALIAS tGVAR tGVAR");
                        OmScopeType aScope(vps->omPtr);
                        NODE **aH = aScope.add( quidToSymbolObj($2, vps));
                        NODE **bH = aScope.add( quidToSymbolObj($3, vps));
                        $$ = RubyGlobalVarAliasNode::s(*aH, *bH, vps);
                    }
                | kALIAS tGVAR tBACK_REF
                    {
                      yTrace(vps, "stmt: | kALIAS tGVAR tBACK_REF");
                        char buf[3];
                        uint ch = 'x';
                        if (OOP_IS_CHARACTER($3)) {
                          ch = OOP_TO_CHAR_($3);
                        } else {
                          rb_compile_error(vps, "invalid tBACK_REF value in kALIAS tGVAR tBACK_REF");
                        }
                        snprintf(buf, sizeof(buf), "$%c", (char)ch );
                        om *omPtr = vps->omPtr;
                        OmScopeType aScope(omPtr);
                        NODE **symH = aScope.add( ObjNewSym(omPtr, buf));
                        NODE **aH = aScope.add( quidToSymbolObj($2, vps));
                        $$ = RubyGlobalVarAliasNode::s( *aH, *symH, vps);
                    }
                | kALIAS tGVAR tNTH_REF
                    {
                        rb_compile_error(vps, "can't make alias for the number variables");
                        $$ = 0;
                    }
                | kUNDEF undef_list
                    {
                      yTrace(vps, "stmt: | kUNDEF undef_list");
                        $$ = $2;
                    }
                | stmt kIF_MOD expr_value
                    {
                      yTrace(vps, "stmt: | stmt kIF_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      $$ = RubyParser::new_if( $3 , $1, ram_OOP_NIL, srcOfs, vps );
                    }
                | stmt kUNLESS_MOD expr_value
                    {
                      yTrace(vps, "stmt: | stmt kWHILE_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      $$ = RubyParser::new_if( $3, ram_OOP_NIL, $1 , srcOfs, vps );
                    }
                | stmt kWHILE_MOD expr_value
                    {
                      yTrace(vps, "stmt: | stmt kWHILE_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      $$ = RubyParser::new_while( $1 , $3 , srcOfs, vps);
                    }
                | stmt kUNTIL_MOD expr_value
                    {
                      yTrace(vps, "stmt: | stmt kUNTIL_MOD expr_value");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      $$ = RubyParser::new_until( $1 , $3 , srcOfs, vps);
                    }
                | stmt kRESCUE_MOD stmt
                    {
                      yTrace(vps, "stmt: | stmt kRESCUE_MOD stmt");
                        OmScopeType aScope(vps->omPtr);
                        omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                        NODE **rescueBodyH = aScope.add(
                          RubyRescueBodyNode::s(ram_OOP_NIL, $3, ram_OOP_NIL, srcOfs, vps));
                        $$ = RubyRescueNode::s( $1, *rescueBodyH, ram_OOP_NIL, srcOfs, vps);
                    }
                | klBEGIN
                    {
                      yTrace(vps, "stmt: | klBEGIN");
                        if (vps->in_def || vps->in_single) {
                            rb_compile_error(vps, "BEGIN in method");
                        }
                        local_push(vps, 0);
                    }
                  '{' compstmt '}'
                    {
                       // ruby_eval_tree_begin = block_append(ruby_eval_tree_begin, NEW_PREEXE($4));
                       yTrace(vps, "stmt: ___ tLCURLY comp_stamt tRCURLY");
                       rParenLexPop(vps);
                       local_pop(vps);
                       $$ = ram_OOP_NIL ;
                    }
                | klEND '{' compstmt '}'
                    {
                       yTrace(vps, "stmt: | klEND tLCURLY comp_stamt tRCURLY");
                       rParenLexPop(vps);
                       if (vps->in_def || vps->in_single) {
                            rb_warning(vps, "END in method; use at_exit");
                       }
                       $$ = RubyIterRpNode::s(ram_OOP_NIL/*no block args*/, $3, $2/*srcOffsetSi*/,
                                                vps, 1/* strlen( '}' ) */ );
                    }
                | lhs '=' command_call
                    {
                      yTrace(vps, "stmt: | lhs tEQL command_call");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      $$ = RubyParser::node_assign(& $1, srcOfs, $3, vps);
                    }
                | mlhs '=' command_call
                    {
                      yTrace(vps, "stmt: | mLhs tEQL command_call");
                        $$ = RubyParser::masgn_append_arg( $1 , $3, vps );
                    }
                | var_lhs tOP_ASGN command_call
                    {
                        if ($1 != ram_OOP_NIL) {
                           yTrace(vps, "stmt: | varLhs tOP_ASGN command_call");
                           $$ = RubyParser::new_op_asgn($1, $2/*RpNameToken*/, $3, vps);
                        } else {
                           yTrace(vps, "stmt: | NIL_LHS tOP_ASGN command_call");
                           $$ = ram_OOP_NIL;
                        }
                    }
                | primary_value ary_ref tOP_ASGN command_call
                    {
                      yTrace(vps, "stmt: | primary_value tLBRACK_STR aref__args tRBRACK tOP_ASGN command_call");
                      omObjSType *aref_args = om::FetchOop($2, 0);
                      $$ = RubyOpElementAsgnNode::s($1, aref_args, $3, $4, vps);
                    }
                | primary_value '.' tIDENTIFIER tOP_ASGN command_call
                    {
                      yTrace(vps, "stmt: | primary_value tDOT tIDENTIFIER tOP_ASGN command_call");
                      // not seen with Ryan's grammar and 1.8.7
                      $$ = RubyOpAsgnNode::s($1, $3, $4, $5, vps);
                    }
                | primary_value '.' tCONSTANT tOP_ASGN command_call
                    {
                      yTrace(vps, "stmt: | primary_value tDOT tCONSTANT tOP_ASGN command_call");
                      // not seen with Ryan's grammar and 1.8.7
                      $$ = RubyOpAsgnNode::s($1, $3, $4, $5, vps);
                    }
                | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call
                    {
                      yTrace(vps, "stmt: | primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call");
                      // not seen with Ryan's grammar and 1.8.7
                      $$ = RubyOpAsgnNode::s($1, $3, $4, $5, vps);
                    }
                | backref tOP_ASGN command_call
                    {
                        yTrace(vps, "stmt: | backref tOP_ASGN command_call");
                        rb_backref_error($1, vps);
                        $$ = ram_OOP_NIL;
                    }
                | lhs '=' mrhs
                    {
                        yTrace(vps, "stmt: | lhs tEQL mrhs");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubySValueNode::s($3, vps));
                        omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                        $$ = RubyParser::node_assign(& $1, srcOfs, *valH, vps);
                    }
                | mlhs '=' arg_value
                    {
                        yTrace(vps, "stmt: | mLhs tEQL arg_value");
                        $$ = RubyParser::masgn_append_arg($1, $3, vps);
                    }
                | mlhs '=' mrhs
                    {
                        yTrace(vps, "stmt: | mLhs tEQL mrhs");
                        $$ = RubyParser::masgn_append_mrhs($1, $3, vps);
                    }
                | expr
                ;

expr            : command_call
                | expr kAND expr
                    {
                        yTrace(vps, "expr: | expr kAND expr");
                        OmScopeType aScope(vps->omPtr);
                        NODE **clsH = aScope.add( RubyAndNode::cls(vps));
                        $$ = RubyParser::logop(*clsH, $1, $3, vps);
                    }
                | expr kOR expr
                    {
                        yTrace(vps, "expr: | expr kOR expr");
                        OmScopeType aScope(vps->omPtr);
                        NODE **clsH = aScope.add( RubyOrNode::cls(vps));
                        $$ = RubyParser::logop(*clsH, $1, $3, vps);
                    }
                | kNOT expr
                    {
                        yTrace(vps, "expr: | kNOT expr");
                        $$ = RubyNotNode::s( $2, vps);
                    }
                | '!' command_call
                    {
                        yTrace(vps, "expr: | tBANG command_call");
                        $$ = RubyNotNode::s( $2, vps);
                    }
                | arg
                ;

expr_value      : expr
                    {
                        yTrace(vps, "expr_value: expr");
                        $$ = RubyParser::value_expr($1, vps);
                    }
                ;

command_call    : command
                | block_command
                | kRETURN call_args
                    {
                        yTrace(vps, "command_call: kRETURN call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args($2, vps));
                        $$ = RubyReturnNode::s(valH, $1/*kRETURN token*/, vps);
                    }
                | kBREAK call_args
                    {
                        yTrace(vps, "command_call: | kBREAK call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args($2, vps));
                        $$ = RubyBreakNode::s( valH, $1/*kBREAK token*/, vps);
                    }
                | kNEXT call_args
                    {
                        yTrace(vps, "command_call: | kNEXT call_args");
                        OmScopeType aScope(vps->omPtr);
                        NODE **valH = aScope.add(RubyParser::ret_args($2, vps));
                        $$ = RubyNextNode::s( valH, $1/*kNEXT token*/, vps);
                    }
                ;

block_command   : block_call
                | block_call '.' operation2 command_args
                    {
                        yTrace(vps, "block_command: block_call...");
                        $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                | block_call tCOLON2 operation2 command_args
                    {
                        yTrace(vps, "block_command: | block_call tCOLON2 operation2 command_args");
                        $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                ;

cmd_brace_block : tLBRACE_ARG
                    {
                        yTrace(vps, "cmd_brace_block: tLBRACE_ARG");
                        reset_block(vps);
                        // $1 = int64ToSi(vps->ruby_sourceline() );
                    }
                  opt_block_var
                    {
                       $$ = ram_OOP_NIL; // getBlockVars not used
                    }
                  compstmt
                  '}'
                    {
                      yTrace(vps, "cmd_brace_block: ___ comp_stamt tRCURLY");
                      rParenLexPop(vps);
                      popBlockVars(vps);
                      $$ = RubyIterRpNode::s( $3/*masgn from opt_block_var*/ , $5/*compstmp*/, $1/*srcOffsetSi*/,
                                                vps, 1/* strlen( '}' ) */ );
                    }
                ;

command         : operation command_args       %prec tLOWEST
                    {
                      yTrace(vps, "command: operation command_args =tLOWEST");
                        $$ = RubyParser::new_fcall($1, $2, vps);
                   }
                | operation command_args cmd_brace_block
                    {
                      yTrace(vps, "command: | operation command_args cmd_brace_block");
                      $$ = RubyParser::new_fcall_braceBlock($1, $2, $3, vps);
                   }
                | primary_value '.' operation2 command_args     %prec tLOWEST
                    {
                      yTrace(vps, "command: | primary_value tDOT operation2 command_args =tLOWEST");
                      $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                | primary_value '.' operation2 command_args cmd_brace_block
                    {
                      yTrace(vps, "command: | primary_value tDOT operation2 command_args cmd_brace_block");
                      $$ = RubyParser::new_call_braceBlock($1, $3, $4, $5, vps);
                    }
                | primary_value tCOLON2 operation2 command_args %prec tLOWEST
                    {
                      yTrace(vps, "command: | primary_value tCOLON2 operation2 command_args =tLOWEST");
                      $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                | primary_value tCOLON2 operation2 command_args cmd_brace_block
                    {
                      yTrace(vps, "command: | primary_value tCOLON2 operation2 command_args cmd_brace_block");
                      $$ = RubyParser::new_call_braceBlock($1, $3, $4, $5, vps);
                   }
                | kSUPER command_args
                    {
                      yTrace(vps, "command: | kSUPER command_args");
                      $$ = RubyParser::new_super(& $2, $1/*super token*/, vps);
                    }
                | kYIELD command_args
                    {
                      yTrace(vps, "command: | kYIELD command_args");
                      $$ = RubyParser::new_yield(& $2, $1/*yield token*/, vps);
                    }
                ;

mlhs            : mlhs_basic
                | tLPAREN mlhs_entry ')'
                    {
                      yTrace(vps, "mLhs: | tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      $$ = $2;
                    }
                ;

mlhs_entry      : mlhs_basic
                | tLPAREN mlhs_entry ')'
                    {
                      yTrace(vps, "mlhs_entry: | tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyArrayNode::s($2, vps));
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1);
                      $$ = RubyParser::new_parasgn( *valH, srcOfs, vps);
                    }
                ;

mlhs_basic      : mlhs_head
                    {
                      yTrace(vps, "mlhs_basic: mlhs_head ");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      $$ = RubyParser::new_parasgn( $1, ofsO, vps);
                    }
                | mlhs_head mlhs_item
                    {
                      yTrace(vps, "mlhs_basic: | mlhs_head mlhs_item");
                      OmScopeType aScope(vps->omPtr);
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      NODE *valO = RubyArrayNode::append_for_mlhs( $1, $2, vps);
                      $$ = RubyParser::new_parasgn(valO, ofsO, vps);
                    }
                | mlhs_head tSTAR mlhs_node
                    {
                      yTrace(vps, "mlhs_basic: | mlhs_head tSTAR mlhs_node");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s($3, vps));
                      *valH = RubyArrayNode::append_for_mlhs( $1, *valH, vps);
                      $$ = RubyParser::new_parasgn($1, $2/*srcOffsetSi*/, vps);
                    }
                | mlhs_head tSTAR
                    {
                      yTrace(vps, "mlhs_basic: | mlhs_head tSTAR");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      *valH = RubyArrayNode::append_for_mlhs( $1, *valH, vps);
                      $$ = RubyParser::new_parasgn( $1, $2/*srcOffsetSi*/, vps);
                    }
                | tSTAR mlhs_node
                    {
                      yTrace(vps, "mlhs_basic: | tSTAR mlhs_node");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s($2, vps));
                      *valH = RubyArrayNode::s( *valH, vps);
                      $$ = RubyParser::new_parasgn( *valH, $1/*srcOffsetSi*/, vps);
                    }
                | tSTAR
                    {
                      yTrace(vps, "mlhs_basic: | tSTAR");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      *valH = RubyArrayNode::s( *valH, vps);
                      $$ = RubyParser::new_parasgn( *valH, $1/*srcOffsetSi*/, vps);
                    }
                ;

mlhs_item       : mlhs_node
                | tLPAREN mlhs_entry ')'
                    {
                      yTrace(vps, "mlhs_item: tLPAREN mlhs_entry tRPAREN");
                      rParenLexPop(vps);
                      $$ = $2;
                    }
                ;

mlhs_head       : mlhs_item ','
                    {
                      yTrace(vps, "mlhs_head: mlhs_item tCOMMA");
                      $$ = RubyArrayNode::s( $1, vps);
                    }
                | mlhs_head mlhs_item ','
                    {
                      yTrace(vps, "mlhs_head: | mlhs_head mlhs_item tCOMMA");
                      $$ = RubyArrayNode::append_for_mlhs($1, $2, vps); // result is $1
                    }
                ;

ary_ref         : '[' aref_args ']'
                   {
                     rParenLexPop(vps);
                     om *omPtr = vps->omPtr;
                     OmScopeType scp(omPtr);
                     omObjSType **resH = scp.add(om::NewArray(omPtr, 2));
                     om::StoreOop(omPtr, resH, 0, & $2 );
                     om::StoreOop(omPtr, resH, 1, & $3 /*srcOffsetSi*/);
                     $$ = *resH;
                   }

mlhs_node       : variable
                    {
                      yTrace(vps, "mlhs_node: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      $$ = assignable(& $1, ofsO, vps->nilH(), vps);
                    }
                | primary_value ary_ref
                    {
                      yTrace(vps, "mlhs_node: | primary_value tLBRACK_STR aref__args tRBRACK");
                      omObjSType *srcOfs = om::FetchOop($2, 1); // no gc
                      omObjSType *aref_args = om::FetchOop($2, 0);
                      $$ = RubyAttrAssignNode::s($1, ram_OOP_NIL/*"[]="*/, aref_args, srcOfs, vps);
                    }
                | primary_value '.' tIDENTIFIER
                    {
                      yTrace(vps, "mlhs_node: | primary_value tDOT tIDENTIFIER");
                      $$ = RubyAttrAssignNode::s($1, $3/*RpNameToken*/, ram_OOP_NIL,
                                                        ram_OOP_NIL, vps);
                    }
                | primary_value tCOLON2 tIDENTIFIER
                    {
                      yTrace(vps, "lhs: | primary_value tCOLON2 tIDENTIFIER");
                      $$ = RubyAttrAssignNode::s($1, $3/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
                | primary_value '.' tCONSTANT
                    {
                      yTrace(vps, "lhs: | primary_value tDOT tCONSTANT");
                      $$ = RubyAttrAssignNode::s($1, $3/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
                | primary_value tCOLON2 tCONSTANT
                    {
                      yTrace(vps, "lhs: | primary_value tCOLON2 tCONSTANT");
                      if (vps->in_def || vps->in_single) {
                         rb_compile_error(vps, "dynamic constant assignment");
                      }
                      $$ = RubyConstDeclNode::colon2($1, $3/*RpNameToken*/, vps);
                    }
                | tCOLON3 tCONSTANT
                    {
                      if (vps->in_def || vps->in_single) {
                          rb_compile_error(vps, "dynamic constant assignment");
                      }
                      $$ = RubyConstDeclNode::colon3( $2/*RpNameToken*/, vps);
                    }
                | backref
                    {
                      rb_backref_error($1, vps);
                      $$ = ram_OOP_NIL;
                    }
                ;

lhs             : variable
                    {
                      yTrace(vps, "lhs: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      $$ = assignable(& $1, ofsO, vps->nilH(), vps);
                    }
                | primary_value ary_ref
                    {
                      yTrace(vps, "lhs: | primary_value tLBRACK_STR aref__args tRBRACK");
                      /* rParenLexPop(vps); */ /* Fix GitHub issue #148, ary_ref pops already */
                      omObjSType *srcOfs = om::FetchOop($2, 1); // no gc
                      omObjSType *aref_args = om::FetchOop($2, 0);
                      $$ = RubyAttrAssignNode::s($1, ram_OOP_NIL/*"[]="*/, aref_args, srcOfs, vps);
                    }
                | primary_value '.' tIDENTIFIER
                    {
                      yTrace(vps, "lhs: | primary_value tDOT tIDENTIFIER");
                      $$ = RubyAttrAssignNode::s($1, $3/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
                | primary_value tCOLON2 tIDENTIFIER
                    {
                      yTrace(vps, "lhs: | primary_value tCOLON2 tIDENTIFIER");
                      $$ = RubyAttrAssignNode::s($1, $3/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
                | primary_value '.' tCONSTANT
                    {
                      yTrace(vps, "lhs: | primary_value tDOT tCONSTANT");
                      $$ = RubyAttrAssignNode::s($1, $3/*RpNameToken*/, ram_OOP_NIL, ram_OOP_NIL, vps);
                    }
                | primary_value tCOLON2 tCONSTANT
                    {
                      yTrace(vps, "lhs: | primary_value tCOLON2 tCONSTANT");
                      if (vps->in_def || vps->in_single) {
                          rb_compile_error(vps, "dynamic constant assignment");
                      }
                      $$ = RubyConstDeclNode::colon2($1, $3/*RpNameToken*/, vps);
                    }
                | tCOLON3 tCONSTANT
                    {
                      if (vps->in_def || vps->in_single) {
                          rb_compile_error(vps, "dynamic constant assignment");
                      }
                      OmScopeType aScope(vps->omPtr);
                      $$ = RubyConstDeclNode::colon3( $2/*RpNameToken*/, vps);
                    }
                | backref
                    {
                        rb_backref_error($1, vps);
                        $$ = ram_OOP_NIL;
                    }
                ;

cname           : tIDENTIFIER
                    {
                      yTrace(vps, "cname: tIDENTIFIER");
                      rb_compile_error(vps, "class/module name must be CONSTANT");
                    }
                | tCONSTANT
                ;

cpath           : tCOLON3 cname
                    {
                      yTrace(vps, "cpath: tCOLON3 cname");
                      // $$  = NEW_COLON3($2);
                      $$ = RubyColon3Node::s($2/*RpNameToken*/, vps);
                    }
                | cname
                    {
                      yTrace(vps, "cpath: | cname");
                      // $$  = NEW_COLON2(0, $$);
                      $$ = $1 ; // a RpNameToken
                    }
                | primary_value tCOLON2 cname
                    {
                      yTrace(vps, "cpath: | primary_value tCOLON2 cname");
                      // $$  = NEW_COLON2($1, $3);
                      $$ = RubyColon2Node::s($1, $3, vps);
                    }
                ;

fname           : tIDENTIFIER
                | tCONSTANT
                | tFID
                | op
                    {
                      yTrace(vps, "fname: tIDENTIFIER | tCONSTANT | tFID | op");
                      vps->lex_state = EXPR_END;
                      // $$  = convert_op($1);
                      $$ = $1; // a RpNameToken
                    }
                | reswords
                    {
                      yTrace(vps, "fname: | reswords");
                      vps->lex_state = EXPR_END;
                      // $$  = $<id>1;
                      $$ = $1; // a RpNameToken or a String
                    }
                ;

fitem           : fname
                    {  // deleted  fsym  : fname
                       //                | symbol
                       //                ;
                       yTrace(vps, "fitem: fname");
                       $$ = RubySymbolNode::s( RpNameToken::symval($1/*RpNameToken*/, vps), vps);
                    }

fitem           : symbol
                    {
                       yTrace(vps, "fitem: | symbol");
                       // $$  = NEW_LIT(QUID2SYM($1));
                       $$ = RubySymbolNode::s( $1/*a Symbol*/, vps);
                    }
                | dsym
                ;

undef_list      : fitem
                    {
                      yTrace(vps, "undef_list: fitem");
                      $$ = RubyParser::new_undef( $1/*a RubySymbolNode*/, vps);
                    }
                | undef_list ',' {vps->lex_state = EXPR_FNAME;} fitem
                    {
                      yTrace(vps, "undef_list: ___ fitem");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::new_undef( $4, vps));
                      $$ = RubyParser::block_append($1, *valH, vps);
                    }
                ;

op              : '|'    { yTrace(vps, "op |");    $$ = RpNameToken::s(a_sym_orOp, $1, vps); }
                | '^'    { yTrace(vps, "op ^");    $$ = RpNameToken::s( a_sym_upArrow, $1, vps); }
                | '&'    { yTrace(vps, "op &");    $$ = RpNameToken::s(a_sym_andOp, $1, vps); }
                | tCMP   { yTrace(vps, "op tCMP"); $$ = $1/*a RpNameToken*/; }
                | tEQ    { yTrace(vps, "op tEQ");  $$ = $1/*a RpNameToken*/; }
                | tEQQ   { yTrace(vps, "op tEQQ"); $$ = $1/*a RpNameToken*/; }
                | tMATCH { yTrace(vps, "op tMATCH"); $$ = RpNameToken::s(a_sym_tMATCH, $1, vps); }
                | '>'    { yTrace(vps, "op >");    $$ = RpNameToken::s(a_sym_gt, $1, vps); }
                | tGEQ   { yTrace(vps, "op tGEQ"); $$ = $1/*a RpNameToken*/; }
                | '<'    { yTrace(vps, "op <");    $$ = RpNameToken::s( a_sym_lt, $1, vps); }
                | tLEQ   { yTrace(vps, "op tLEQ"); $$ = $1/*a RpNameToken*/; }
                | tLSHFT { yTrace(vps, "op tLSHFT"); $$ = $1/*a RpNameToken*/; }
                | tRSHFT { yTrace(vps, "op tRSHFT"); $$ = $1/*a RpNameToken*/; }
                | '+'    { yTrace(vps, "op +");    $$ = RpNameToken::s(a_sym_plus, $1, vps); }
                | '-'    { yTrace(vps, "op -");    $$ = RpNameToken::s(a_sym_minus, $1, vps); }
                | '*'    { yTrace(vps, "op *");    $$ = RpNameToken::s( a_sym_star, $1, vps); }
                | tSTAR  { yTrace(vps, "op tSTAR"); $$ = RpNameToken::s( a_sym_star, $1, vps); }
                | '/'    { yTrace(vps, "op /");    $$ = RpNameToken::s( a_sym_div, $1, vps); }
                | '%'    { yTrace(vps, "op %");    $$ = RpNameToken::s( a_sym_percent, $1, vps); }
                | tPOW   { yTrace(vps, "op tPOW"); $$ = RpNameToken::s( a_sym_tPOW, $1, vps); }
                | '~'    { yTrace(vps, "op ~");    $$ = RpNameToken::s(a_sym_tilde, $1, vps); }
                | tUPLUS { yTrace(vps, "op tUPLUS"); $$ = RpNameToken::s( a_sym_tUPLUS, $1, vps);}
                | tUMINUS { yTrace(vps, "op tUMINUS"); $$ = RpNameToken::s(a_sym_tUMINUS, $1, vps);; }
                | tAREF  { yTrace(vps, "op tAREF"); $$ = RpNameToken::s(a_sym_tAREF, $1, vps); }
                | tASET  { yTrace(vps, "op tASET"); $$ = RpNameToken::s(a_sym_tASET, $1, vps); }
                | '`'    { yTrace(vps, "op `");    $$ = RpNameToken::s( a_sym_backtick, $1, vps); }
                ;

reswords        : k__LINE__ | k__FILE__  | klBEGIN | klEND
                | kALIAS | kAND | kBEGIN | kBREAK | kCASE | kCLASS | kDEF
                | kDEFINED | kDO | kELSE | kELSIF | kEND | kENSURE | kFALSE
                | kFOR | kIN | kMODULE | kNEXT | kNIL | kNOT
                | kOR | kREDO | kRESCUE | kRETRY | kRETURN | kSELF | kSUPER
                | kTHEN | kTRUE | kUNDEF | kWHEN | kYIELD
                | kIF | kUNLESS | kWHILE | kUNTIL
                ;

arg             : lhs '=' arg
                    {
                      yTrace(vps, "arg: lhs tEQL arg");
                      $$ = RubyParser::node_assign( & $1, $2/*srcOffsetSi*/, $3, vps);
                    }
                | lhs '=' arg kRESCUE_MOD arg
                    {
                      yTrace(vps, "arg: | lhs tEQL arg kRESCUE_MOD arg");
                      OmScopeType aScope(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $4);
                      NODE **valH = aScope.add(
                        RubyRescueBodyNode::s(ram_OOP_NIL, $5, ram_OOP_NIL, srcOfs, vps));
                      *valH = RubyRescueNode::s( $3, *valH, ram_OOP_NIL, srcOfs, vps);
                      $$ = RubyParser::node_assign( & $1, $2/*srcOffsetSi*/, *valH, vps);
                    }
                | var_lhs tOP_ASGN arg
                    {
                      $3 = RubyParser::value_expr($3, vps);
                      if ($1 != ram_OOP_NIL) {
                        yTrace(vps, "arg: | varLhs tOP_ASGN arg");
                        $$ = RubyParser::new_op_asgn($1, $2/*RpNameToken*/, $3, vps);
                      } else {
                        yTrace(vps, "arg: | NIL_LHS tOP_ASGN arg");
                        $$ = ram_OOP_NIL;
                      }
                    }
                | primary_value ary_ref tOP_ASGN arg
                    {
                      yTrace(vps, "arg: | primary_value tLBRACK_STR aref__args tRBRACK tOP_ASGN arg");
                      omObjSType *aref_args = om::FetchOop($2, 0);
                      $$ = RubyOpElementAsgnNode::s($1, aref_args, $3, $4, vps);
                    }
                | primary_value '.' tIDENTIFIER tOP_ASGN arg
                    {
                      yTrace(vps, "arg: | primary_value tDOT tIDENTIFIER tOP_ASGN arg");
                      $$ = RubyOpAsgnNode::s($1, $3, $4, $5, vps);
                    }
                | primary_value '.' tCONSTANT tOP_ASGN arg
                    {
                      yTrace(vps, "arg: | primary_value tDOT tCONSTANT tOP_ASGN arg");
                      $$ = RubyOpAsgnNode::s($1, $3, $4, $5, vps);
                    }
                | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg
                    {
                      yTrace(vps, "arg: | primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg");
                      // not seen with Ryan's grammar
                      $$ = RubyOpAsgnNode::s($1, $3, $4, $5, vps);
                    }
                | primary_value tCOLON2 tCONSTANT tOP_ASGN arg
                    {
                        rb_compile_error(vps, "constant re-assignment");
                        $$ = ram_OOP_NIL;
                    }
                | tCOLON3 tCONSTANT tOP_ASGN arg
                    {
                        rb_compile_error(vps, "constant re-assignment");
                        $$ = ram_OOP_NIL;
                    }
                | backref tOP_ASGN arg
                    {
                        rb_backref_error($1, vps);
                        $$ = ram_OOP_NIL;
                    }
                | arg tDOT2 arg
                    {
                      yTrace(vps, "arg: | arg tDOT2 arg");
                      $$ = RubyDotNode::s(2, $1, $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg tDOT3 arg
                    {
                      yTrace(vps, "arg: | arg tDOT3 arg");
                      $$ = RubyDotNode::s(3, $1, $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '+' arg
                    {
                      yTrace(vps, "arg: | arg tPLUS arg");
                      $$ = RubyParser::new_call_1( & $1, a_sym_plus, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '-' arg
                    {
                      yTrace(vps, "arg: | arg tMINUS arg");
                      $$ = RubyParser::new_call_1( & $1, a_sym_minus, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '*' arg
                    {
                      yTrace(vps, "arg: | arg tSTAR2 arg");
                      $$ = RubyParser::new_call_1( & $1, a_sym_star, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '/' arg
                    {
                      yTrace(vps, "arg: | arg tDIVIDE arg");
                      $$ = RubyParser::new_call_1( & $1, a_sym_div, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '%' arg
                    {
                      yTrace(vps, "arg: | arg tPERCENT arg");
                      $$ = RubyParser::new_call_1( & $1, a_sym_percent, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg tPOW arg
                    {
                      yTrace(vps, "arg: | arg tPOW arg");
                      $$ = RubyParser::new_call_1( & $1, a_sym_tPOW, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | tUMINUS_NUM tINTEGER tPOW arg
                    {
                        // $$  = call_op(call_op($2, tPOW, 1, $4, vps), tUMINUS, 0, 0, vps);
                      yTrace(vps, "arg: | tUMINUS_NUM tINTEGER tPOW arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **litH = aScope.add(RubyAbstractNumberNode::s( $2, vps));
                      NODE **valH = aScope.add(
                          RubyParser::new_call_1( litH, a_sym_tPOW, & $4, $3/*srcOffsetSi*/, vps));
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, $3, vps));
                      $$ = RubyParser::new_vcall( *valH, *selH , vps);
                    }
                | tUMINUS_NUM tFLOAT tPOW arg
                    {
                        // $$  = call_op(call_op($2, tPOW, 1, $4, vps), tUMINUS, 0, 0, vps);
                      yTrace(vps, "arg: | tUMINUS_NUM tFLOAT tPOW arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **litH = aScope.add(RubyAbstractNumberNode::s( $2, vps));
                      NODE **valH = aScope.add( RubyParser::new_call_1( litH, a_sym_tPOW, & $4, $3/*srcOffsetSi*/, vps));
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, $3, vps));
                      $$ = RubyParser::new_vcall( *valH, *selH , vps);
                    }
                | tUPLUS arg
                    {
                      yTrace(vps, "arg: | tUPLUS arg");
                      $$ = RubyParser::uplus_production( $2, $1/*srcOffsetSi*/, vps);
                    }
                | tUMINUS arg
                    {
                      yTrace(vps, "arg: | tUMINUS arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tUMINUS, $1, vps));
                      $$ = RubyParser::new_vcall( $2, *selH, vps);
                    }
                | arg '|' arg
                    {
                      yTrace(vps, "arg: | arg tPIPE arg");
                      $$ = RubyParser::new_call_1(& $1, a_sym_orOp, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '^' arg
                    {
                      yTrace(vps, "arg: | arg tCARET arg");
                      $$ = RubyParser::new_call_1(& $1, a_sym_upArrow, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg '&' arg
                    {
                      yTrace(vps, "arg: | arg tAMPER2 arg");
                      $$ = RubyParser::new_call_1(& $1, a_sym_andOp, & $3,  $2/*srcOffsetSi*/, vps);
                    }
                | arg tCMP arg
                    {
                      yTrace(vps, "arg: | arg tCMP arg");
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg '>' arg
                    {
                      yTrace(vps, "arg: | arg tGT arg");
                      $$ = RubyParser::new_call_1(& $1, a_sym_gt, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg tGEQ arg
                    {
                      yTrace(vps, "arg: | arg tGEQ arg");
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg '<' arg
                    {
                      yTrace(vps, "arg: | arg tLT arg");
                      $$ = RubyParser::new_call_1(& $1, a_sym_lt, & $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg tLEQ arg
                    {
                      yTrace(vps, "arg: | arg tLEQ arg");
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg tEQ arg
                    {
                      yTrace(vps, "arg: | arg tEQ arg");
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg tEQQ arg
                    {
                      yTrace(vps, "arg: | arg tEQQ arg");
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg tNEQ arg
                    {
                      yTrace(vps, "arg: | arg tNEQ arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::new_call_1(& $1, a_sym_tEQ, & $3, $2/*srcOffsetSi*/, vps));
                      $$ = RubyNotNode::s( *valH, vps);
                    }
                | arg tMATCH arg
                    {
                      yTrace(vps, "arg: | arg tMATCH arg");
                      $$ = RubyParser::get_match_node($1, $3, $2/*srcOffsetSi*/, vps);
                    }
                | arg tNMATCH arg
                    {
                      yTrace(vps, "arg: | arg tNMATCH arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubyParser::get_match_node($1, $3, $2/*srcOffsetSi*/, vps));
                      $$ = RubyNotNode::s( *valH, vps);
                    }
                | '!' arg
                    {
                      yTrace(vps, "arg: | tBANG arg");
                      $$ = RubyNotNode::s( $2, vps);
                    }
                | '~' arg
                    {
                      yTrace(vps, "arg: | tTILDE arg");
                      OmScopeType aScope(vps->omPtr);   // try it without value_expr
                      NODE **selH = aScope.add( RpNameToken::s(a_sym_tilde, $1, vps));
                      $$ = RubyParser::new_vcall( $2,  *selH, vps);
                    }
                | arg tLSHFT arg
                    {
                      yTrace(vps, "arg: | arg tRSHFT arg"); // try without value_expr
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg tRSHFT arg
                    {
                      yTrace(vps, "arg: | arg tRSHFT arg"); // try without value_expr
                      $$ = RubyParser::new_call_1($1, $2, $3, vps);
                    }
                | arg tANDOP arg
                    {
                      yTrace(vps, "arg: | arg tANDOP arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **clsH = aScope.add( RubyAndNode::cls(vps));
                      $$ = RubyParser::logop(*clsH, $1, $3, vps);
                    }
                | arg tOROP arg
                    {
                      yTrace(vps, "arg: | arg tOROP arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **clsH = aScope.add( RubyOrNode::cls(vps));
                      $$ = RubyParser::logop(*clsH, $1, $3, vps);
                    }
                | kDEFINED opt_nl {vps->in_defined = 1;} arg
                    {
                      yTrace(vps, "arg: | kDEFINED opt_nl arg");
                      vps->in_defined = 0;
                      $$ = RubyDefinedNode::s($4, vps);
                    }
                | arg '?' {vps->ternary_colon++;} arg ':' arg
                    {
                      yTrace(vps, "arg: | arg tEH arg tCOLON arg");
                      $$ = RubyIfNode::s($1, $4, $6, vps);
                      vps->ternary_colon--;
                    }
                | primary
                    {
                        yTrace(vps, "arg: | primary");
                        $$ = $1;
                    }
                ;

arg_value       : arg
                    {
                      yTrace(vps, "arg_value: arg");
                      $$ = RubyParser::value_expr($1, vps);
                    }
                ;

aref_args       : none
                | command opt_nl
                    {
                      yTrace(vps, "aref__args: | command opt_nl");
                      $$ = RubyRpCallArgs::s($1, vps);
                    }
                | args trailer
                    {
                      yTrace(vps, "aref__args: | args trailer");
                      $$ = $1;
                    }
                | args ',' tSTAR arg opt_nl
                    {
                      yTrace(vps, "aref__args: | args tCOMMA tSTAR arg opt_nl");
                      // value_expr($4);  was in rubinius, try without
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s($4, vps));
                      $$ = RubyArrayNode::append( $1 , *valH, vps /*returns first arg*/);
                    }
                | assocs trailer
                    {
                      yTrace(vps, "aref__args: | assocs trailer");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($1, vps));
                      $$ = RubyRpCallArgs::s(*hashNodeH, vps);
                    }
                | tSTAR arg opt_nl
                    {
                      yTrace(vps, "aref__args: | tSTAR arg opt_nl");
                      $2 = RubyParser::value_expr($2, vps);
                      OmScopeType aScope(vps->omPtr);
                      NODE **valH = aScope.add( RubySplatNode::s( $2, vps));
                      $$ = RubyRpCallArgs::s( *valH, vps);
                    }
                ;

paren_args      : '(' none ')'
                    {
                      yTrace(vps, "paren_args: tLPAREN2 none tRPAREN");
                      rParenLexPop(vps);
                      $$ = $2;
                    }
                | '(' call_args opt_nl ')'
                    {
                      yTrace(vps, "paren_args: | tLPAREN2 call_args opt_nl tRPAREN");
                      rParenLexPop(vps);
                      $$ = $2;
                    }
                | '(' block_call opt_nl ')'
                    {
                      yTrace(vps, "paren_args: | tLPAREN2 block_call opt_nl tRPAREN");
                      rParenLexPop(vps);
                      $$ = RubyRpCallArgs::s( $2, vps);
                    }
                | '(' args ',' block_call opt_nl ')'
                    {
                      yTrace(vps, "paren_args: | tLPAREN2 args tCOMMA block_call opt_nl tRPAREN");
                      rParenLexPop(vps);
                      $$ = RubyArrayNode::append( $2, $4, vps);
                    }
                ;

opt_paren_args  : none
                | paren_args
                ;

call_args       : command
                    {
                      yTrace(vps, "call_args: command");
                      $$ = RubyRpCallArgs::s( $1, vps);
                    }
                | args opt_block_arg
                    {
                      yTrace(vps, "call_args: | args opt_block_arg");
                        $$ = RubyRpCallArgs::append_blkArg($1, $2, vps /*returns first arg*/);
                    }
                | args ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args: | args tCOMMA tSTAR arg_value opt_block_arg");
                      // $$  = arg_concat(vps, $1, $4);
                      // $$  = arg_blk_pass($$, $5);
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s($4, vps));
                      RubyRpCallArgs::append_arg( $1, *splatH, vps);
                      RubyRpCallArgs::append_blkArg( $1, $5, vps);
                      $$ = $1 ;
                    }
                | assocs opt_block_arg
                    {
                      yTrace(vps, "call_args: | assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($1, vps));
                      $$ = RubyRpCallArgs::s_arg_blkArg(*hashNodeH, $2, vps);
                    }
                | assocs ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args: | assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($1, vps));
                      $$ = RubyRpCallArgs::s_arg_splatArg_blkArg(*hashNodeH, $4, $5, vps);
                    }
                | args ',' assocs opt_block_arg
                    {
                      yTrace(vps, "call_args: | args tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($3, vps));
                      $$ = RubyRpCallArgs::append_arg_blkArg($1, *hashNodeH, $4, vps);
                    }
                | args ',' assocs ',' tSTAR arg opt_block_arg
                    {
                      yTrace(vps, "call_args: | args tCOMMA assocs tCOMMA tSTAR arg opt_block_arg");
                      // rubinius had   value_expr($6);
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($3, vps));
                      $$ = RubyRpCallArgs::append_arg_splatArg_blkArg($1, *hashNodeH, $6, $7, vps);

                    }
                | tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args: | tSTAR arg_value opt_block_arg");
                      $$ = RubyRpCallArgs::s_splatArg_blkArg($2, $3, vps);
                    }
                | block_arg
                ;

call_args2      : arg_value ',' args opt_block_arg
                    {
                      yTrace(vps, "call_args2: arg_value tCOMMA args opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callArgsH = aScope.add( RubyParser::list_prepend($3, $1, vps));
                      RubyRpCallArgs::append_blkArg( *callArgsH, $4, vps);
                      $$ = *callArgsH;
                    }
                | arg_value ',' block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA block_arg");
                      $$ = RubyRpCallArgs::append_blkArg( $1, $3, vps);
                    }
                | arg_value ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA tSTAR arg_value opt_block_arg");
                      $$ = RubyRpCallArgs::s_arg_splatArg_blkArg( $1, $4, $5, vps);
                    }
                | arg_value ',' args ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callArgsH = aScope.add( RubyParser::list_prepend($3, $1, vps));
                      $$ = RubyRpCallArgs::append_arg_blkArg(*callArgsH, $6, $7, vps); // returns first arg
                    }
                | assocs opt_block_arg
                    {
                      yTrace(vps, "call_args2: | assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($1, vps));
                      $$ = RubyRpCallArgs::s_arg_blkArg(*hashNodeH, $2, vps);
                    }
                | assocs ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args2: | assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($1, vps));
                      $$ = RubyRpCallArgs::s_arg_splatArg_blkArg( *hashNodeH, $4, $5, vps);
                    }
                | arg_value ',' assocs opt_block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($3, vps));
                      $$ = RubyRpCallArgs::s_arg_arg_blkArg( $1, *hashNodeH, $4, vps);
                    }
                | arg_value ',' args ',' assocs opt_block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA assocs opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($5, vps));
                      $$ = RubyRpCallArgs::s_arg_addAll_arg_blkArg($1, $3, *hashNodeH, $6, vps);
                    }
                | arg_value ',' assocs ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($3, vps));
                      $$ = RubyRpCallArgs::s_arg_arg_splatArg_blkArg($1, *hashNodeH, $6, $7, vps);
                    }
                | arg_value ',' args ',' assocs ',' tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args2: | arg_value tCOMMA args tCOMMA assocs tCOMMA tSTAR arg_value opt_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **hashNodeH = aScope.add( RubyHashNode::s($5, vps));
                      $$ = RubyRpCallArgs::s_arg_addAll_arg_splatArg_blkArg($1, $3, *hashNodeH, $8, $9, vps);
                    }
                | tSTAR arg_value opt_block_arg
                    {
                      yTrace(vps, "call_args2: | tSTAR arg_value opt_block_arg");
                      $$ = RubyRpCallArgs::s_splatArg_blkArg($2, $3, vps);
                    }
                | block_arg
                ;

command_args    :  {
                      yTrace(vps, "command_args:");
                      OmScopeType scp(vps->omPtr);
                      $$ = vps->cmdarg_stack.asSi() ;
#if defined(FLG_DEBUG)
  if (debugCmdArg) {
    printf("saving cmdarg_stack 0x%lx\n", vps->cmdarg_stack.word());
  }
#endif
                      CMDARG_PUSH(vps, 1);
                    }
                  open_args
                    {
                      yTrace(vps, "command_args: ___  open_args");
                      if (! vps->cmdarg_stack.restoreFromSi( $1 )) {
                        rb_compile_error("invalid cmdarg_stack.restore", vps);
                      }
#if defined(FLG_DEBUG)
  if (debugCmdArg) {
    printf("restored cmdarg_stack 0x%lx\n", vps->cmdarg_stack.word());
  }
#endif
                      $$ = $2;
                    }
                ;

open_args       : call_args
                | tLPAREN_ARG  {vps->lex_state = EXPR_ENDARG;} ')'
                    {
                      yTrace(vps, "open_args: tLPAREN_ARG");
                      rParenLexPop(vps);
                      rb_warning(vps, "don't put space before argument parentheses");
                      $$ = ram_OOP_NIL;
                    }
                | tLPAREN_ARG call_args2 {vps->lex_state = EXPR_ENDARG;} ')'
                    {
                      yTrace(vps, "open_args: ___ tRPAREN");
                      rParenLexPop(vps);
                      rb_warning(vps, "don't put space before argument parentheses");
                      $$ = $2;
                    }
                ;

block_arg       : tAMPER arg_value
                    {
                      yTrace(vps, "block_arg: tAMPER arg_value");
                      $$ = RubyBlockPassNode::s( $2 , vps);
                    } ;
opt_block_arg   : ',' block_arg
                    {
                      yTrace(vps, "opt_block_arg: tCOMMA block_arg");
                      $$ = $2;
                    }
                | none
                ;

args            : arg_value
                    {
                      yTrace(vps, "args: arg_value");
                      $$ = RubyRpCallArgs::s( $1, vps);
                    }
                | args ',' arg_value
                    {
                      yTrace(vps, " args: | args tCOMMA arg_value");
                      $$ = RubyRpCallArgs::append_arg($1, $3, vps /*return first arg*/);
                    }
                ;

mrhs            : args ',' arg_value
                    {
                      yTrace(vps, "mrhs: args tCOMMA arg_value");
                      $$ = RubyRpCallArgs::append_arg($1, $3, vps /*return first arg*/);
                    }
                | args ',' tSTAR arg_value
                    {
                      yTrace(vps, "mrhs: | args tCOMMA tSTAR arg_value");
                      $$ = RubyRpCallArgs::append_splatArg($1, $4, vps);
                    }
                | tSTAR arg_value
                    {
                      yTrace(vps, "mrhs: | tSTAR arg_value");
                      $$ = RubySplatNode::s($2, vps);
                    }
                ;

primary         : literal
                | strings
                | xstring
                | regexp
                | words
                | qwords
                | var_ref
                | backref
                | tFID
                    {
                      yTrace(vps, "primary: tFID");
                      $$ = RubyParser::new_fcall($1, ram_OOP_NIL, vps);
                    }
                | kBEGIN
                    {
                      yTrace(vps, "primary: | kBEGIN");
                        // $<num>1 = ruby_sourceline;
                      PUSH_LINE(vps, "begin");
                    }
                  bodystmt
                  kEND
                    {
                      yTrace(vps, "primary: | kBEGIN body_stamt kEND");
                      POP_LINE(vps);
                      if ($3 == ram_OOP_NIL) {
                         $$ = RubyNilNode::new_(vps);
                      } else {
                         $$ = RubyBeginNode::s($3, vps);
                      }
                      //  nd_set_line($$, $<num>1);
                    }
                | tLPAREN_ARG expr {vps->lex_state = EXPR_ENDARG;} opt_nl ')'
                    {
                      yTrace(vps, "primary: ___ opt_nl tRPAREN");
                      rParenLexPop(vps);
                      rb_warning(vps, "(...) interpreted as grouped expression");
                      $$ = $2;
                    }
                | tLPAREN compstmt ')'
                    {
                      yTrace(vps, "primary: | tLPAREN comp_stamt tRPAREN");
                      rParenLexPop(vps);
                      OmScopeType scp(vps->omPtr);
                      NODE **resH;
                      if ($2 == ram_OOP_NIL) {
                        resH = scp.add( RubyNilNode::new_(vps) );
                      } else {
                        resH = scp.add( $2);
                      }
                      $$ = RubyNode::setParen(*resH, vps);
                    }
                | primary_value tCOLON2 tCONSTANT
                    {
                      yTrace(vps, "primary: | primary_value tCOLON2 tCONSTANT");
                      $$ = RubyColon2Node::s($1, $3, vps);
                    }
                | tCOLON3 tCONSTANT
                    {
                      yTrace(vps, "primary: | tCOLON3 tCONSTANT");
                      $$ = RubyColon3Node::s( $2, vps);
                    }
                | primary_value ary_ref
                    {
                      yTrace(vps, "primary: | primary_value tLBRACK_STR aref__args tRBRACK");
                      omObjSType *srcOfs = om::FetchOop($2, 1); // no gc
                      omObjSType *aref_args = om::FetchOop($2, 0);
                      $$ = RubyParser::new_aref($1, aref_args, srcOfs, vps);
                    }
                | tLBRACK aref_args ']'
                    {
                      yTrace(vps, "primary: | tLBRACK aref__args tRBRACK");
                      rParenLexPop(vps);
                      if ($2 == ram_OOP_NIL) {
                         $$ = RubyRpCallArgs::s(vps);
                      } else {
                         $$ = $2;
                      }
                    }
                | tLBRACE assoc_list '}'
                    {
                      yTrace(vps, "primary: | tLBRACE assoc_list tRCURLY");
                      rParenLexPop(vps);
                      $$ = RubyHashNode::s($2, vps);
                    }
                | kRETURN
                    {
                      yTrace(vps, "primary: | kRETURN");
                      $$ = RubyReturnNode::s( vps->nilH(), $1/*return token*/, vps);
                    }
                | kYIELD '(' call_args ')'
                    {
                      yTrace(vps, "primary: | kYIELD tLPAREN2 call_args tRPAREN");
                      rParenLexPop(vps);
                      $$ = RubyParser::new_yield(& $3, $1/*yield token*/, vps);
                    }
                | kYIELD '(' ')'
                    {
                      yTrace(vps, "primary: | kYIELD tLPAREN2 tRPAREN");
                      rParenLexPop(vps);
                      $$ = RubyParser::new_yield(vps->nilH(), $1/*yield token*/, vps);
                    }
                | kYIELD
                    {
                      yTrace(vps, "primary: | kYIELD");
                      $$ = RubyParser::new_yield(vps->nilH(), $1/*yield token*/, vps);
                    }
                | kDEFINED opt_nl '(' {vps->in_defined = 1;} expr ')'
                    {
                      yTrace(vps, "primary: | kDEFINED opt_nl tLPAREN2 expr tRPAREN");
                      rParenLexPop(vps);
                      vps->in_defined = 0;
                      $$ = RubyDefinedNode::s($5, vps);
                    }
                | operation brace_block
                    {
                      yTrace(vps, "primary: | operation brace_block");
                      OmScopeType aScope(vps->omPtr);
                      NODE **callH = aScope.add( RubyParser::new_fcall( $1, ram_OOP_NIL, vps));
                      RubyIterRpNode::set_call( $2, *callH, vps);
                      $$ = $2; // $2 is a RubyIterRpNode
                    }
                | method_call
                | method_call brace_block
                    {
                      yTrace(vps, "primary: | method_call brace_block");
                      if (RubyBlockPassNode::is_a($1, vps)) {
                         rb_compile_error(vps, "both block arg and actual block given");
                      }
                      RubyIterRpNode::set_call( $2, $1, vps);
                      $$ = $2;
                    }
                | kIF {
                    PUSH_LINE(vps, "if");
                  } expr_value then
                  compstmt
                  if_tail
                  kEND
                    {
                      yTrace(vps, "primary: | kIF expr_value then comp_stamt if_tail kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); // kIF
                      $$ = RubyParser::new_if( $3, $5, $6, srcOfs, vps);
                    }
                | kUNLESS {
                    PUSH_LINE(vps, "unless");
                  } expr_value then
                  compstmt
                  opt_else
                  kEND
                    {
                      yTrace(vps, "primary: | kUNLESS expr_value then comp_stamt opt_else kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); // kUNLESS
                      $$ = RubyParser::new_if( $3, $6, $5, srcOfs, vps);
                    }
                | kWHILE {
                    yTrace(vps, "primary: | kWHILE");
                    PUSH_LINE(vps, "while");
                    COND_PUSH(vps, 1);
                  } expr_value do { COND_POP(vps);}
                  compstmt
                  kEND
                    {
                      yTrace(vps, "primary: kWHILE ___ comp_stamt kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); /* of kWHILE*/
                      $$ = RubyParser::new_while( $6, $3, srcOfs, vps);
                    }
                | kUNTIL {
                    yTrace(vps, "primary: | kUNTIL");
                    PUSH_LINE(vps, "until");
                    COND_PUSH(vps, 1);
                  } expr_value do { COND_POP(vps);}
                  compstmt
                  kEND
                    {
                      yTrace(vps, "kUNTIL ___ comp_stamt kEND");
                      // maglev had premature_eof() check
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); /* of kUNTIL*/
                      $$ = RubyParser::new_until( $6, $3, srcOfs, vps);
                    }
                | kCASE {
                    PUSH_LINE(vps, "case");
                  } expr_value opt_terms
                  case_body
                  kEND
                    {
                      yTrace(vps, "primary: | kCASE expr_value opt_termms case_body kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); /* of kCASE*/
                      $$ = RubyCaseNode::s($3, $5, srcOfs, vps);
                    }
                | kCASE opt_terms {
                    push_start_line(vps, vps->ruby_sourceline() - 1, "case");
                  } case_body kEND
                    {
                      yTrace(vps, "primary: | kCASE opt_termms case_body kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); /* of kCASE*/
                      $$ = RubyCaseNode::s(ram_OOP_NIL, $4, srcOfs, vps);
                    }
                | kCASE opt_terms {
                    push_start_line(vps, vps->ruby_sourceline() - 1, "case");
                  } kELSE compstmt kEND
                    {
                      yTrace(vps, "primary: | kCASE opt_termms kELSE comp_stamt kEND");
                      POP_LINE(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); /* of kCASE*/
                      $$ = RubyCaseNode::s(ram_OOP_NIL, $5, srcOfs, vps);
                    }
                | kFOR {
                    PUSH_LINE(vps, "for");
                  } for_var kIN { COND_PUSH(vps, 1);} expr_value do { COND_POP(vps);}
                  compstmt
                  kEND
                    {
                      yTrace(vps, "primary: kFOR ___ comp_stamt kEND");
                      POP_LINE(vps);
                      $$ = RubyForNode::s( & $6, & $3, & $9, $1/*for token*/,
                                                vps, 3/* strlen( 'end' ) */ );
                    }
                | kCLASS cpath superclass
                    {
                      yTrace(vps, "primary: | kCLASS cpath superclass");
                      PUSH_LINE(vps, "class");
                      if (vps->in_def || vps->in_single) {
                          rb_compile_error(vps, "class definition in method body");
                      }
                      vps->class_nest++;
                      local_push(vps, 0);
                      $$ = int64ToSi( vps->ruby_sourceline() );
                    }
                  bodystmt
                  kEND
                    {
                      yTrace(vps, "primary: | kCLASS ___ body_stamt kEND");
                      POP_LINE(vps);
                      // new_class( path, superclass, body)
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1);  // of kCLASS
                      NODE **resH = scp.add( RubyClassNode::s( $2, $3, $5, *vps->sourceStrH, srcOfs,  vps));
                      //  nd_set_line($$, $<num>4);
                      local_pop(vps);
                      vps->class_nest--;
                      $$ = *resH;
                    }
                | kCLASS tLSHFT expr
                    {
                      yTrace(vps, "primary: | kCLASS tLSHFT expr");
                      PUSH_LINE(vps, "class");
                      $$ = int64ToSi( vps->in_def );
                      vps->in_def = 0;
                    }
                  term
                    {
                      yTrace(vps, "primary | kCLASS ___ Term");
                      $$ = int64ToSi( vps->in_single );
                      vps->in_single = 0;
                      vps->class_nest++;
                      local_push(vps, 0);
                    }
                  bodystmt
                  kEND
                    {
                      yTrace(vps, "primary  | kCLASS ___ body_stamt kEND");
                      int lineNum = POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      NODE **resH = scp.add( RubySClassNode::s(& $3, & $7, $1/*RpNameTokenkCLASS*/, lineNum, vps));
                      local_pop(vps);
                      vps->class_nest--;
                      vps->in_def = siToI64( $4 );
                      vps->in_single = siToI64( $6) ;
                      $$ = *resH;
                    }
                | kMODULE cpath
                    {
                      yTrace(vps, "primary: | kMODULE cpath");
                      PUSH_LINE(vps, "module");
                      if (vps->in_def || vps->in_single) {
                          rb_compile_error(vps, "module definition in method body");
                      }
                      vps->class_nest++;
                      local_push(vps, 0);
                      $$ = int64ToSi( vps->ruby_sourceline() );
                    }
                  bodystmt
                  kEND
                    {
                      yTrace(vps, "primary: | kMODULE ___ body_stamt kEND");
                      POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1);  // of kMODULE
                      NODE **resH = scp.add( RubyModuleNode::s( $2, $4, *vps->sourceStrH, srcOfs, vps));
                      // nd_set_line($$, $<num>3);
                      local_pop(vps);
                      vps->class_nest--;
                      $$ = *resH;
                    }
                | kDEF fname
                    {
                      yTrace(vps, "primary: | kDEF fname");
                      PUSH_LINE(vps, "def");
                      $$ = ram_OOP_Zero; // $<id>$ = cur_mid;
                      // cur_mid = $2;
                      vps->in_def++;
                      local_push(vps, 0);
                    }
                  f_arglist
                  bodystmt
                  kEND
                    {
                      yTrace(vps, "primary: | kDEF ___ f_arglist body_stamt kEND");
                      int lineNum = POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1/*kDEF*/);
                      omObjSType *endOfs = RpNameToken::srcOffsetO(vps, $6/*kEND*/);
                      NODE **resH = scp.add( RubyParser::new_defn( $2/*fname*/, $4/*arglist*/,
                                        $5/*body*/, srcOfs, lineNum, endOfs, vps));
                      local_pop(vps);
                      vps->in_def--;
                      // cur_mid = $<id>3;
                      $$ = *resH;
                    }
                | kDEF singleton dot_or_colon {vps->lex_state = EXPR_FNAME;} fname
                    {
                      yTrace(vps, "primary: | kDEF ___ fname");
                      PUSH_LINE(vps, "def");
                      vps->in_single++;
                      local_push(vps, 0);
                      vps->lex_state = EXPR_END; /* force for args */
                    }
                  f_arglist
                  bodystmt
                  kEND
                    {
                      yTrace(vps, "primary: | kDEF ___ f_arglist body_stamt kEND");
                      int lineNum = POP_LINE(vps);
                      OmScopeType scp(vps->omPtr);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); // of kDEF
                      omObjSType *endOfs = RpNameToken::srcOffsetO(vps, $9/*kEND*/);
                      NODE **resH = scp.add( RubyParser::new_defs( $2/*rcvr (the singleton)*/,
                                 $5/*fname*/, $7/*args*/, $8/*body*/, srcOfs,
                                  lineNum, endOfs, vps));
                      local_pop(vps);
                      vps->in_single--;
                      $$ = *resH;
                    }
                | kBREAK
                    {
                      yTrace(vps, "primary: | kBREAK");
                      $$ = RubyBreakNode::s(vps->nilH(), $1/*break token*/, vps);
                    }
                | kNEXT
                    {
                      yTrace(vps, "primary: | kNEXT");
                      $$ = RubyNextNode::s(vps->nilH(), $1/*next token*/, vps);
                    }
                | kREDO
                    {
                      yTrace(vps, "primary: | kREDO");
                      $$ = RubyRedoNode::s($1/*redo token*/, vps);
                    }
                | kRETRY
                    {
                      yTrace(vps, "primary: | kRETRY");
                      $$ = RubyRetryNode::s($1/*retry token*/, vps);
                    }
                ;

primary_value   : primary
                    {
                      yTrace(vps, "primary_value: primary");
                      $$ = RubyParser::value_expr($1, vps);
                    }
                ;

then            : term
                | ':'
                | kTHEN
                | term kTHEN
                ;

do              : term
                | ':'
                | kDO_COND
                ;

if_tail         : opt_else
                | kELSIF expr_value then
                  compstmt
                  if_tail
                    {
                      yTrace(vps, "if_tail: opt_else| kELSIF___if_tail ");
                      $$ = RubyIfNode::s($2, $4, $5, vps);
                    }
                ;

opt_else        : none
                | kELSE compstmt
                    {
                      yTrace(vps, "opt_else: | kELSE comp_stamt");
                      $$ = $2;
                    }
                ;

for_var         : lhs
                | mlhs
                ;

block_par       : mlhs_item
                    {
                      yTrace(vps, "block_par : mlhs_item");
                      $$ = RubyArrayNode::s($1, vps);
                    }
                | block_par ',' mlhs_item
                    {
                      yTrace(vps, "block_par : block_par , mlhs_item");
                      $$ = RubyArrayNode::append($1, $3, vps);
                    }
                ;

block_var       : block_par
                    {
                      yTrace(vps, "block_var : block_par x");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      $$ = RubyParser::new_parasgn( $1, ofsO, vps);
                    }
                | block_par ','
                    {
                      yTrace(vps, "block_var | block_par , x");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      $$ = RubyParser::new_parasgn_trailingComma( $1, ofsO, vps);
                    }
                | block_par ',' tAMPER lhs
                    {
                      yTrace(vps, "block_var | block_par , & lhs x");
                      RubyArrayNode::append_amperLhs($1, $4, vps);
                      $$ = RubyParser::new_parasgn( $1, $3/*srcOffsetSi*/, vps);
                    }
                | block_par ',' tSTAR lhs ',' tAMPER lhs
                    {
                      yTrace(vps, "block_var | block_par , STAR lhs , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s($4, vps));
                      RubyArrayNode::append($1, *splatH, vps);
                      RubyArrayNode::append_amperLhs($1, $7, vps);
                      $$ = RubyParser::new_parasgn( $1, $3/*srcOffsetSi*/, vps);
                    }
                | block_par ',' tSTAR ',' tAMPER lhs
                    {
                      yTrace(vps, "block_var | block_par , STAR , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      RubyArrayNode::append($1, *splatH, vps);
                      RubyArrayNode::append_amperLhs($1, $6, vps);
                      $$ = RubyParser::new_parasgn( $1, $3/*srcOffsetSi*/, vps);
                    }
                | block_par ',' tSTAR lhs
                    {
                      yTrace(vps, "block_var | block_par , STAR lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s($4, vps));
                      RubyArrayNode::append($1, *splatH, vps);
                      $$ = RubyParser::new_parasgn( $1, $3/*srcOffsetSi*/, vps);
                    }
                | block_par ',' tSTAR
                    {
                      yTrace(vps, "block_var | block_par , STAR x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      RubyArrayNode::append($1, *splatH, vps);
                      $$ = RubyParser::new_parasgn( $1, $3/*srcOffsetSi*/, vps);
                    }
                | tSTAR lhs ',' tAMPER lhs
                    {
                      yTrace(vps, "block_var | STAR lhs , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s($2, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      RubyArrayNode::append_amperLhs(*arrH, $5, vps);
                      $$ = RubyParser::new_parasgn( *arrH, $1/*srcOffsetSi*/, vps);
                    }
                | tSTAR ',' tAMPER lhs
                    {
                      yTrace(vps, "block_var | STAR , & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      RubyArrayNode::append_amperLhs(*arrH, $4, vps);
                      $$ = RubyParser::new_parasgn( *arrH, $1/*srcOffsetSi*/, vps);
                    }
                | tSTAR lhs
                    {
                      yTrace(vps, "block_var | STAR lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s($2, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      $$ = RubyParser::new_parasgn( *arrH, $1/*srcOffsetSi*/, vps);
                    }
                | tSTAR
                    {
                      yTrace(vps, "block_var | STAR x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **splatH = aScope.add( RubySplatNode::s(ram_OOP_NIL, vps));
                      NODE **arrH = aScope.add(RubyArrayNode::s(*splatH, vps));
                      $$ = RubyParser::new_parasgn( *arrH, $1/*srcOffsetSi*/, vps);
                    }
                | tAMPER lhs
                    {
                      yTrace(vps, "block_var | & lhs x");
                      OmScopeType aScope(vps->omPtr);
                      NODE **arrH = aScope.add(RubyArrayNode::new_(vps));
                      RubyArrayNode::append_amperLhs(*arrH, $2, vps);
                      $$ = RubyParser::new_parasgn( *arrH, $1/*srcOffsetSi*/, vps);
                    }
                ;

opt_block_var   : none
                | '|' /* none */ '|'
                    {
                      yTrace(vps, "opt_block_var: | tPIPE tPIPE");
                      $$ = ram_OOP_NIL ;
                    }
                | tOROP
                    {
                      yTrace(vps, "opt_block_var: | tOROP");
                      $$ = ram_OOP_NIL ;
                    }
                | '|' block_var '|'
                    {
                      yTrace(vps, "opt_block_var: | tPIPE block_var tPIPE");
                      $$ = $2;
                    }
                ;

do_block        : kDO_BLOCK
                    {
                      yTrace(vps, "do_block: kDO_BLOCK");
                      PUSH_LINE(vps, "do");
                      reset_block(vps);
                      // $1 = int64ToSi(vps->ruby_sourceline() );
                    }
                  opt_block_var
                    {
                      yTrace(vps, "do_block: ___ opt_block_var");
                       $$ = ram_OOP_NIL; // getBlockVars not used
                    }
                  compstmt
                  kEND
                    {
                      yTrace(vps, "do_block: ___ comp_stamt kEND");
                      POP_LINE(vps);
                      popBlockVars(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1); // of kDO_BLOCK
                      $$ = RubyIterRpNode::s( $3/*masgn from opt_block_var*/, $5/*compstmt*/, srcOfs,
                                                vps, 3/* strlen( 'end' ) */ );
                    }
                ;

block_call      : command do_block
                    {
                      yTrace(vps, "block_call: command do_block");
                      if (RubyBlockPassNode::is_a($1, vps)) {
                         rb_compile_error(vps, "both block arg and actual block given");
                      }
                      RubyIterRpNode::set_call( $2, $1, vps);
                      $$ = $2;
                    }
                | block_call '.' operation2 opt_paren_args
                    {
                      yTrace(vps, "block_call: | block_call tDOT operation2 opt_paren_args");
                      $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                | block_call tCOLON2 operation2 opt_paren_args
                    {
                      yTrace(vps, "block_call: block_call tCOLON2 operation2 opt_paren_args");
                      $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                ;

method_call     : operation paren_args
                    {
                      yTrace(vps, "method_call: operation  paren_args");
                      $$ = RubyParser::new_fcall($1, $2, vps);
                    }
                | primary_value '.' operation2 opt_paren_args
                    {
                      yTrace(vps, "method_call: | primary_value tDOT operation2 opt_paren_args");
                      $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                | primary_value tCOLON2 operation2 paren_args
                    {
                      yTrace(vps, "method_call: | primary_value tCOLON2 operation2 paren_args");
                      $$ = RubyParser::new_call($1, $3, $4, vps);
                    }
                | primary_value tCOLON2 operation3
                    {
                      yTrace(vps, "method_call: | primary_value tCOLON2 operation3");
                      $$ = RubyParser::new_vcall($1, $3, vps);
                    }

                | primary_value '.' paren_args
                    {
                      yTrace(vps, "method_call: | primary_value tDOT paren_args");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      omObjSType* sym_call = RpNameToken::s("call", srcOfs, vps);
                      $$ = RubyParser::new_call($1, sym_call, $3, vps);
                    }

                | primary_value tCOLON2 paren_args
                    {
                      yTrace(vps, "method_call: | primary_value tCOLON2 paren_args");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $2);
                      omObjSType* sym_call = RpNameToken::s("call", srcOfs, vps);
                      $$ = RubyParser::new_call($1, sym_call, $3, vps);
                    }

                | kSUPER paren_args
                    {
                      yTrace(vps, "method_call: | kSUPER paren_args");
                      $$ = RubyParser::new_super(&  $2, $1/*super token*/, vps);
                    }
                | kSUPER
                    {
                      yTrace(vps, "method_call: | kSUPER");
                      $$ = RubyZSuperNode::s( $1/*super token*/ , vps);
                    }
                ;

brace_block     : '{'
                    {
                      yTrace(vps, "brace_block: tLCURLY");
                      reset_block(vps);
                      // $1 is srcOffsetSi
                    }
                  opt_block_var
                    {
                       $$ = ram_OOP_NIL; // getBlockVars not used
                    }
                  compstmt '}'
                    {
                      yTrace(vps, "brace_block: tLCURLY ___ comp_stamt tRCURLY");
                      rParenLexPop(vps);
                      popBlockVars(vps);
                      $$ = RubyIterRpNode::s($3/*masgn from opt_block_var*/, $5/*compstmt*/, $1/*srcOffsetSi*/,
                                                vps, 1/* strlen( '}' ) */ );
                    }
                | kDO
                    {
                      yTrace(vps, "brace_block: | kDO");
                      PUSH_LINE(vps, "do");
                      // $1 is RpNameToken of 'do'
                      reset_block(vps);
                    }
                  opt_block_var
                    {
                       $$ = ram_OOP_NIL; // getBlockVars not used
                    }
                  compstmt kEND
                    {
                      yTrace(vps, "brace_block: | kDO ___ comp_stamt kEND");
                      POP_LINE(vps);
                      popBlockVars(vps);
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1);
                      $$ = RubyIterRpNode::s($3/*masgn from opt_block_var*/, $5/*compstmt*/, srcOfs,
                                                vps, 3/* strlen( 'end' ) */ );
                    }
                ;

case_body       : kWHEN when_args then
                  compstmt
                  cases
                    {
                      yTrace(vps, "case_body: kWHEN when_args then comp_stamt cases");
                      $$ = RubyWhenNode::s( & $2, & $4, & $5, $1/*when token*/, vps);
                    }
                ;
when_args       : args
                | args ',' tSTAR arg_value
                    {
                      yTrace(vps, "when_args: args | args tCOMMA tSTAR arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE **whenH = aScope.add( RubyWhenNode::s( & $4, vps->nilH(), vps->nilH(),
                                                                        $3/*srcOffsetSi of tSTAR*/, vps));
                      $$ = RubyParser::list_append($1, *whenH, vps);
                    }
                | tSTAR arg_value
                    {
                      yTrace(vps, "when_args: | tSTAR arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE **whenH = aScope.add( RubyWhenNode::s( & $2, vps->nilH(), vps->nilH(),
                                                                        $1/*srcOffsetSi of tSTAR*/, vps));
                      $$ = RubyRpCallArgs::s( *whenH, vps);
                    }
                ;

cases           : opt_else
                | case_body
                ;

opt_rescue      : kRESCUE exc_list exc_var then
                  compstmt
                  opt_rescue
                    {
                      yTrace(vps, "opt_rescue: kRESCUE exc_list exc_var then comp_stamt opt_rescue");
                      omObjSType *srcOfs = RpNameToken::srcOffsetO(vps, $1);
                      $$ = RubyParser::opt_rescue( $2, $3, $5, $6, srcOfs, vps);
                    }
                | none
                ;

exc_list        : arg_value
                    {
                      yTrace(vps, "exc_list: arg_value");
                      $$ = RubyArrayNode::s($1, vps);
                    }
                | mrhs
                | none
                ;

exc_var         : tASSOC lhs
                    {
                      yTrace(vps, "exc_var: tASSOC lhs");
                      $$ = $2;
                    }
                | none
                ;

opt_ensure      : kENSURE compstmt
                    {
                      yTrace(vps, "opt_ensure: kENSURE comp_stamt");
                      // $2 is argument block to rubyEnsure:
                      $$ = RubyEnsureNode::s(& $2/*nil arg coerced to RubyNilNode::new_*/ , $1/*ensure token*/, vps);
                    }
                | none
                ;

literal         : numeric
                    {
                      yTrace(vps, "literal: numeric");
                      $$= RubyAbstractNumberNode::s( $1 , vps);
                    }
                | symbol
                    {
                      yTrace(vps, "literal: | symbol");
                      $$ = RubySymbolNode::s( quidToSymbolObj($1, vps), vps);
                    }
                | dsym
                ;

strings         : string
                    {
                      yTrace(vps, "strings: string");
                      $$ = RubyParser::new_string($1, vps);
                    }
                ;

string          : tCHAR
                | string1
                | string string1
                    {
                      yTrace(vps, "string: | string string1");
                      $$ = RubyParser::literal_concat($1, $2, vps);
                    }
                ;

string1         : tSTRING_BEG string_contents tSTRING_END
                    {
                      yTrace(vps, "string1: tSTRING_BEG string_contents tSTRING_END");
                      $$ = $2;
                    }
                ;

xstring         : tXSTRING_BEG xstring_contents tSTRING_END
                    {
                      yTrace(vps, "xstring: tXSTRING_BEG xstring_contents tSTRING_END");
                      $$ = RubyParser::new_xstring($2, vps);
                    }
                ;

regexp          : tREGEXP_BEG xstring_contents tREGEXP_END
                    {
                      yTrace(vps, "regexp: tREGEXP_BEG xstring_contents tREGEXP_END");
                      $$ = RubyParser::new_regexp( $2, $3/*regexp options Si*/, vps);
                    }
                ;

words           : tWORDS_BEG ' ' tSTRING_END
                    {
                      yTrace(vps, "words: tWORDS_BEG tSPACE tSTRING_END");
                      $$ = RubyArrayNode::new_(vps);
                    }
                | tWORDS_BEG word_list tSTRING_END
                    {
                      yTrace(vps, "words: | tWORDS_BEG word_list tSTRING_END");
                      $$ = $2;
                    }
                ;

word_list       : /* none */
                    {
                      yTrace(vps, "word_list: none");
                      $$ = RubyArrayNode::new_(vps); // $$  = 0;
                    }
                | word_list word ' '
                    {
                      yTrace(vps, "word_list: | word_list word tSPACE");
                      $$ = RubyParser::append_evstr2dstr( $1 , $2, vps);
                    }
                ;

word            : string_content
                | word string_content
                    {
                      yTrace(vps, "word: | word string_content");
                      $$ = RubyParser::literal_concat($1, $2, vps);
                    }
                ;

qwords          : tQWORDS_BEG ' ' tSTRING_END
                    {
                      yTrace(vps, "tQWORDS_BEG tSPACE tSTRING_END");
                      $$ = RubyArrayNode::new_(vps);
                    }
                | tQWORDS_BEG qword_list tSTRING_END
                    {
                      yTrace(vps, "tQWORDS_BEG qword_list tSTRING_END");
                      $$ = $2;
                    }
                ;

qword_list      : /* none */
                    {
                      yTrace(vps, "qword_list: none");
                      $$ = RubyArrayNode::new_(vps); // $$  = 0;
                    }
                | qword_list tSTRING_CONTENT ' '
                    {
                      yTrace(vps, "qword_list: | qword_list tSTRING_CONTENT tSPACE");
                      OmScopeType aScope(vps->omPtr);
                      NODE **strH = aScope.add(RubyStrNode::s($2, vps));
                      $$ = RubyArrayNode::append($1, *strH, vps);// returns first arg
                    }
                ;

string_contents : /* none */
                    {
                      yTrace(vps, "string_contents: none");
                      $$ = RubyStrNode::s( om::NewString(vps->omPtr , 0), vps);
                    }
                | string_contents string_content
                    {
                      yTrace(vps, "string_contents: | string_contents string_content");
                      $$ = RubyParser::literal_concat($1, $2, vps);
                    }
                ;

xstring_contents: /* none */
                    {
                      yTrace(vps, "xstring_contents: none");
                      $$ = ram_OOP_NIL;
                    }
                | xstring_contents string_content
                    {
                      yTrace(vps, "xstring_contents: | xstring_contents string_content");
                      $$ = RubyParser::literal_concat($1, $2, vps);
                    }
                ;

string_content  : tSTRING_CONTENT
                    {
                      yTrace(vps,  "string_content: tSTRING_CONTENT" );
                      $$ = RubyStrNode::s( $1, vps );
                    }
                | tSTRING_DVAR
                    {
                      yTrace(vps, "string_content: | tSTRING_DVAR");
                      vps->lex_state = EXPR_BEG;
                      $$ = vps->clear_lex_strterm();
                    }
                  string_dvar
                    {
                      yTrace(vps, "string_content: | string_dvar");
                      vps->set_lex_strterm( $2);
                      $$ = RubyEvStrNode::s($3, vps);
                    }
                | tSTRING_DBEG
                    {
                      yTrace(vps, "string_content: | tSTRING_DBEG");
                      OmScopeType scp(vps->omPtr);
                      NODE **resH = scp.add( vps->clear_lex_strterm());
                      vps->lex_state = EXPR_BEG;
                      COND_PUSH(vps, 0);
                      CMDARG_PUSH(vps, 0);
                      $$ = *resH;
                    }
                  compstmt '}'
                    {
                      yTrace(vps, "string_content: | tSTRING_DBEG ___ comp_stamt tRCURLY");
                      vps->set_lex_strterm( $2);
                      rParenLexPop(vps);
                      $$ = RubyParser::new_evstr($3, vps);
                    }
                ;

string_dvar     : tGVAR
                   {
                      yTrace(vps, "string_dvar: tGVAR");
                      $$ = RubyGlobalVarNode::s( quidToSymbolObj( $1, vps), vps);
                   }
                | tIVAR
                   {
                      yTrace(vps, "string_dvar: | tIVAR");
                      $$ = RubyInstVarNode::s( quidToSymbolObj( $1, vps), vps);
                   }
                | tCVAR
                   {
                      yTrace(vps, "string_dvar: | tCVAR");
                      $$ = RubyClassVarNode::s( quidToSymbolObj( $1, vps), vps);
                   }
                | backref
                ;

symbol          : tSYMBEG sym
                    {
                      yTrace(vps, "symbol: tSYMBEG sym");
                      vps->lex_state = EXPR_END;
                      $$ = $2;
                    }
                ;

sym             : fname
                | tIVAR
                | tGVAR
                | tCVAR
                ;

dsym            : tSYMBEG xstring_contents tSTRING_END
                    {
                      yTrace(vps, "dsym: tSYMBEG xstring_contents tSTRING_END");
                      vps->lex_state = EXPR_END;
                      if ( $2 == ram_OOP_NIL) {
                        rb_compile_error(vps, "empty symbol literal");
                      } else {
                        $$ = RubyParser::new_dsym($2, vps);
                      }
                    }
                ;

numeric         : tINTEGER
                | tFLOAT
                | tUMINUS_NUM tINTEGER         %prec tLOWEST
                    {
                      yTrace(vps, "numeric: tUMINUS_NUM tINTEGER");
                      om *omPtr = vps->omPtr;
                      OmScopeType aScope(omPtr);
                      NODE **aH = aScope.add($2);
                      $$ = LrgNegate(omPtr, aH);
                    }
                | tUMINUS_NUM tFLOAT           %prec tLOWEST
                    {
                      yTrace(vps, "numeric: tUMINUS_NUM tFLOAT");
                      om *omPtr = vps->omPtr;
                      OmScopeType aScope(omPtr);
                      NODE **aH = aScope.add($2);
                      double d;
                      if (! FloatPrimFetchArg(omPtr, aH, &d)) {
                        rb_compile_error(vps, "tUMINUS_NUM tFLOAT , number not a Float");
                      }
                      $$ = FloatPrimDoubleToOop(omPtr, d * -1.0 );
                    }
                ;

variable        : tIDENTIFIER
                | tIVAR
                | tGVAR
                | tCONSTANT
                | tCVAR
                | kNIL { $$ = int64ToSi( kNIL) ; }
                | kSELF { $$ = int64ToSi(kSELF); }
                | kTRUE { $$ = int64ToSi(kTRUE); }
                | kFALSE {$$ = int64ToSi(kFALSE); }
                | k__FILE__ {  $$ = int64ToSi(k__FILE__); }
                | k__LINE__ {  $$ = int64ToSi(k__LINE__); }
                ;

var_ref         : variable
                    {
                      yTrace(vps, "var_ref: variable");
                      $$ = gettable(vps, & $1);
                    }
                ;

var_lhs         : variable
                    {
                      yTrace(vps, "varLhs: variable");
                      NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                      $$ = assignable(& $1, ofsO, vps->nilH(), vps);
                    }
                ;

backref         : tNTH_REF
                  {
                    NODE *ofsO = OOP_OF_SMALL_LONG_(vps->tokenOffset());
                    $$ = RubyNthRefNode::s($1/*a SmallInt*/, ofsO, vps);
                  }
                | tBACK_REF
                  {
                    $$ = RubyBackRefNode::s($1/*a Character*/, vps);
                  }
                ;

superclass      : term
                    {
                      yTrace(vps, "superclass: Term");
                      $$ = ram_OOP_NIL;
                    }
                | '<'
                    {
                      vps->lex_state = EXPR_BEG;
                    }
                  expr_value term
                    {
                      yTrace(vps, "superclass: | tLT expr_value Term");
                      $$ = $3;
                    }
                | error term { yyerrflag = 0; $$ = ram_OOP_NIL;}
                ;

f_arglist       : '(' f_args opt_nl ')'
                    {
                      yTrace(vps, "f_arglist: tLPAREN2 f_args opt_nl tRPAREN");
                      rParenLexPop(vps);
                      $$ = $2;
                      vps->lex_state = EXPR_BEG;
                      vps->command_start = TRUE;
                    }
                | f_args term
                    {
                      yTrace(vps, "f_arglist: | f_args Term");
                      $$ = $1;
                    }
                ;

f_args          : f_arg ',' f_optarg ',' f_rest_arg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: f_arg tCOMMA f_optarg tCOMMA f_rest_arg opt_f_block_arg");
                      RubyArgsNode::add_optional_arg($1, $3, vps);
                      RubyArgsNode::add_star_arg($1, $5, vps);
                      $$ = RubyArgsNode::add_block_arg($1, $6, vps); // returns first arg
                    }
                | f_arg ',' f_optarg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: | f_arg tCOMMA f_optarg  opt_f_block_arg");
                      RubyArgsNode::add_optional_arg($1, $3, vps);
                      $$ = RubyArgsNode::add_block_arg($1, $4, vps);
                    }
                | f_arg ',' f_rest_arg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: | f_arg tCOMMA  f_rest_arg opt_f_block_arg");
                      RubyArgsNode::add_star_arg($1, $3, vps);
                      $$ = RubyArgsNode::add_block_arg($1, $4, vps);
                    }
                | f_arg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: | f_arg  opt_f_block_arg");
                      $$ = RubyArgsNode::add_block_arg($1, $2, vps);
                    }
                | f_optarg ',' f_rest_arg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: | f_optarg tCOMMA f_rest_arg opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_optional_arg(*argsH, $1, vps);
                      RubyArgsNode::add_star_arg(*argsH, $3, vps);
                      $$ = RubyArgsNode::add_block_arg(*argsH, $4, vps); // returns first arg
                    }
                | f_optarg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: |  f_optarg  opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_optional_arg(*argsH, $1, vps);
                      $$ = RubyArgsNode::add_block_arg(*argsH, $2, vps); // returns first arg
                    }
                | f_rest_arg opt_f_block_arg
                    {
                      yTrace(vps, "f_args: | f_rest_arg opt_f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      RubyArgsNode::add_star_arg(*argsH, $1, vps);
                      $$ = RubyArgsNode::add_block_arg(*argsH, $2, vps); // returns first arg
                    }
                | f_block_arg
                    {
                      yTrace(vps, "f_args: |  f_block_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      $$ = RubyArgsNode::add_block_arg(*argsH, $1, vps);
                    }
                | /* none */
                    {
                      yTrace(vps, "f_args: | <nothing>");
                      $$ = RubyArgsNode::new_(vps);
                    }
                ;

f_norm_arg      : tCONSTANT
                    {
                        rb_compile_error(vps, "formal argument cannot be a constant");
                    }
                | tIVAR
                    {
                        rb_compile_error(vps, "formal argument cannot be an instance variable");
                    }
                | tGVAR
                    {
                        rb_compile_error(vps, "formal argument cannot be a global variable");
                    }
                | tCVAR
                    {
                        rb_compile_error(vps, "formal argument cannot be a class variable");
                    }
                | tIDENTIFIER
                    {
                      yTrace(vps, "f_norm_arg: | tIDENTIFIER");
                      OmScopeType aScope(vps->omPtr);
                      NODE *quidO = asQuid($1, vps);
                      if (! is_local_id(quidO)) {
                          rb_compile_error_q(vps, "formal argument must be local variable", quidO);
                      } else if (local_id(vps, quidO)) {
                          rb_compile_error_q(vps, "duplicate argument name", quidO);
                      }
                      local_cnt(vps, quidO);
                      $$ = $1 ;
                    }
                ;

f_arg           : f_norm_arg
                    { yTrace(vps, "f_arg: f_norm_arg");
                      OmScopeType aScope(vps->omPtr);
                      NODE **argsH = aScope.add(RubyArgsNode::new_(vps));
                      $$ = RubyArgsNode::add_arg(argsH, $1/*RpNameToken*/, vps);  // returns first arg
                    }

                | f_arg ',' f_norm_arg
                    {
                      yTrace(vps, "f_arg: | f_arg tCOMMA f_norm_arg");
                      $$ = RubyArgsNode::add_arg(& $1, $3/*RpNameToken*/, vps);
                    }
                ;

f_opt           : tIDENTIFIER '=' arg_value
                    {
                      yTrace(vps, "f_opt: tIDENTIFIER tEQL arg_value");
                      OmScopeType aScope(vps->omPtr);
                      NODE *quidO = asQuid($1, vps);
                      if (! is_local_id(quidO)) {
                          rb_compile_error_q(vps, "formal argument must be local variable", quidO);
                      } else if (local_id(vps, quidO)) {
                          rb_compile_error_q(vps, "duplicate optional argument name", quidO);
                      }
                      NODE **thirdH = aScope.add($3);
                      $$ = assignable(& $1, $2/*srcOffsetSi*/, thirdH, vps);
                    }
                ;

f_optarg        : f_opt
                    {
                      yTrace(vps, "f_optarg: f_opt");
                      $$ = RubyBlockNode::s( $1, vps);
                    }
                | f_optarg ',' f_opt
                    {
                      yTrace(vps, "f_optarg: | f_optarg tCOMMA f_opt");
                      $$ = RubyBlockNode::append_to_block($1, $3, vps);
                    }
                ;

restarg_mark    : '*'
                | tSTAR
                ;

f_rest_arg      : restarg_mark tIDENTIFIER
                    {
                      yTrace(vps, "f_rest_arg: restarg_mark tIDENTIFIER");
                      NODE *quidO = asQuid($2, vps);
                      if (! is_local_id(quidO)) {
                          rb_compile_error("rest argument must be local variable", vps);
                      } else if (local_id(vps, quidO)) {
                          rb_compile_error("duplicate rest argument name", vps);
                      }
                      local_cnt(vps, quidO);
                      $$ = $2 /* a RpNameToken or quid*/;
                    }
                | restarg_mark
                    {
                      yTrace(vps, "f_rest_arg: | restarg_mark");
                      $$ = RpNameToken::s(a_sym_rest_args, vps);
                    }
                ;

blkarg_mark     : '&'
                | tAMPER
                ;

f_block_arg     : blkarg_mark tIDENTIFIER
                    {
                      yTrace(vps, "f_block_arg: blkarg_mark tIDENTIFIER");
                      NODE *quidO = asQuid($2, vps);
                      if (! is_local_id(quidO)) {
                          rb_compile_error("block argument must be local variable", vps);
                      } else if (local_id(vps, quidO)) {
                          rb_compile_error("duplicate block argument name", vps);
                      }
                      local_cnt(vps, quidO);
                      $$ = RubyBlockArgNode::s(RpNameToken::symval($2, vps), vps);
                    }
                ;

opt_f_block_arg : ',' f_block_arg
                    {
                      yTrace(vps, "opt_f_block_arg: tCOMMA f_block_arg");
                      $$ = $2;
                    }
                | none
                    {
                      yTrace(vps, "opt_f_block_arg: | <nothing>");
                      $$ = ram_OOP_NIL;
                    }
                ;

singleton       : var_ref
                    {
                        yTrace(vps, "singleton : var_ref");
                        $$ = $1;
                    }
                | '(' { vps->lex_state = EXPR_BEG;} expr opt_nl ')'
                    {
                       yTrace(vps, "singleton: ___ expr opt_nl tRPAREN");
                       rParenLexPop(vps);
                       if ($3 == ram_OOP_NIL) {
                         rb_compile_error("can't define singleton method for ().", vps);
                       } else if (RubyAbstractLiteralNode::kind_of($3, vps)) {
                         rb_compile_error("can't define singleton method for literals", vps);
                       }
                       $$ = $3; // rubinius had  $$  =  value_expr($3);
                    }
                ;

assoc_list      : none
                    {
                      yTrace(vps, "assoc_list: none");
                      $$ = RubyArrayNode::new_(vps);
                    }
                | assocs trailer
                    {
                      yTrace(vps, "assoc_list: | assocs trailer");
                      $$ = $1;
                    }
                | args trailer
                    {
                      yTrace(vps, "assoc_list: | args trailer");
                      if ((RubyArrayNode::arrayLength($1, vps) & 1) != 0) {
                        rb_compile_error("odd number list for Hash", vps);
                      }
                      $$ = $1;
                    }
                ;

assocs          : assoc
                | assocs ',' assoc
                    {
                      yTrace(vps, "assocs: | assocs tCOMMA assoc");
                      $$ = RubyArrayNode::appendAll($1, $3, vps); // returns first arg
                    }
                ;

assoc           : arg_value tASSOC arg_value
                    {
                      yTrace(vps, "assoc: arg_value tASSOC arg_value");
                      $$ = RubyArrayNode::s_a_b( $1, $3, vps);
                    }
                  | tLABEL arg_value
                    {
                      yTrace(vps, "assoc: arg_value tLABEL arg_value");
                      $$ = RubyArrayNode::s_a_b(RubySymbolNode::s($1, vps), $2, vps);
                    }
                ;

operation       : tIDENTIFIER
                | tCONSTANT
                | tFID
                ;

operation2      : tIDENTIFIER
                | tCONSTANT
                | tFID
                | op
                ;

operation3      : tIDENTIFIER
                | tFID
                | op
                ;

dot_or_colon    : '.'
                | tCOLON2
                ;

opt_terms       : /* none */
                | terms
                ;

opt_nl          : /* none */
                | '\n'
                ;

trailer         : /* none */
                | '\n'
                | ','
                ;

term            : ';' { yyerrflag = 0 ;}
                | '\n'
                ;

terms           : term
                | terms ';' { yyerrflag = 0;}
                ;

none            : /* none */ {  yTrace(vps, "none:");  $$ = ram_OOP_NIL; }
                ;
%%


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
    case  cls_RubyAliasNode:            nam = "RubyAliasNode"; break;
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
    case sel_add_arg:           str = "add_arg:";       break;
    case sel_add_block_arg:     str = "add_block_arg:"; break;
    case sel_add_optional_arg:  str = "add_optional_arg:";      break;
    case sel_add_star_arg:      str = "add_star_arg:";  break;
    case sel__append:           str = "_append:";       break;
    case sel__appendAll:        str = "_appendAll:"; break;
    case sel__append_amperLhs:  str = "_appendAmperLhs:";  break;
    case sel_append_arg:        str = "append_arg:"; break;
    case sel_append_arg_blkArg: str = "append_arg:blkArg:"; break;
    case sel_append_arg_splatArg_blkArg: str = "append_arg:splatArg:blkArg:"; break;
    case sel_append_blkArg:     str = "append_blk_arg:"; break;
    case sel_append_splatArg:   str = "append_splatArg:"; break;
    case sel_append_splatArg_blk:  str = "append_splatArg:blk:"; break;
    case sel_append_to_block:   str = "append_to_block:"; break;
    case sel_appendTo_evstr2dstr: str = "appendTo:evstr2dstr:";  break;
    case sel_arrayLength:       str = "arrayLength"; break;
    case sel_block_append:      str = "block_append:tail:"; break;
    case sel_colon2_name:       str = "colon2:name:"; break;
    case sel_colon3:            str = "colon3:"; break;
    case sel_callNode_:         str = "callNode:"; break;
    case sel_backref_error:     str = "backref_error:" ; break;
    case sel_bodyNode_:         str = "bodyNode:"; break;
    case sel_includesTemp_:   str = "includesTemp:"; break;
    case sel_get_match_node:   str = "get_match_node:rhs:ofs:"; break;
    case sel_list_append:       str = "list_append:item:"; break;
    case sel_list_prepend:      str = "list_prepend:item:"; break;
    case sel_literal_concat:   str = "literal_concat:tail:"; break;
    case sel_logop:             str = "logop:left:right:"; break;
    case sel_masgn_append_arg: str = "masgn_append_arg:right:"; break;
    case sel_masgn_append_mrhs: str = "masgn_append_mrhs:right:"; break;
    case sel__new:              str = "_new"; break;
    case sel__new_:             str = "_new:"; break;
    case sel__new_with:         str = "_new:with:"; break;
    case sel_new_aref:          str = "new_aref:args:ofs:"; break;
    case sel_new_call:          str = "new_call:sel:arg:"; break;
    case sel_new_call_1:        str = "new_call_1:sel:arg:"; break;
    case sel_new_call_braceBlock: str = "new_call_braceBlock:sel:args:blkArg:"; break;
    case sel_new_defn:  str = "new_defn:args:body:ofs:startLine:endOfs:"; break;
    case sel_new_defs:  str = "new_defs:name:args:body:ofs:startLine:endOfs:"; break;
    case sel_new_dsym:          str = "new_dsym:"; break;
    case sel_new_evstr:         str = "new_evstr:"; break;
    case sel_new_fcall:         str = "new_fcall:arg:"; break;
    case sel_new_fcall_braceBlock: str = "new_fcall_braceBlock:args:blkArg:"; break;
    case sel_new_if:            str = "new_if:t:f:ofs:"; break;
    case sel_new_op_asgn:       str = "new_op_asgn:sel:arg:"; break;
    case sel_new_parasgn:       str = "new_parasgn:ofs:comma:"; break;
    case sel_new_regexp:        str = "new_regexp:options:"; break;
    case sel_new_string:        str = "new_string:"; break;
    case sel_new_super:         str = "new_super:ofs:"; break;
    case sel_new_undef:         str = "new_undef:ofs:"; break;
    case sel_new_until:         str = "new_until:expr:ofs:"; break;
    case sel_new_vcall:         str = "new_vcall:sel:"; break;
    case sel_new_while:         str = "new_while:expr:ofs:"; break;
    case sel_new_xstring:       str = "new_xstring:"; break;
    case sel_new_yield:         str = "new_yield:ofs:"; break;
    case sel_node_assign:       str = "node_assign:ofs:rhs:"; break;
    case sel_opt_rescue:        str = "opt_rescue:var:body:rescue:ofs:"; break;
    case sel_ret_args:          str = "ret_args:"; break;
    case sel_s_a:               str = "s_a:"; break;
    case sel_s_a_b:             str = "s_a:b:"; break;
    case sel_s_a_b_c:           str = "s_a:b:c:"; break;
    case sel_s_a_b_c_d:         str = "s_a:b:c:d:"; break;
    case sel_s_a_b_c_d_e:       str = "s_a:b:c:d:e:"; break;
    case sel_s_splat_blk:       str = "s_splat:blk:"; break;
    case sel_s_a_blk:           str = "s_a:blk:"; break;
    case sel_s_a_splat_blk:     str = "s_a:splat:blk:"; break;
    case sel_s_a_b_blk:         str = "s_a:b:blk:"; break;
    case sel_s_a_b_splat_blk:   str = "s_a:b:splat:blk:"; break;
    case sel_a_all_b_blk:       str = "s_a:all:b:blk:"; break;
    case sel_a_all_b_splat_blk: str = "s_a:all:b:splat:blk:"; break;
    case sel_sym_srcOffset:     str = "sym:srcOffset:"; break;
    case sel_setParen:          str = "setParen"; break;
    case sel_sym_ofs_val:       str = "sym:ofs:val:"; break;
    case sel_uplus_production : str = "uplus_production:ofs:"; break;
    case sel_value_expr:        str = "value_expr:"; break;
#if !defined(FLG_LINT_SWITCHES)
    default:
#endif
    case NUM_AST_SELECTORS:
      GemErrAnsi(omPtr, ERR_ArgumentError, NULL, "invalid enum value in initAstSelector");
  }
  OmScopeType aScope(omPtr);
  NODE **strH = aScope.add( om::NewString_(omPtr, str));
  NODE* symO = ObjExistingCanonicalSym__(omPtr, strH);
  if (symO == NULL) {
    printf( "non-existant symbol %s in initAstSelector\n", str);
    return FALSE;
  }
  OopType selObjId = om::objIdOfObj__(omPtr, symO);
  selectorIds[e_sel] = OOP_makeSelectorId(0, selObjId);
  return TRUE;
}

static void initAstSymbol(om *omPtr, NODE** symbolsH, AstSymbolEType e_sym)
{
  const char* str = NULL;
  switch (e_sym) {
    case a_sym_or:      str = "or";     break;
    case a_sym_orOp: str = "|"; break;
    case a_sym_OOR: str = "||"; break;
    case a_sym_upArrow: str = "^"; break;
    case a_sym_andOp: str = "&"; break;
    case a_sym_and:   str = "and";      break;
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

    case  a_sym_end:    str = "end";    break;
    case  a_sym_else:   str = "else";   break;
    case  a_sym_module: str = "module";         break;
    case  a_sym_elsif:  str = "elsif";  break;
    case  a_sym_def:    str = "def";    break;
    case  a_sym_rescue: str = "rescue";         break;
    case  a_sym_then:   str = "then";   break;
    case  a_sym_self:   str = "self";   break;
    case  a_sym_if:     str = "if";     break;
    case  a_sym_do:     str = "do";     break;
    case  a_sym_nil:    str = "nil";    break;
    case  a_sym_until:  str = "until";  break;
    case  a_sym_unless: str = "unless";         break;
    case  a_sym_begin:  str = "begin";  break;
    case  a_sym__LINE_: str = "__LINE__";       break;
    case  a_sym__FILE_: str = "__FILE__";       break;
    case  a_sym_END:    str = "END";    break;
    case  a_sym_BEGIN:  str = "BEGIN";  break;
    case  a_sym_while:  str = "while";  break;
    case  a_sym_rest_args: str = "rest_args";   break;

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
  omPtr->set_rubyParseState( ps);
  ps->omPtr = omPtr;

  ps->yystack.initialize();
  yygrowstack(ps, NULL);
  omPtr->set_rubyParseStack(&ps->yystack) ;

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

  rb_parse_state *ps = omPtr->rubyParseState();
  if (ps == NULL || ! ps->parserActive)
    return ram_OOP_FALSE; // caller should signal an Exception

  omObjSType *strO = *strH;
  if ( om::strCharSize(strO) != 1)
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
    if (! om::isCByteArray(cbytesO) )
      return NULL;
  }
  { omObjSType *srcO = *sourceH;
    if (om::strCharSize(srcO) != 1)
      return NULL;
  }
  { omObjSType *fileO = *fileNameH;
    if (om::strCharSize(fileO) != 1)
      return NULL;
  }
  rb_parse_state *ps = omPtr->rubyParseState();
  if (ps == NULL) {
    // this path executed on first parse during session only
    ps = (rb_parse_state*)malloc( sizeof(*ps) );
    sessionInit(omPtr, ps);
    omPtr->set_rubyParseState( ps);
    omPtr->set_rubyParseStack(&ps->yystack) ;
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

  ps->cst = omPtr->compilerState();
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
    UTL_ASSERT(om::isCByteArray(cbytesO));
    int64 info = om::FetchSmallInt__(*cbytesH, OC_CByteArray_info);
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
  } else {                      /* handle \uxxxx form */
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
            om::AppendToString_(omPtr, strValH, p, pend - p);
            if (pend < ps->lex_pend) {
              om::AppendToString_(omPtr, strValH, "\n", 1);
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

    // c = nextc();                     // Rubinius has commented out (uncomment for debug?)
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
        pushback(c, ps);
        return '\\';

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
  QUID *nList = (QUID*)ComHeapMalloc(ps->omPtr->compilerState(), sizeof(QUID) * allocatedSize * 2);
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
    OopType symId = om::objIdOfObj__(ps->omPtr, symO);
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
