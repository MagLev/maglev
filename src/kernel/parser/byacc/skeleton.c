/* $Id: skeleton.c,v 1.25 2010/06/07 21:24:58 Andres.Mejia Exp $ */

#include "defs.h"

const char *banner[] =
{
    "#define YYBYACC 1",
    CONCAT1("#define YYMAJOR ", YYMAJOR),
    CONCAT1("#define YYMINOR ", YYMINOR),
#ifdef YYPATCH
    CONCAT1("#define YYPATCH ", YYPATCH),
#endif
    "",
    "enum {  YYEMPTY  =  -1 }; ",
    /* define yyclearin      (yychar = YYEMPTY) */ 
    /* define yyerrok        (yyerrflag = 0) , */
    /* define YYRECOVERING() (yyerrflag != 0) */ 
    "",
    0
};

const char *xdecls[] =
{
    "/*extern int YYPARSE_DECL();*/",
    "/*extern int YYLEX_DECL();*/",
    "",
    0
};

const char *tables[] =
{
    "extern short yylhs[];",
    "extern short yylen[];",
    "extern short yydefred[];",
    "extern short yydgoto[];",
    "extern short yysindex[];",
    "extern short yyrindex[];",
    "extern short yygindex[];",
    "extern short yytable[];",
    "extern short yycheck[];",
    "",
    "#if YYDEBUG",
    "extern char *yyname[];",
    "extern char *yyrule[];",
    "#endif",
    0
};

const char *hdr_defs[] =
{
   // deleted    define the initial stack-sizes 
   // see yystack_ constants in rubyparser.h
    "",
    "/* yydebug defined in .y file now */",
    "static int  yynerrs = 0;",
    "",
    0
    /* deleted YYSTACKDATA , see YyStackData in rubyparser.h*/
};

const char *hdr_vars[] =
{
  /* Maglev not used,   only used if  not pure_parser */
    "int      yyerrflag;",
    "int      yychar;",
    "YYSTYPE  yyval;",
    "YYSTYPE  yylval;",
    "",
    "/* variables for the parser stack */",
    "static YYSTACKDATA yystack;",
    0
};

const char *body_vars[] =
{
    "    int      yyerrflag; /* named yyerrstatus in bison*/ ",
    "    int      yychar;",
    "    const short *unifiedTable = yyUnifiedTable; ",
    "",
    "    /* variables for the parser stack */",
    "    YyStackData *yystack = &vps->yystack ;",
    0
};

const char *body_1[] =
{
    "",
    "#if YYDEBUG",
    "#include <stdio.h>		/* needed for printf */",
    "#endif",
    "",
    "#include <stdlib.h>	/* needed for malloc, etc */",
    "#include <string.h>	/* needed for memset */",
    "",
    /* user must provide int yygrowstack(rb_parse_state *ps, YyStackElement* markPtr)*/
    "",
    /* yyfreestack not used */
    "",
    /* define YYABORT  goto yyabort */  /* not used in Maglev grammar.y*/
    /* #define YYREJECT goto yyabort */
    /* #define YYACCEPT goto yyaccept */
    /* #define YYERROR  goto yyerrlab */
    "",
    "static int yyparse(rb_parse_state* vps)",
    "{",
    0
};

const char *body_2[] =
{
    "    intptr_t yym, yyn, yystate;",
    /* deleted getenv(YYDEBUG) code */
    "",
    "    yynerrs = 0;",
    "    yyerrflag = 0;",
    "    yychar = YYEMPTY;",
    "",
    "    yystate = 0;",
    "    UTL_ASSERT(yystack->stacksize > 0);",
    "    YyStackElement* yymarkPtr = yystack->base;",
    "    yystack->mark = yymarkPtr;",
    "    yymarkPtr->state = yystate;",
    "    yymarkPtr->obj = ram_OOP_NIL;",
    "",
    "yyloop:",
    "    yyn = unifiedTable[yystate + defredBASE]/*yydefred[yystate]*/ ;",
    "    if (yyn != 0) {",
    "      goto yyreduce;",
    "    }",
    "    if (yychar < 0) {",
    "        yychar = yylex(vps); ",
    "        UTL_ASSERT(yychar >= 0); ",
    "#if YYDEBUG",
    "        if (yydebug)",
    "        {",
    "            const char *yys = NULL;",
    "            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];",
    "            if (!yys) yys = \"illegal-symbol\";",
    "            printf(\"%sdebug: state %ld, reading %d (%s)\\n\",",
    "                    YYPREFIX, yystate, yychar, yys);",
    "            yytrap();",
    "        }",
    "#endif",
    "    }",
    "    yyn = unifiedTable[yystate + sindexBASE] /*yysindex[x]*/;",
    "    if (yyn) { ",
    "      yyn += yychar; ",
    "      if ((uint64)yyn <= YYTABLESIZE) {",
    "        int yChk = unifiedTable[yyn + checkBASE]; /*yycheck[yyn]*/ ",
    "        if (yChk == yychar) {",
    "          intptr_t new_state = unifiedTable[yyn + tableBASE]; /* yytable[yyn]*/",
    "#if YYDEBUG",
    "          if (yydebug) { ",
    "            printf(\"%sdebug: state %ld, shifting to state %ld\\n\",",
    "                    YYPREFIX, yystate, new_state );",
    "          }",
    "#endif",
    "        if (yymarkPtr >= yystack->last) {",
    "          yymarkPtr = yygrowstack(vps, yymarkPtr); ",
    "          if (yymarkPtr == NULL) { ",
    "            yyerror(\"yacc stack overflow\", vps);",
    "            return 1;",
    "          } ",
    "        }",
    "        yystate = new_state; ",
    "        yymarkPtr += 1; ",
    "        yystack->mark = yymarkPtr ; ",
    "        yymarkPtr->state = yystate ; ",
    "        yymarkPtr->obj = *vps->lexvalH ;",
    "        yychar = YYEMPTY;",
    "        if (yyerrflag > 0)  --yyerrflag;",
    "        goto yyloop;",
    "    }}}",
    "    yyn = unifiedTable[yystate + rindexBASE]/*yyrindex[x]*/ ; ",
    "    if (yyn) {",
    "      yyn += yychar; ",
    "      if ((uint64)yyn <= YYTABLESIZE) {",
    "         int yChk = unifiedTable[yyn + checkBASE]; /*yycheck[yyn]*/",
    "         if (yChk == yychar) {",
    "           yyn = unifiedTable[yyn + tableBASE]; /*yytable[yyn]*/ ",
    "           goto yyreduce;",
    "    }}}",
    "    if (yyerrflag) goto yyinrecovery;",
    "",
    "    yyStateError(yystate, yychar, vps);",
    "",
    "    goto yyerrlab;",
    "",
    "yyerrlab:",
    "    ++yynerrs;",
    "",
    "yyinrecovery:",
    "  if (yyerrflag < 3) {",
    "    yyerrflag = 3;",
    "    for (;;) {",
    "      yyn = unifiedTable[yymarkPtr->state + sindexBASE]/*yysindex[x]*/;",
    "      if (yyn) { ",
    "        yyn += YYERRCODE;",
    "        if (yyn >= 0) {",
    "          if ((uint64)yyn <= YYTABLESIZE) {",
    "            int yChk = unifiedTable[yyn + checkBASE]; /*yycheck[yyn]*/",
    "            if (yChk == YYERRCODE) {",
    "              intptr_t new_state = unifiedTable[yyn + tableBASE]; /*yytable[yyn]*/",
    "#if YYDEBUG",
    "              if (yydebug) {",
    "                  printf(\"%sdebug: state %d, error recovery shifting to state %ld\\n\",",
    "                           YYPREFIX, yymarkPtr->state, new_state);",
    "              }",
    "#endif",
    "              if (yymarkPtr >= yystack->last) {",
    "                  yymarkPtr = yygrowstack(vps, yymarkPtr); ",
    "                  if (yymarkPtr == NULL) {",
    "                    yyerror(\"yacc stack overflow\", vps);",
    "                    return 1;",
    "                  } ",
    "              }",
    "              yystate = new_state;",
    "              yymarkPtr += 1; ",
    "              yystack->mark = yymarkPtr ; ",
    "              yymarkPtr->state = yystate ; ",
    "              yymarkPtr->obj = *vps->lexvalH ; ",
    "              goto yyloop;",
    "      }}}}",
    "#if YYDEBUG",
    "          if (yydebug) { ",
    "             printf(\"%sdebug: error recovery discarding state %d \\n\",",
    "                            YYPREFIX, yymarkPtr->state);",
    "          } ",
    "#endif",
    "          if (yymarkPtr <= yystack->base) {",
    "            yTrace(vps, \"yyabort\");",
    "            return 1; ",
    "          }",
    "          yymarkPtr -= 1; ",
    "          yystack->mark = yymarkPtr; ",
    "        }",
    "  } else { ",
    "        if (yychar == 0) {",
    "          yTrace(vps, \"yyabort\");",
    "          return 1;",
    "        }",
    "#if YYDEBUG",
    "        if (yydebug) {",
    "            const char* yys = NULL;",
    "            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];",
    "            if (!yys) yys = \"illegal-symbol\";",
    "            printf(\"%sdebug: state %ld, error recovery discards token %d\
 (%s)\\n\",",
    "                    YYPREFIX, yystate, yychar, yys);",
    "        }",
    "#endif",
    "        yychar = YYEMPTY;",
    "        goto yyloop;",
    "    }",
    "",
    "yyreduce:",
    "#if YYDEBUG",
    "    if (yydebug) { ",
    "        printf(\"%sdebug: state %ld, reducing by rule %ld (%s)\\n\",",
    "                YYPREFIX, yystate, yyn, yyrule[yyn]);",
    "    }" ,
    "#endif",
    "    yym = unifiedTable[yyn + lenBASE] /*yylen[yyn]*/ ;",
    "    YyStackElement* yyvalPtr; ",
    "    if (yym) {",
    "       UTL_ASSERT( yymarkPtr == yystack->mark );",
    "       yyvalPtr = yymarkPtr + 1 - yym ; ",
    "    } else {",
    "       yyvalPtr = NULL; // was memset(&yyval, 0, sizeof yyval)",
    "    } ", 
    "    NODE*  yyvalO = NULL; ",
    "    switch (yyn) {",
    "      /* no default: in this switch */",
    0
};

const char *trailer[] =
{  
    "    }",  
    "    if (yyvalO == NULL) {  /*compute default state result*/ ",
    "      if (yyvalPtr != NULL) {",
    "        yyvalO = yyvalPtr->obj;",
    "      } else {",
    "        yyvalO = ram_OOP_NIL;",
    "      }",
    "    }",
    "    yymarkPtr -= yym;",
    "    yystack->mark = yymarkPtr ;",
    "    yystate = yymarkPtr->state ;",
    "    yym = unifiedTable[yyn + lhsBASE]; /* yylhs[yyn]*/ ;",
    "    if ((yystate | yym) == 0 /*yystate==0 && yym == 0*/ ) {",
    "#if YYDEBUG",
    "        if (yydebug)",
    "            printf(\"%sdebug: after reductionZ, shifting from state 0 to state %d\\n\", ",
    "                       YYPREFIX, YYFINAL);",
    "#endif",
    "        yystate = YYFINAL;",
    "        UTL_ASSERT(yymarkPtr < yystack->last);",
    "        yymarkPtr += 1;",
    "        yystack->mark = yymarkPtr ; ", 
    "        yymarkPtr->state = YYFINAL;",
    "        yymarkPtr->obj = yyvalO ;",
    "        if (yychar < 0) {",
    "            yychar = yylex(vps); ",
    "            UTL_ASSERT(yychar >= 0); ",
    "#if YYDEBUG",
    "            if (yydebug) {",
    "                const char* yys = NULL;",
    "                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];",
    "                if (!yys) yys = \"illegal-symbol\";",
    "                printf(\"%sdebug: state %d, reading %d (%s)\\n\",",
    "                        YYPREFIX, YYFINAL, yychar, yys);",
    "                yytrap();",
    "            }",
    "#endif",
    "        }",
    "        if (yychar == 0) goto yyaccept;",
    "        goto yyloop;",
    "    }",
    "    yyn = unifiedTable[yym + gindexBASE]/*yygindex[x]*/;",
    "    if (yyn) {",
    "      yyn += yystate;",
    "      if ((uint64)yyn <= YYTABLESIZE) {",
    "        int yChk = unifiedTable[yyn + checkBASE]/*yycheck[yyn]*/;",
    "        if (yChk == yystate) {",
    "          yystate = unifiedTable[yyn + tableBASE];",
    "          goto reduction2 ;",
    "    }}}",
    "    yystate = unifiedTable[yym + dgotoBASE]/*yydgoto[yym]*/ ;",
    "reduction2: ;",
    "#if YYDEBUG",
    "    if (yydebug)",
    "        printf(\"%sdebug: after reduction, shifting from state %d to state %ld\\n\", ",
    "               YYPREFIX, yystack->mark->state, yystate);",
    "#endif",
    "    if (yymarkPtr >= yystack->last) { ",
    "        *vps->yyvalH = yyvalO;",
    "        yymarkPtr = yygrowstack(vps, yymarkPtr); ",
    "        if (yymarkPtr == NULL) { ",
    "           yyerror(\"yacc stack overflow\", vps);",
    "           return 1;",
    "        } ",
    "        yyvalO = *vps->yyvalH; ",
    "    }",
    "    yymarkPtr += 1 ; ",
    "    yystack->mark = yymarkPtr ; ",
    "    yymarkPtr->state = yystate ; ",
    "    yymarkPtr->obj = yyvalO; ",
    "    goto yyloop;",
    "",
    "",
    "yyaccept:",
    "    /* yyfreestack(&yystack);*/ ",
    "    return 0;",
    "}",
    0
};

void
write_section(const char *section[])
{
    int c;
    int i;
    const char *s;
    FILE *f;

    f = code_file;
    for (i = 0; (s = section[i]) != 0; ++i)
    {
	++outline;
	while ((c = *s) != 0)
	{
	    putc(c, f);
	    ++s;
	}
	putc('\n', f);
    }
}
