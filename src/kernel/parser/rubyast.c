#include "rubyom.hf"
#include "rubyparser.h"
#include "rubyast.hf"

omObjSType* RubyNode::basicNew(AstClassEType cls_e, rb_parse_state *ps)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
  return om::NewObj(omPtr, clsH);
}

omObjSType* RubyNode::call(omObjSType *rcvr, AstSelectorEType sel_e, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **rcvrH = scp.add(rcvr);
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim_(omPtr, rcvrH, selId);
}

omObjSType* RubyNode::call(omObjSType *rcvr, AstSelectorEType sel_e, omObjSType *arg, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **rcvrH = scp.add(rcvr);
    omObjSType **argH = scp.add(arg);
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, rcvrH, selId, 1, &argH); 
}

omObjSType* RubyNode::call(omObjSType *rcvr, AstSelectorEType sel_e, 
			omObjSType *a, omObjSType *b, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **rcvrH = scp.add(rcvr);
    omObjSType **aH[2];
    aH[0] = scp.add(a);
    aH[1] = scp.add(b);
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, rcvrH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(omObjSType *rcvr, AstSelectorEType sel_e, 
			omObjSType *a, omObjSType *b, omObjSType *c, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **rcvrH = scp.add(rcvr);
    omObjSType **aH[3];
    aH[0] = scp.add(a);
    aH[1] = scp.add(b);
    aH[2] = scp.add(c);
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, rcvrH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(omObjSType *rcvr, AstSelectorEType sel_e, omObjSType *a, 
		      omObjSType *b, omObjSType *c, omObjSType *d, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **rcvrH = scp.add(rcvr);
    omObjSType **aH[4];
    aH[0] = scp.add(a);
    aH[1] = scp.add(b);
    aH[2] = scp.add(c);
    aH[3] = scp.add(d);
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, rcvrH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, 0, NULL);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
		omObjSType *arg, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **argH = scp.add( arg );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, 1, &argH);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
		omObjSType *a, omObjSType *b, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **aH[2];
    aH[0] = scp.add( a );
    aH[1] = scp.add( b );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    int64 numArgs = UTL_ARRAY_LENGTH(aH);
    omObjSType *res = om::IntRecurFromPrim__(omPtr, clsH, selId, numArgs, aH);
    return res;
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
		omObjSType *a, omObjSType *b, omObjSType *c, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **aH[3]; 
    aH[0] = scp.add( a );
    aH[1] = scp.add( b );
    aH[2] = scp.add( c );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
  	 omObjSType *a, omObjSType *b, omObjSType *c, omObjSType *d, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **aH[4]; 
    aH[0] = scp.add( a );
    aH[1] = scp.add( b );
    aH[2] = scp.add( c );
    aH[3] = scp.add( d );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
  	 omObjSType *a, omObjSType *b, omObjSType *c, omObjSType *d, 
	 omObjSType *e, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **aH[5]; 
    aH[0] = scp.add( a );
    aH[1] = scp.add( b );
    aH[2] = scp.add( c );
    aH[3] = scp.add( d );
    aH[4] = scp.add( e );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
  	 omObjSType *a, omObjSType *b, omObjSType *c, omObjSType *d, 
	 omObjSType *e, omObjSType *f, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **aH[6]; 
    aH[0] = scp.add( a );
    aH[1] = scp.add( b );
    aH[2] = scp.add( c );
    aH[3] = scp.add( d );
    aH[4] = scp.add( e );
    aH[5] = scp.add( f );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

omObjSType* RubyNode::call(AstClassEType cls_e, AstSelectorEType sel_e,
  	 omObjSType *a, omObjSType *b, omObjSType *c, omObjSType *d, 
	 omObjSType *e, omObjSType *f, omObjSType *g, rb_parse_state *ps)
{
    om *omPtr = ps->omPtr;
    OmScopeType scp(omPtr);
    omObjSType **aH[7]; 
    aH[0] = scp.add( a );
    aH[1] = scp.add( b );
    aH[2] = scp.add( c );
    aH[3] = scp.add( d );
    aH[4] = scp.add( e );
    aH[5] = scp.add( f );
    aH[6] = scp.add( g );
    omObjSType **clsH = scp.add(om::FetchOop(*ps->astClassesH, cls_e));
    OopType selId = ps->astSelectorIds[sel_e];
    return om::IntRecurFromPrim__(omPtr, clsH, selId, UTL_ARRAY_LENGTH(aH), aH);
}

void RubyNode::checkInstanceOf(omObjSType **objH, AstClassEType cls_e, rb_parse_state *ps)
{
  omObjSType *clsO =  om::FetchOop(*ps->astClassesH, cls_e);
  if (clsO != ps->omPtr->FetchClassObj_(*objH)) {
    GemSupErr_oo_(RT_ERR_BAD_ARG_KIND, *objH, clsO);
  }
}

BoolType RubyNode::is_a(omObjSType *o, AstClassEType cls_e, rb_parse_state *ps)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  omObjSType **objH = scp.add( o);
  omObjSType *cls = om::FetchOop(*ps->astClassesH, cls_e);
  return cls == omPtr->FetchClassObj_(*objH);
}

omObjSType* RpNameToken::s(const char* str, omObjSType *srcOfs, rb_parse_state *ps)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  omObjSType **symH = scp.add( ObjNewSym(omPtr, str));
  return RpNameToken::s(symH, srcOfs, ps);
}

omObjSType* RpNameToken::s(AstSymbolEType e_sym, omObjSType *srcOfs, rb_parse_state *ps)
{
  OmScopeType scp(ps->omPtr);
  omObjSType **symH = scp.add( om::FetchOop(*ps->astSymbolsH, e_sym));
  UTL_ASSERT(*symH != ram_OOP_NIL);
  return RpNameToken::s(symH, srcOfs, ps);
}
  
omObjSType* RpNameToken::quidForSym(omObjSType *sym, om *omPtr) 
{
  // result is a SmallInteger
  // used for symbol creation other than via rb_parser_sym()
  OopType symId = om::objIdOfObj__(omPtr, sym);
  return buildQuid(symId, RpNameToken::tLastToken() + 1, ID_JUNK);
}


omObjSType* RpNameToken::s(omObjSType **symH, omObjSType *srcOfs, rb_parse_state *ps)
{
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  omObjSType **clsH = scp.add( om::FetchOop(*ps->astClassesH, my_cls));
  omObjSType **resH = scp.add(om::NewObj(omPtr, clsH));
  om::StoreOop(omPtr, resH, val_ofs, symH);
  UTL_ASSERT(OOP_IS_SMALL_INT(srcOfs));
  omObjSType **tmpH = scp.add(srcOfs);
  om::StoreOop(omPtr, resH, src_offset_ofs, tmpH);
  *tmpH = quidForSym(*symH, omPtr);
  om::StoreOop(omPtr, resH, quid_ofs, tmpH);
  return *resH;
}

omObjSType* RpNameToken::s(rb_parse_state *ps, omObjSType *quidO)
{
  UTL_ASSERT(OOP_IS_SMALL_INT(quidO));
  int64 qval = OOP_TO_I64(quidO);
  int64 oopNum = qval >> ID_symOopNum_SHIFT;
  OopType symId = BIT_TO_OOP(oopNum);
  om *omPtr = ps->omPtr;
  OmScopeType scp(omPtr);
  omObjSType **clsH = scp.add( om::FetchOop(*ps->astClassesH, my_cls));
  omObjSType **resH = scp.add(om::NewObj(omPtr, clsH));
  omObjSType **tmpH = scp.add( om::LocatePomObj(omPtr, symId));
  UTL_ASSERT( om::isSymbol(*tmpH) );
  om::StoreOop(omPtr, resH, val_ofs, tmpH);
  *tmpH = quidO;
  om::StoreOop(omPtr, resH, quid_ofs, tmpH);
  om::StoreSmallInt_(omPtr, resH, src_offset_ofs, ps->tokenOffset());
  return *resH;
}

omObjSType* RpNameToken::s(AstSymbolEType e_sym, rb_parse_state *ps)
{
  return RpNameToken::s(e_sym, OOP_OF_SMALL_LONG_( ps->tokenOffset()), ps);
}

omObjSType* RubyParser::new_call_1(omObjSType **rcvH, AstSymbolEType op_sym, 
		omObjSType **argsH, omObjSType *srcOfs, rb_parse_state *ps) 
{
  omObjSType *selO = RpNameToken::s(op_sym, srcOfs, ps);
  return new_call_1(*rcvH, selO, *argsH, ps);
}


VarTable* VarTable::allocate(rb_parse_state *ps, int numElements) 
{
    UTL_ASSERT(numElements > 0);
    VarTable *v;
    ComStateType *cst = ps->cst;
    v = (VarTable*)ComHeapMalloc(cst, sizeof(*v)); 
    v->list = (QUID*)ComHeapMalloc(cst, sizeof(QUID) * numElements);
    v->initialize(numElements);
    return v;
}

LocalState* LocalState::allocate(rb_parse_state *ps)
{
    LocalState* s = (LocalState*)ComHeapMalloc(ps->cst, sizeof(LocalState));
    s->initialize(ps);
    return s;
} 

void bstring::balloc(bstring *s, int64 len, rb_parse_state *ps) 
{
    // allocate new or larger memory and reset string length
    if (len > s->memLen) {
      s->mem = ComHeapMalloc(ps->cst, len);
      s->memLen = len;
    }
    s->strLen = 0;
}

bstring* bstring::new_(rb_parse_state *ps) 
{
    bstring* s = (bstring*)ComHeapMalloc(ps->cst, sizeof(*s));
    s->initialize();
    return s;
}
void StartPositionList::allocate(rb_parse_state *ps, int newSize) 
{
     int64 numBytes = sizeof(list[0]) * newSize;
     StartPosition *newList = (StartPosition*)ComHeapMalloc(ps->cst, numBytes);
     if (allocatedSize > 0) {
       memcpy(newList, list, sizeof(list[0]) * size);
     }
     list = newList; 
     allocatedSize = newSize;
}

