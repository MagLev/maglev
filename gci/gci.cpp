#include "gci.hf"
#include <iostream>

extern "C" {
#include "ruby.h"

VALUE cGCI, cGCIError, cOOP;

void Init_gci();

OopType r2g(VALUE val)
{
    switch(rb_type(val))
    {
        case T_NIL:
            return OOP_NIL;
        case T_TRUE:
            return OOP_TRUE;
        case T_FALSE:
            return OOP_FALSE;
        case T_FIXNUM:
        case T_BIGNUM:
            return GciI64ToOop(NUM2LONG(val));
        case T_DATA:
            return (OopType) DATA_PTR(val);
        default:
            rb_raise(rb_eRuntimeError, "Could not convert to GCI value");
    }
    return OOP_NIL;
}

VALUE g2r(OopType oop)
{
    if(oop == OOP_NIL)
        return Qnil;
    if(oop == OOP_FALSE)
        return Qfalse;
    if(oop == OOP_TRUE)
        return Qtrue;
    if(GCI_OOP_IS_SMALL_INT(oop))
        return INT2FIX(GciOopToI32(oop));
        
    return Data_Wrap_Struct(cOOP, 0L, 0L, (void*)oop);
}

VALUE check_error()
{
	GciErrSType err;
    
    if(GciErr(&err))
    {
        rb_raise(cGCIError, err.message);
        return Qfalse;
    }
    else
        return Qtrue;
}

VALUE login(VALUE self, VALUE user, VALUE pass)
{
	GciLogin(StringValuePtr(user), StringValuePtr(pass));
	return check_error();
}   

VALUE execute(VALUE self, VALUE str)
{
    OopType result = GciExecuteStr(StringValuePtr(str), OOP_NIL);
    check_error();
    return g2r(result);
}

VALUE to_s(VALUE self)
{
    OopType oop, result;
    char buf[1000];
    long size;
    
    oop = (OopType) DATA_PTR(self);
    result = GciPerform(oop, "asString", 0L, 0);
    check_error();
    size = GciFetchChars_(result, 1, buf, 1000);
    check_error();
    return rb_str_new(buf, size);
}

VALUE perform(int argc, VALUE* argv, VALUE self)
{
    OopType oop, result;
    char* selector;
    OopType args[10];
    int i;
    
    oop = (OopType) DATA_PTR(self);
    if(argc == 0)
        return Qnil;

    selector = StringValuePtr(argv[0]);    
    for(i = 1; i < argc; i++)
        args[i-1] = r2g(argv[i]);
        
    result = GciPerform(oop, selector, args, argc-1);
    check_error();
    return g2r(result);
}

void Init_gci() {
	if(!GciInit())
	{
		std::cerr << "Could not initialize GemStone";
		return;
	}
	
	cGCI = rb_define_class("GCI", rb_cObject);
	cGCIError = rb_define_class("GCIError", rb_eStandardError);
	cOOP = rb_define_class("GCIObject", rb_cObject);
	rb_define_method(cGCI, "login", RUBY_METHOD_FUNC(login), 2);
	rb_define_method(cGCI, "execute", RUBY_METHOD_FUNC(execute), 1);
	rb_define_method(cOOP, "to_s", RUBY_METHOD_FUNC(to_s), 0);
	rb_define_method(cOOP, "perform", RUBY_METHOD_FUNC(perform), -1);
}

}