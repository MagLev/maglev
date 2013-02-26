/**********************************************************************

  ruby/ruby.h -

  $Author: yugui $
  created at: Thu Jun 10 14:26:32 JST 1993

  Copyright (C) 1993-2008 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

  Maglev:  copied from JRuby sources on  18 Oct 2010 
  svn file     src/rubycext.h 
  installed as include/ruby.h in the Maglev product tree

**********************************************************************/

#if !defined(Maglev_RUBYCEXT_H)
#define	Maglev_RUBYCEXT_H

#define MAGLEV

#include <sys/types.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>

// A number of extensions expect these to be already included
#include <stddef.h>
#include <stdlib.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>

//  include <st_sizes.h>
#define SIZEOF_LONG 8
#define SIZEOF_VOIDP 8
#define SIZEOF_OFF_T 8

// Some platform specific includes
#if defined(__WIN32__) || defined(__MINGW32__)
+++  Windows not supported by Maglev
#else
#define RUBY_DLLSPEC
#include <sys/select.h>
#include <pthread.h>
#endif

//  ifdef RUBY_EXTCONF_H
//  include RUBY_EXTCONF_H
//  endif

#ifdef	__cplusplus
extern "C" {
#endif

#ifndef  HAVE_PROTOTYPES
# define HAVE_PROTOTYPES 1
#endif
#ifndef  HAVE_STDARG_PROTOTYPES
# define HAVE_STDARG_PROTOTYPES 1
#endif

#ifndef NORETURN
#define NORETURN(x) __attribute__ ((noreturn)) x
#endif

#ifndef EXTERN
#define EXTERN extern
#endif

#undef _
#ifdef HAVE_PROTOTYPES
# define _(args) args
#else
+++  K&R C not supported by Maglev
# define _(args) ()
#endif

#undef __
#ifdef HAVE_STDARG_PROTOTYPES
# define __(args) args
#else
+++  K&R C not supported by Maglev
# define __(args) ()
#endif

#ifdef __cplusplus
# define ANYARGS ...
#else
# define ANYARGS
#endif

/** In MRI, ID represents an interned string, i.e. a Symbol. */
typedef uintptr_t ID;
/** In MRI, VALUE represents an object. */
typedef uintptr_t VALUE;
typedef intptr_t SIGNED_VALUE;

#if !defined(RSHIFT)
# define RSHIFT(x,y) ((x)>>(int)y)
#endif

/** Zero out N elements of type starting at given pointer. */
#define MEMZERO(p,type,n) memset((p), 0, (sizeof(type) * (n)))
/** Copies n objects of type from p2 to p1. Behavior is undefined if objects overlap. */
#define MEMCPY(p1,p2,type,n) memcpy((p1), (p2), sizeof(type)*(n))
/** Copies n objects of type from p2 to p1. Objects may overlap. */
#define MEMMOVE(p1,p2,type,n) memmove((p1), (p2), sizeof(type)*(n))
/** Compares n objects of type. */
#define MEMCMP(p1,p2,type,n) memcmp((p1), (p2), sizeof(type)*(n))

		// from gcioop.ht, oop.ht
#define Mag_OOP_TAG_MASK 0x7  /* OOP_RAM_TAG_MASK */
#define Mag_OOP_TAG_SMALLINT 0x2
#define Mag_OOP_NUM_TAG_BITS 3
#define Mag_OOP_TAG_SPECIAL_MASK 0x6
#define Mag_OOP_NIL             ((VALUE)0x14)
#define Mag_OOP_REMOTE_NIL     ((VALUE)0x114)
#define Mag_OOP_FALSE           ((VALUE)0x0C)
#define Mag_OOP_TRUE           ((VALUE)0x10C)

#define Mag_GCI_I32_TO_OOP(arg) (\
  (VALUE)((intptr_t)(arg) << Mag_OOP_NUM_TAG_BITS) | Mag_OOP_TAG_SMALLINT)

// FIXNUM_FLAG not available
// SYMBOL_FLAG not available

#define FIXNUM_P(f) ( ((f) & Mag_OOP_TAG_MASK) == Mag_OOP_TAG_SMALLINT )

#define FIXNUM_MAX (LONG_MAX>> Mag_OOP_NUM_TAG_BITS )
#define FIXNUM_MIN RSHIFT((long)LONG_MIN, Mag_OOP_NUM_TAG_BITS)

#define POSFIXABLE(f) ((f) < FIXNUM_MAX+1)
#define NEGFIXABLE(f) ((f) >= FIXNUM_MIN)
#define FIXABLE(f) (POSFIXABLE(f) && NEGFIXABLE(f))

// IMMEDIATE_MASK  not available

#define IMMEDIATE_P(x) ((VALUE)(x) & Mag_OOP_TAG_SPECIAL_MASK)
  // IMMEDIATE_P does not check for illegal special oops

/** The false object.  Maglev: IT IS NOT ZERO */
#define Qfalse Mag_OOP_FALSE  
/** The true object. */
#define Qtrue  Mag_OOP_TRUE
/** The nil object. */
#define Qnil   Mag_OOP_NIL
/** The undef object. Value for placeholder */  // value of an 'undefined' inst var
#define Qundef Mag_OOP_REMOTE_NIL

#define RTEST(v) RTEST_implem(v)

static inline int RTEST_implem(VALUE v) 
{
  return v != Mag_OOP_FALSE && v != Mag_OOP_NIL;
}

#define NIL_P(v) ((v) == Qnil)

#define SPECIAL_CONST_P(x) (IMMEDIATE_P(x) || ! RTEST(x))

#define SYMBOL_P(x) rb_is_symbol(x)

RUBY_DLLSPEC int rb_special_const__(VALUE v);

static inline int rb_special_const_p_(VALUE v) {
  if (v & Mag_OOP_TAG_SPECIAL_MASK) {
    return 1;
  }
  return rb_special_const__(v);
}

static inline VALUE rb_special_const_p(VALUE v) {
  return rb_special_const_p_(v) ? Qtrue : Qfalse ;
}

RUBY_DLLSPEC int rb_is_symbol(VALUE v);

// Maglev: use the objectId of the symbol directly as the ID
#define ID2SYM(x) (x)
#define SYM2ID(x) (x)

RUBY_DLLSPEC int rb_obj_frozen(VALUE v);

static inline int OBJ_FROZEN(VALUE x) { return rb_obj_frozen(x); }

// define OBJ_INFECT(o1, o2) 

// Maglev does not support tainting
#define OBJ_TAINT(obj) { /* do nothing */ }
#define OBJ_TAINTED(obj)  0 

typedef enum MagRubyType {
    T_NONE,
    T_NIL,
    T_OBJECT,
    T_CLASS,
    T__CLASS, 
    T_MODULE,
    T_FLOAT,
    T_STRING,
    T_REGEXP,
    T_ARRAY,
    T_FIXNUM,
    T_HASH,
    T_STRUCT,
    T_BIGNUM,
    T_FILE,

    T_TRUE,
    T_FALSE,
    T_DATA,   // wrapped C pointers 
    T_MATCH,  // a MatchData
    T_SYMBOL,

    T_BLKTAG,  // not used in maglev
    T_UNDEF,    // no object exists for specified objectId 
#ifndef MAGLEV_LINT
    T_VARMAP, // internal use: dynamic variables , not in Maglev
    T_SCOPE,  // internal use: variable scope , not in Maglev
    T_NODE    // internal use: syntax tree node
#endif

#ifdef MAGLEV_LINT
#define T_ICLASSnotUsed T__CLASS
#else
#define T_ICLASS T__CLASS
#endif

} MagRubyType;

// define T_MASK (0x1f)


RUBY_DLLSPEC int rb_type(VALUE v);

static inline int TYPE(VALUE x) { return rb_type(x); }

RUBY_DLLSPEC void rb_check_type(VALUE v, int expected_type);

static inline void Check_Type(VALUE v,int t) { rb_check_type(v ,t); }

#define xmalloc rb_malloc
#define xmalloc2 rb_malloc2
#define xcalloc  rb_calloc
#define xrealloc rb_realloc
#define xrealloc2 rb_realloc2
#define xfree   rb_free

// following like rubinius
#define ruby_xmalloc   xmalloc    
#define ruby_xcalloc   xcalloc
#define ruby_xrealloc  xrealloc
#define ruby_xfree     xfree

RUBY_DLLSPEC void *rb_malloc(size_t);
RUBY_DLLSPEC void *rb_malloc2(size_t,size_t);
RUBY_DLLSPEC void *rb_calloc(size_t,size_t);
RUBY_DLLSPEC void *rb_realloc(void*,size_t);
RUBY_DLLSPEC void *rb_realloc2(void*,size_t,size_t);
RUBY_DLLSPEC void rb_free(void*);

/* need to include <ctype.h> to use these macros */
#if !defined(ISPRINT)
#define ISASCII(c) isascii((int)(unsigned char)(c))
#undef ISPRINT
#define ISPRINT(c) (ISASCII(c) && isprint((int)(unsigned char)(c)))
#define ISSPACE(c) (ISASCII(c) && isspace((int)(unsigned char)(c)))
#define ISUPPER(c) (ISASCII(c) && isupper((int)(unsigned char)(c)))
#define ISLOWER(c) (ISASCII(c) && islower((int)(unsigned char)(c)))
#define ISALNUM(c) (ISASCII(c) && isalnum((int)(unsigned char)(c)))
#define ISALPHA(c) (ISASCII(c) && isalpha((int)(unsigned char)(c)))
#define ISDIGIT(c) (ISASCII(c) && isdigit((int)(unsigned char)(c)))
#define ISXDIGIT(c) (ISASCII(c) && isxdigit((int)(unsigned char)(c)))
#endif

/* Interface macros */

/** Allocate memory for type. Must NOT be used to allocate Ruby objects. */
#define ALLOC(type) (type*)xmalloc(sizeof(type))

/** Allocate memory for N of type. Must NOT be used to allocate Ruby objects. */
#define ALLOC_N(type,n) (type*)xmalloc(sizeof(type)*(n))

/** Reallocate memory allocated with ALLOC or ALLOC_N. */
#define REALLOC_N(var,type,n) (var)=(type*)xrealloc((char*)(var),sizeof(type)*(n))

/** Interrupt checking (no-op). */
#define CHECK_INTS        /* No-op */

/** Test macros */
//  RTEST defined above

// RARRAY_PTR, RARRAY not supported
// RFLOAT not supported

// DATA_PTR, RDATA not supported
#define SetRDataData(obj, ptr) \
    rb_ivar_set(obj, rb_intern("RDataData"), Data_Wrap_Struct(rb_cObject, NULL, NULL, ptr))
#define GetRDataData(obj) \
    rb_rdata_fetch(rb_ivar_get(obj, rb_intern("RDataData")))

/* End of interface macros */

/**
 *  Process arguments using a template rather than manually.
 *
 *  The first two arguments are simple: the number of arguments given
 *  and an array of the args. Usually you get these as parameters to
 *  your function.
 *
 *  The spec works like this: it must have one (or more) of the following
 *  specifiers, and the specifiers that are given must always appear
 *  in the order given here. If the first character is a digit (0-9),
 *  it is the number of required parameters. If there is a second digit
 *  (0-9), it is the number of optional parameters. The next character
 *  may be "*", indicating a "splat" i.e. it consumes all remaining
 *  parameters. Finally, the last character may be "&", signifying
 *  that the block given (or Qnil) should be stored.
 *
 *  The remaining arguments are pointers to the variables in which
 *  the aforementioned format assigns the scanned parameters. For
 *  example in some imaginary function:
 *
 *    VALUE required1, required2, optional, splat, block
 *    rb_scan_args(argc, argv, "21*&", &required1, &required2,
 *                                     &optional,
 *                                     &splat,
 *                                     &block);
 *
 *  The required parameters must naturally always be exact. The
 *  optional parameters are set to nil when parameters run out.
 *  The splat is always an Array, but may be empty if there were
 *  no parameters that were not consumed by required or optional.
 *  Lastly, the block may be nil.
 */
RUBY_DLLSPEC int rb_scan_args(int argc, const VALUE* argv, const char* spec, ...); 

/** Returns true on first load, false if already loaded or raises. */
RUBY_DLLSPEC VALUE rb_require(const char* name);

RUBY_DLLSPEC void rb_raise(VALUE exc, const char *fmt, ...) ;
RUBY_DLLSPEC void rb_raise_(VALUE exc, const char *message);

RUBY_DLLSPEC VALUE rb_rescue(VALUE(*)(ANYARGS), VALUE,VALUE(*)(ANYARGS), VALUE);
RUBY_DLLSPEC VALUE rb_rescue2(VALUE(*)(ANYARGS),VALUE,VALUE(*)(ANYARGS),VALUE,...);
//  In ruby code invoked from third arg to rb_rescue, rb_rescue2
//   $! does not reflect the exception being rescued by the C function.
//   that is the 3rd arg.  Use the second argument to the
//   rescue function to obtain the exception being rescued.

RUBY_DLLSPEC VALUE rb_ensure(VALUE(*)(ANYARGS),VALUE,VALUE(*)(ANYARGS),VALUE);
RUBY_DLLSPEC VALUE rb_protect(VALUE (*func)(VALUE), VALUE data, int* status);
// RUBY_DLLSPEC void rb_jump_tag(int status);
// RUBY_DLLSPEC void rb_throw(const char* symbol, VALUE result);

// RUBY_DLLSPEC void rb_fatal(const char *fmt, ...) __attribute__((noreturn));
RUBY_DLLSPEC void rb_sys_fail(const char *msg);
// RUBY_DLLSPEC void rb_bug(const char*, ...) __attribute__((noreturn));

RUBY_DLLSPEC VALUE rb_exc_raise(VALUE an_exception);

RUBY_DLLSPEC VALUE rb_exc_new(VALUE klass, const char* str, long str_length);

static VALUE rb_exc_new2(VALUE klass, const char* s)
{
  return  rb_exc_new(klass, s, strlen(s));
}

RUBY_DLLSPEC VALUE rb_exc_new3(VALUE klass, VALUE str);

static void rb_secure(int k) { }
static void rb_check_safe_obj(VALUE obj) { }
static void rb_check_safe_str(VALUE str) { }
static int rb_safe_level(void) { return 0; }

RUBY_DLLSPEC int rb_num2int(VALUE v);
RUBY_DLLSPEC unsigned int rb_num2uint(VALUE v);

RUBY_DLLSPEC long rb_num2long(VALUE v);
RUBY_DLLSPEC unsigned long rb_num2ulong(VALUE v);

RUBY_DLLSPEC char rb_num2chr(VALUE v);
RUBY_DLLSPEC double rb_num2dbl(VALUE v);

/** Convert a Fixnum into an int. */
RUBY_DLLSPEC int FIX2INT(VALUE x);
  // throws an Exception if argument it not a Fixnum within range of a C int

/** Convert a Fixnum into an unsigned int. */
RUBY_DLLSPEC unsigned int FIX2UINT(VALUE x);
  // throws an Exception if argument it not a Fixnum within range of a C uint

RUBY_DLLSPEC long FIX2LONG(VALUE x);

RUBY_DLLSPEC unsigned long FIX2ULONG(VALUE x);

static inline int rb_fix2int(VALUE v) { return FIX2INT(v); }

static inline unsigned int rb_fix2uint(VALUE v) { return FIX2UINT(v); }

static inline long rb_num2ll(VALUE v)
{
  return rb_num2long(v);
}
static inline long rb_num2ull(VALUE v)
{
  return rb_num2ulong(v);
}

RUBY_DLLSPEC double rb_num2dbl(VALUE v);
RUBY_DLLSPEC long rb_big2long(VALUE v);

RUBY_DLLSPEC int rb_big2int(VALUE x);  // usually throws RangeError

RUBY_DLLSPEC unsigned long rb_big2ulong(VALUE);

RUBY_DLLSPEC unsigned int rb_big2uint(VALUE x); // usually throws RangeError

static inline long rb_big2ll(VALUE v)
{
  return rb_big2long(v);
}

RUBY_DLLSPEC double rb_big2dbl(VALUE v);
RUBY_DLLSPEC VALUE rb_big2str(VALUE v, int radix);


RUBY_DLLSPEC VALUE rb_int2inum(long v);
RUBY_DLLSPEC VALUE rb_uint2inum(unsigned long v);

enum { SIZEOF_BDIGITS = 4 }; // 32bits per digit

RUBY_DLLSPEC int rb_bignum_len(VALUE v); 
  // returns the number of digits in the internal implementation

#define RBIGNUM_LEN rb_bignum_len

static inline VALUE rb_ll2inum(long n) { return rb_int2inum(n); }
static inline VALUE rb_ull2inum(unsigned long n) { return rb_uint2inum(n); }

static inline VALUE rb_int2big(long n) { return rb_int2inum(n); }
static inline VALUE rb_uint2big(unsigned long n) { return rb_uint2inum(n); }

RUBY_DLLSPEC VALUE rb_Integer(VALUE v); 
  // calls to_i  on v

/** Converts an object to an Integer by calling #to_int. */
RUBY_DLLSPEC VALUE rb_to_int(VALUE v);

/** Converts an object to an Integer using the specified method */
RUBY_DLLSPEC VALUE rb_to_integer(VALUE v, const char* method_name);

/** Convert a VALUE into an int. */
static inline int NUM2INT(VALUE x)
{
    return FIXNUM_P(x) ? FIX2INT(x) : rb_num2int(x);
}

static inline unsigned int NUM2UINT(VALUE x)
{
    return FIXNUM_P(x) ? FIX2UINT(x) : rb_num2uint(x);
}

/** Convert a VALUE into a long int. */
static inline long NUM2LONG(VALUE x)
{
    return FIXNUM_P(x) ? FIX2LONG(x) : rb_num2long(x);
}


static unsigned long NUM2ULONG(VALUE x)
{
    return FIXNUM_P(x) ? FIX2ULONG(x) : rb_num2ulong(x);
}

static inline long NUM2LL(VALUE x)
{
  return NUM2LONG(x);
}

static inline unsigned long NUM2ULL(VALUE x)
{
  return NUM2ULONG(x);
}

/** Convert int to a Ruby Integer. */
static inline VALUE INT2FIX(int i) { return Mag_GCI_I32_TO_OOP(i); }

/** Convert unsigned int to a Ruby Integer. */
static inline VALUE UINT2FIX(unsigned int i) { return Mag_GCI_I32_TO_OOP(i); }

/** Convert long int to a Ruby Integer. */
static inline VALUE LONG2FIX(long i) { return rb_int2inum(i); }

/** Convert unsigned int to a Ruby Integer. */
static inline VALUE ULONG2FIX(unsigned long i) { return rb_uint2inum(i); }

/** Convert int to a Ruby Integer. */

static inline VALUE INT2NUM(long v) { return rb_int2inum(v); }

static inline VALUE UINT2NUM(unsigned long v) { return rb_uint2inum(v); }

static inline VALUE LONG2NUM(long v) { return rb_int2inum(v); }

static inline VALUE ULONG2NUM(unsigned long v) { return rb_uint2inum(v); }


static inline VALUE LL2NUM(long v) { return rb_int2inum(v); }

static inline VALUE ULL2NUM(unsigned long v) { return rb_uint2inum(v); }

/** Convert a VALUE into a char */
static inline char NUM2CHR(VALUE x) { return rb_num2chr(x); }

/** Convert a VALUE into a double */
static inline double NUM2DBL(VALUE x) { return rb_num2dbl(x); }

static inline VALUE rb_int_new(long v) { return rb_int2inum(v); }

RUBY_DLLSPEC VALUE rb_funcall(VALUE obj, ID meth, int cnt, ...);
RUBY_DLLSPEC VALUE rb_funcall2(VALUE obj, ID meth, int cnt, VALUE* args);
RUBY_DLLSPEC VALUE rb_funcall2_(VALUE obj, ID meth, int cnt, VALUE args_array);

// rb_funcall3 same as rb_funcall2 but not supposed to call private methods
static inline VALUE rb_funcall3(VALUE obj, ID meth, int cnt, VALUE* args) {
  return rb_funcall2(obj, meth, cnt, args);
}

/** Starts the lookup in the superclass to call a method on the current self */
// RUBY_DLLSPEC VALUE rb_call_super(int argc, const VALUE *argv);

/** Returns a new, anonymous class inheriting from super_klass.,
    without calling Class#inherited */
RUBY_DLLSPEC VALUE rb_class_new(VALUE super_klass);

/** Calls the class method 'inherited' on super passing the class. */
RUBY_DLLSPEC VALUE rb_class_inherited(VALUE super, VALUE klass);

/** As Ruby's .new, with the given arguments. Returns the new object. */
RUBY_DLLSPEC VALUE rb_class_new_instance(int arg_count, VALUE* args, VALUE klass);

/** Returns the Class object this object is an instance of. */
RUBY_DLLSPEC VALUE rb_class_of(VALUE obj);

static inline VALUE CLASS_OF(VALUE x) { return rb_class_of(x); }   

/** Returns String representation of the class' name. */
RUBY_DLLSPEC VALUE rb_class_name(VALUE klass);

/** C string representation of the class' name. You must free this string. */
RUBY_DLLSPEC char* rb_class2name(VALUE klass);

/** Convert a path string to a class */
RUBY_DLLSPEC VALUE rb_path2class(const char* path);

/** Include Module in another Module, just as Ruby's Module#include. */
RUBY_DLLSPEC void rb_include_module(VALUE self, VALUE module);

/** Return the object's singleton class */
RUBY_DLLSPEC VALUE rb_singleton_class(VALUE obj);

RUBY_DLLSPEC VALUE rb_define_class(const char* name, VALUE parent);
RUBY_DLLSPEC VALUE rb_define_module(const char* name);
RUBY_DLLSPEC VALUE rb_define_class_under(VALUE parent, const char* name, VALUE super_class);
RUBY_DLLSPEC VALUE rb_define_module_under(VALUE parent, const char* name);

/** Ruby's attr_* for given name. Nonzeros to toggle read/write. */
RUBY_DLLSPEC void rb_define_attr(VALUE module, const char* attr_name, int readable, int writable);
#define rb_attr(mod, attr_name, read, write, unused) \
    rb_define_attr(mod, rb_id2name(attr_name), read, write);

typedef VALUE (*anyArgsFct_t)(ANYARGS);

//  define ruby methods which are calls to specified C functions .
RUBY_DLLSPEC void rb_define_method(VALUE klass, const char* rubySelector,
				anyArgsFct_t cFunction, int nArgs);
RUBY_DLLSPEC void rb_define_private_method(VALUE,const char*,VALUE(*)(ANYARGS),int);
RUBY_DLLSPEC void rb_define_protected_method(VALUE,const char*,VALUE(*)(ANYARGS),int);
RUBY_DLLSPEC void rb_define_module_function(VALUE,const char*,VALUE(*)(ANYARGS),int);
RUBY_DLLSPEC void rb_define_global_function(const char*,VALUE(*)(ANYARGS),int);
RUBY_DLLSPEC void rb_define_singleton_method(VALUE object, const char* meth, VALUE(*fn)(ANYARGS), int arity);

#define HAVE_RB_DEFINE_ALLOC_FUNC 1
typedef VALUE (*rb_alloc_func_t)(VALUE klass);
RUBY_DLLSPEC void rb_define_alloc_func(VALUE klass, rb_alloc_func_t f);
  // install a C allocator function that will be called when the
  // method  klass.allocate  is called.

RUBY_DLLSPEC void rb_undef_method(VALUE, const char*);
RUBY_DLLSPEC void rb_undef(VALUE, ID);

RUBY_DLLSPEC VALUE rb_define_class_variable(VALUE klass, const char* name, VALUE val); 
RUBY_DLLSPEC VALUE rb_cv_get(VALUE klass, const char* name);
RUBY_DLLSPEC VALUE rb_cv_set(VALUE klass, const char* name, VALUE value);

/** Returns a value evaluating true if module has named class var. */
RUBY_DLLSPEC VALUE rb_cvar_defined(VALUE module_handle, ID name);

/** Returns class variable by (Symbol) name from module. */
RUBY_DLLSPEC VALUE rb_cvar_get(VALUE module_handle, ID name);

/** Set module's named class variable to given value. Returns the value. */
RUBY_DLLSPEC VALUE rb_cvar_set_(VALUE modul, ID name, VALUE value);

static inline RUBY_DLLSPEC VALUE rb_cvar_set(VALUE module_handle, ID name, VALUE value, ...)
{
  // added var args per Trac 851
  // for ruby 1.9, remove the var args
  return rb_cvar_set_(module_handle, name, value);
}

/** Return object's instance variable by name. @ optional. */
RUBY_DLLSPEC VALUE rb_iv_get(VALUE obj, const char* name);

/** Set instance variable by name to given value. Returns the value. @ optional. */
RUBY_DLLSPEC VALUE rb_iv_set(VALUE obj, const char* name, VALUE value);

/** Get object's instance variable. */
RUBY_DLLSPEC VALUE rb_ivar_get(VALUE obj, ID ivar_name);

/** Set object's instance variable to given value. */
RUBY_DLLSPEC VALUE rb_ivar_set(VALUE obj, ID ivar_name, VALUE value);

RUBY_DLLSPEC int rb_ivar_defined_(VALUE obj, ID ivar_name);

/*  common usage patterns are to use rb_ivar_defined without RTEST,
 *  when MAGLEV_LINT is defined, we get compile errors to help find
 *  the bad uses */
#if !defined(MAGLEV_LINT)
static inline VALUE rb_ivar_defined(VALUE obj, ID ivar_name) 
{
  return rb_ivar_defined_(obj, ivar_name) ? Qtrue : Qfalse;
}
#endif

/** Nonzero if constant corresponding to Symbol exists in the Module. */
RUBY_DLLSPEC int rb_const_defined(VALUE mod, ID sym);

/** Returns non-zero if the constant is defined in the Module, not searching outside */
RUBY_DLLSPEC int rb_const_defined_at(VALUE mod, ID sym);

/** Retrieve constant from given module. */
RUBY_DLLSPEC VALUE rb_const_get(VALUE mod, ID sym);


/** Returns a constant defined in module only. */
RUBY_DLLSPEC VALUE rb_const_get_at(VALUE mod, ID sym);
RUBY_DLLSPEC VALUE rb_const_get_at_str(VALUE mod, const char* name);

/** Retrieve constant from given module. */
RUBY_DLLSPEC VALUE rb_const_get_from(VALUE mod, ID sym);

/** Set constant on the given module */
RUBY_DLLSPEC void rb_const_set(VALUE, ID, VALUE);

/** Alias method by old name as new name. */
RUBY_DLLSPEC void rb_define_alias(VALUE klass, const char *new_name, const char *old_name);

/* Array */
RUBY_DLLSPEC VALUE rb_Array(VALUE val);  //   Array.new(val)
RUBY_DLLSPEC VALUE rb_ary_new2(long length);
static inline VALUE rb_ary_new(void) { return rb_ary_new2(0); }

RUBY_DLLSPEC VALUE rb_ary_new3(long size, ...);
RUBY_DLLSPEC VALUE rb_ary_new4(long n, const VALUE *values);
RUBY_DLLSPEC VALUE rb_assoc_new(VALUE a, VALUE b);
RUBY_DLLSPEC long rb_ary_size(VALUE self);

#define RARRAY_LEN(ary) rb_ary_size(ary)

RUBY_DLLSPEC VALUE rb_ary_push(VALUE array, VALUE val);
RUBY_DLLSPEC VALUE rb_ary_pop(VALUE array);
RUBY_DLLSPEC VALUE rb_ary_entry(VALUE array, long offset);  // use instead of RARRAY_PTR
RUBY_DLLSPEC VALUE rb_ary_clear(VALUE array);
RUBY_DLLSPEC VALUE rb_ary_dup(VALUE array);
RUBY_DLLSPEC VALUE rb_ary_join(VALUE array1, VALUE separator);
RUBY_DLLSPEC VALUE rb_ary_reverse(VALUE array);   // NOTE  invokes Array#reverse!  
RUBY_DLLSPEC VALUE rb_ary_unshift(VALUE array, VALUE val);
RUBY_DLLSPEC VALUE rb_ary_shift(VALUE array);
RUBY_DLLSPEC void rb_ary_store(VALUE array, long offset, VALUE val);
RUBY_DLLSPEC VALUE rb_ary_includes(VALUE array, VALUE val);
RUBY_DLLSPEC VALUE rb_ary_delete(VALUE array, VALUE val);
RUBY_DLLSPEC VALUE rb_ary_delete_at(VALUE array, long offset);
RUBY_DLLSPEC VALUE rb_ary_aref(int argc, VALUE* args, VALUE array);

RUBY_DLLSPEC VALUE rb_each(VALUE);  // may not be called, may only be used as arg to rb_iterate
RUBY_DLLSPEC VALUE rb_iterate(VALUE (*ifunc)(VALUE), VALUE ary, VALUE(*cb)(ANYARGS), VALUE cb_data);

/* Hash */
RUBY_DLLSPEC VALUE rb_hash_new(void);
RUBY_DLLSPEC VALUE rb_hash_aref(VALUE hash, VALUE key);

#if !defined(HAVE_RB_HASH_ASET)
#define HAVE_RB_HASH_ASET 1
#endif
RUBY_DLLSPEC VALUE rb_hash_aset(VALUE hash, VALUE key, VALUE val);

RUBY_DLLSPEC VALUE rb_hash_delete(VALUE hash, VALUE key);
RUBY_DLLSPEC VALUE rb_hash_size(VALUE hash);


#define RHASH_SIZE(h) FIX2INT(rb_hash_size(h))
#define RHASH_LEN(h) FIX2INT(rb_hash_size(h))

// define RHASH ({ Maglev does not support RHASH })
// define RHASH_TBL ({ Maglev does not support RHASH_TBL })

#ifndef HAVE_RB_HASH_FOREACH
#define HAVE_RB_HASH_FOREACH 1
#endif
RUBY_DLLSPEC void rb_hash_foreach(VALUE hash, 
			int (*func)(VALUE key, VALUE val, VALUE arg), VALUE arg);

RUBY_DLLSPEC VALUE rb_hash_lookup(VALUE hash, VALUE key);

/* String */

/**
 * Returns a pointer to the String, the length is returned
 * in len parameter, which can be NULL.
 */
RUBY_DLLSPEC const char* rb_str2cstr(VALUE str, long *len);
   // C memory is auto-freed by GC

/** The pointer to the string str's data. */
#define RSTRING_PTR(str) (char*)rb_str2cstr(str, NULL)
static inline const char* rb_str_ptr(VALUE str) { return rb_str2cstr(str, NULL); }
static inline const char* rb_str_ptr_readonly(VALUE str) { return rb_str2cstr(str, NULL); }

/** Pointer to the MRI string structure */  
// define RSTRING(str) // not supported

static inline const char* STR2CSTR(VALUE str) { return rb_str2cstr(str, NULL); }

/** Return a String using #to_str. Error raised if invalid conversion. */
RUBY_DLLSPEC VALUE rb_str_to_str(VALUE v);

/** Return a String using #to_s. Error raised if invalid conversion. */
RUBY_DLLSPEC VALUE rb_String(VALUE v);

/** Modifies the VALUE object in place by calling rb_obj_as_string(). */
#define StringValue(v)        rb_string_value(&(v))
#define StringValuePtr(v)     rb_string_value_ptr(&(v))
#define StringValueCStr(str)  rb_string_value_cstr(&(str))

/** Call #to_s on object pointed to and _replace_ it with the String. */
RUBY_DLLSPEC VALUE rb_string_value(VALUE* object_variable);

/** As rb_string_value but also returns a C string of the new String. */
RUBY_DLLSPEC const char* rb_string_value_ptr(VALUE* object_variable);
   // C memory is auto-freed by GC

/** As rb_string_value but also returns a C string of the new String. */
RUBY_DLLSPEC const char* rb_string_value_cstr(VALUE* object_variable);   
   // C memory is auto-freed by GC
   // raises exception if string data contains NUL byte

RUBY_DLLSPEC void rb_store_string_l(VALUE str, long offset, const char* value, size_t len);
   // Modifies bytes of str in the VM's object memory.
   // C memory state from a previous StringValue not changed. The next
   // StringValue call on obj will return a pointer to a refreshed or 
   // newly allocated C memory copy of the bytes of obj .

static void rb_store_string(VALUE str, long offset, const char* value)
{
  rb_store_string_l(str, offset, value, strlen(value));
}

RUBY_DLLSPEC VALUE rb_str_new(const char* str, long len);
RUBY_DLLSPEC VALUE rb_str_new2(const char* str);

static inline VALUE rb_str_new_cstr(const char* str) { return rb_str_new2(str); }

// no actual taint support in Maglev
static inline RUBY_DLLSPEC VALUE rb_tainted_str_new_cstr(const char* str)
{
  return rb_str_new2(str);
}

static inline RUBY_DLLSPEC VALUE rb_tainted_str_new(const char* str, long len)
{
  return rb_str_new(str, len);
}

static inline RUBY_DLLSPEC VALUE rb_tainted_str_new2(const char* str)
{
  return rb_str_new2(str);
}

static inline VALUE rb_str_buf_new(long len )
{
  // creates a new String of size 0 . The argument is ignored.
  // Maglev does not support separate logical and physical sizes 
  // of String objects
  return rb_str_new("", 0);
}

static inline VALUE rb_str_buf_new2(const char* string)
{
  // Return a new String containing the given C string
  return rb_str_new2(string);
}

// following 3 append bytes to the str object which must be a kind of String
RUBY_DLLSPEC VALUE rb_str_buf_append(VALUE str, VALUE val);
RUBY_DLLSPEC VALUE rb_str_buf_cat(VALUE str, const char* bytes, long len);
RUBY_DLLSPEC VALUE rb_str_buf_cat2(VALUE str, const char* cstring);

RUBY_DLLSPEC VALUE rb_obj_as_string(VALUE obj);

RUBY_DLLSPEC VALUE rb_check_string_type(VALUE obj);

RUBY_DLLSPEC VALUE rb_str_dup(VALUE str);
RUBY_DLLSPEC VALUE rb_str_dup_frozen(VALUE str);

static inline VALUE rb_str_new_frozen(VALUE v) { return rb_str_dup_frozen(v); }

static inline VALUE rb_str_new3(VALUE v) { return rb_str_dup(v); }

static inline VALUE rb_str_new4(VALUE v) { return rb_str_new_frozen(v); }

RUBY_DLLSPEC VALUE rb_str_plus(VALUE str, VALUE arg);

RUBY_DLLSPEC VALUE rb_str_length(VALUE str);
RUBY_DLLSPEC VALUE rb_str_substr(VALUE str, long start, long len);
RUBY_DLLSPEC VALUE rb_str_freeze(VALUE str);
RUBY_DLLSPEC VALUE rb_str_resize(VALUE str, long len);
#define HAVE_RB_STR_SET_LEN
static inline void rb_str_set_len(VALUE str, long len) { rb_str_resize(str, len); }
RUBY_DLLSPEC VALUE rb_str_cat(VALUE str, const char* bytes, long len);
RUBY_DLLSPEC VALUE rb_str_cat2(VALUE str, const char* cstring);

RUBY_DLLSPEC VALUE rb_str_append(VALUE str, VALUE arg);
static inline VALUE rb_str_concat(VALUE str, VALUE arg) { return rb_str_append(str, arg); }
RUBY_DLLSPEC int rb_str_cmp(VALUE str, VALUE arg);
RUBY_DLLSPEC void rb_str_update(VALUE str, long beg, long len, VALUE arg);
RUBY_DLLSPEC VALUE rb_str_inspect(VALUE str);
RUBY_DLLSPEC VALUE rb_str_split(VALUE str, const char* separator);
RUBY_DLLSPEC VALUE rb_str_intern(VALUE str);

/** The length of string str. */
RUBY_DLLSPEC long rb_str_len(VALUE str);

#define RSTRING_LEN(str) rb_str_len(str)
#define RSTRING_LENINT(str) rb_long2int(rb_str_len(str))
#define rb_long2int(x) ((int)x)

// /** Deprecated alias for rb_obj_freeze */
// RUBY_DLLSPEC VALUE rb_str_freeze(VALUE str);

/** Return Integer obtained from String#to_i using given base. */
RUBY_DLLSPEC VALUE rb_str2inum(VALUE str, int base);

RUBY_DLLSPEC VALUE rb_cstr2inum(const char* str, int base) ;

#define rb_cstr_to_dbl(x, badcheck) atof(x)

//  define rb_cstr_to_inum(VALUE str, int base, badcheck) rb_cstr2inum(str, base)


/** Returns the string associated with a symbol. */
RUBY_DLLSPEC const char *rb_id2name(ID sym);
  //   pins the Symbol in memory and returns CData pointer as for StringValue
 
/** Call #to_sym on object. */
RUBY_DLLSPEC ID rb_to_id(VALUE obj);

/** Returns a Struct with the specified fields. */
RUBY_DLLSPEC VALUE rb_struct_define(const char *name, ...);

typedef void (*RUBY_DATA_FUNC)(void*);

RUBY_DLLSPEC VALUE rb_data_object_alloc(VALUE klass, void* data, 
		RUBY_DATA_FUNC dmark , RUBY_DATA_FUNC dfree);

// an instance of any Ruby class may be created 
//  and have C data attached to it by Data_Wrap

#ifdef MAGLEV_LINT
#define Data_Wrap_Struct(klass,mark,free,sval)				\
    rb_data_object_alloc(klass,sval,(RUBY_DATA_FUNC)mark,(RUBY_DATA_FUNC)free)
#else
#define Data_Wrap_Struct(klass,mark,free,sval)				\
    rb_data_object_alloc(klass,sval,(RUBY_DATA_FUNC)NULL,(RUBY_DATA_FUNC)free)
#endif

#define Data_Make_Struct(klass,type,mark,free,sval) (\
    sval = ALLOC(type),\
    memset(sval, 0, sizeof(type)),\
    Data_Wrap_Struct(klass,mark,free,sval)\
)

RUBY_DLLSPEC void* rb_rdata_fetch(VALUE obj);

/** if obj already contains a C data pointer installed by rb_rdata_store or
    by Data_Wrap_Struct, and the object has a non-nil free function
   installed by Data_Wrap_Struct, the free function will be called with 
   previous data .  Then stores p as the new C data pointer for obj  */
RUBY_DLLSPEC void  rb_rdata_store(VALUE obj, void *p);

#define Data_Get_Struct(obj, type, sval)  sval = (type*)rb_rdata_fetch(obj)

/** Return the global variable. $ optional */
RUBY_DLLSPEC VALUE rb_gv_get(const char* name);

/** Set named global to given value, returning the value. $ optional. */
RUBY_DLLSPEC VALUE rb_gv_set(const char* name, VALUE value);

#ifndef MAGLEV_LINT
/** Mark variable global */
#define rb_global_variable rb_gc_register_address
static void rb_gc_register_address(VALUE* address) {
    char to_s[17] = {'\0'};
    sprintf(to_s, "%p", address);
    rb_gv_set((const char*)to_s, *address);
}

/** Unmark variable as global */
static void rb_gc_unregister_address(VALUE* address) {
    char to_s[17] = {'\0'};
    sprintf(to_s, "%p", address);
    rb_gv_set((const char*)to_s, Qnil);
}

/** No-op, gc access cannot be provided */
#define rb_gc_mark(v)
#define rb_gc_mark_maybe(v)
// RUBY_DLLSPEC void rb_gc_mark_locations(VALUE* a, VALUE* b);
#endif

static inline VALUE rb_errinfo()
{
  return rb_gv_get("$!");
}

static inline VALUE rb_set_errinfo(VALUE err)
{
  rb_gv_set("$!", err);
  return err;
}

// RUBY_DLLSPEC void rb_define_readonly_variable(const char* name, VALUE *value); // not implemented yet
//  a Ruby Global variable that is read-only from Ruby code 
 
/** Sets the $KCODE global variable */
// RUBY_DLLSPEC void rb_set_kcode(const char *code); // not supported 

/** Return an array containing the names of all global variables */
RUBY_DLLSPEC VALUE rb_f_global_variables(void);

RUBY_DLLSPEC VALUE rb_eval_string_(const char* string, const char* file, int line);

#define rb_eval_string(cstring)  rb_eval_string_(cstring, __FILE__, __LINE__)

RUBY_DLLSPEC VALUE rb_obj_instance_eval(int argc, VALUE* argv, VALUE self);

/** Print a warning if $VERBOSE is not nil. */
RUBY_DLLSPEC void rb_warn(const char *fmt, ...);

/** Print a warning if $VERBOSE is true. */
RUBY_DLLSPEC void rb_warning(const char *fmt, ...);

/** 1 if obj.respond_to? method_name evaluates true, 0 otherwise. */
RUBY_DLLSPEC int rb_respond_to(VALUE obj, ID method_name);

/** Returns object returned by invoking method on object if right type, or raises error. */
RUBY_DLLSPEC VALUE rb_convert_type(VALUE object_handle, int type, const char* type_name, const char* method_name);

/** Returns object returned by invoking method on object or nil */
RUBY_DLLSPEC VALUE rb_check_convert_type(VALUE val, int type, const char* type_name, const char* method);

RUBY_DLLSPEC VALUE rb_check_to_integer(VALUE obj, const char* method);

RUBY_DLLSPEC VALUE rb_check_array_type(VALUE val);
RUBY_DLLSPEC VALUE rb_check_string_type(VALUE val);

/** Define a constant in given Module's namespace. */
RUBY_DLLSPEC void rb_define_const(VALUE module, const char* name, VALUE obj);

/** Define a toplevel constant */
RUBY_DLLSPEC void rb_define_global_const(const char* name, VALUE obj);

RUBY_DLLSPEC ID rb_intern2(const char* str, long len);

static ID rb_intern(const char* v) { 
   return rb_intern2(v, v != NULL ? strlen(v) : 0); 
}

static ID rb_intern_const(const char*v ) { 
  return rb_intern2(v, v != NULL ? strlen(v) : 0);
}

RUBY_DLLSPEC int rb_is_class_id(ID symbol);
RUBY_DLLSPEC int rb_is_instance_id(ID symbol);
RUBY_DLLSPEC int rb_is_const_id(ID symbol);

RUBY_DLLSPEC VALUE rb_float_new(double value);

/** v must be a Float */
RUBY_DLLSPEC double rb_float_value(VALUE v);

// RFLOAT not supported

#define RFLOAT_VALUE(v) rb_float_value(v)

/** if v is a Float return v, else return v.to_f  */
RUBY_DLLSPEC VALUE rb_Float(VALUE v);

   // following are for the block argument to the C extension invocation 
/** Return 1 if block given, 0 if not */
RUBY_DLLSPEC int rb_block_given_p();		

/** Return the Proc for the implicit block */
RUBY_DLLSPEC VALUE rb_block_proc();  

/** Call block with given argument(s) or raise error if no block given. */
RUBY_DLLSPEC VALUE rb_yield(VALUE argument);
RUBY_DLLSPEC VALUE rb_yield_splat(VALUE array);
RUBY_DLLSPEC VALUE rb_yield_values(int n, ...);  

/** Create a proc with func as body and val as proc argument ({|*args, proc_arg| func }) */
// RUBY_DLLSPEC VALUE rb_proc_new(VALUE (*func)(ANYARGS), VALUE val);   // not implemented yet 

/** Freeze object and return it. */
RUBY_DLLSPEC VALUE rb_obj_freeze(VALUE obj);

static inline VALUE OBJ_FREEZE(VALUE obj) { return rb_obj_freeze(obj); }

/** Raise an error if the object is frozen */
RUBY_DLLSPEC void rb_check_frozen(VALUE obj);

/** Allocate uninitialised instance of given class. */
RUBY_DLLSPEC VALUE rb_obj_alloc(VALUE klass);   

/** Call initialize */
RUBY_DLLSPEC void rb_obj_call_init(VALUE recv, int arg_count, VALUE* args);

/** String representation of the object's class' name , no need to free the string 
    same result semantics as  result of rb_id2name */
RUBY_DLLSPEC const char* rb_obj_classname_(VALUE object);

/** Returns 1 if module is object's class or other ancestor, 0 otherwise. */
RUBY_DLLSPEC int rb_obj_is_kind_of_(VALUE obj, VALUE module);

#if !defined(MAGLEV_LINT)
static inline VALUE rb_obj_is_kind_of(VALUE obj, VALUE module)
{
  return rb_obj_is_kind_of_(obj, module) ? Qtrue : Qfalse;
}
#endif

/** Returns 1 if module is object's class , 0 otherwise. */
RUBY_DLLSPEC int rb_obj_is_instance_of_(VALUE obj, VALUE module);

#if !defined(MAGLEV_LINT)
static inline VALUE rb_obj_is_instance_of(VALUE obj, VALUE module)
{
  return rb_obj_is_instance_of_(obj, module) ? Qtrue : Qfalse ;
}
#endif

/** Returns the Class object this object is an instance of. */
static inline VALUE rb_obj_class(VALUE object) { return rb_class_of(object); }

RUBY_DLLSPEC VALUE rb_obj_clone(VALUE obj);

RUBY_DLLSPEC void rb_extend_object(VALUE obj, VALUE module);

// RUBY_DLLSPEC VALUE rb_obj_taint(VALUE); // not supported
static inline VALUE rb_obj_tainted(VALUE obj) { return Qfalse; }

RUBY_DLLSPEC VALUE rb_any_to_s(VALUE obj);  
RUBY_DLLSPEC VALUE rb_inspect(VALUE obj);
RUBY_DLLSPEC VALUE rb_obj_as_string(VALUE obj);

RUBY_DLLSPEC VALUE rb_obj_dup(VALUE obj);

static inline VALUE rb_obj_id(VALUE obj) { return obj; }

RUBY_DLLSPEC VALUE rb_equal(VALUE obj, VALUE other);

RUBY_DLLSPEC VALUE rb_attr_get(VALUE obj, ID id);

#ifndef MAGLEV_LINT

RUBY_DLLSPEC VALUE rb_io_write(VALUE io, VALUE str);
#define HAVE_RB_IO_FD 1
#define rb_io_fd(io) NUM2INT(rb_funcall((io), rb_intern("fileno"), 0))
// RUBY_DLLSPEC void rb_io_check_readable(rb_io_t* io);
// RUBY_DLLSPEC void rb_io_check_writable(rb_io_t* io);
// RUBY_DLLSPEC void rb_io_check_closed(rb_io_t* io);
// RUBY_DLLSPEC void rb_io_set_nonblock(rb_io_t* io);
// RUBY_DLLSPEC int rb_io_wait_readable(int f);
// RUBY_DLLSPEC int rb_io_wait_writable(int f);

// Provide our own versions which work on the objects
RUBY_DLLSPEC void rb_io_check_readable_(VALUE io);
RUBY_DLLSPEC void rb_io_check_writable_(VALUE io);
RUBY_DLLSPEC void rb_io_check_closed_(VALUE io);
RUBY_DLLSPEC int rb_io_wait_readable_(VALUE io);
RUBY_DLLSPEC int rb_io_wait_writable_(VALUE io);

// RUBY_DLLSPEC void rb_io_wait_fd(int fd)
#define rb_thread_wait_fd_(ioobj) rb_io_wait_readable_(ioobj)

#endif

RUBY_DLLSPEC VALUE rb_range_new(VALUE from, VALUE to, int exclude_end);

RUBY_DLLSPEC VALUE rb_range_beg_len(VALUE range, long* begp, long* lenp, long len, int err);

// RUBY_DLLSPEC void rb_undef_alloc_func(VALUE);
RUBY_DLLSPEC void rb_need_block(void);

RUBY_DLLSPEC VALUE rb_marshal_dump(VALUE, VALUE);
RUBY_DLLSPEC VALUE rb_marshal_load(VALUE);

RUBY_DLLSPEC VALUE rb_reg_nth_match(long n, VALUE match_data);
RUBY_DLLSPEC VALUE rb_reg_new(const char* str, long str_length, int options);
RUBY_DLLSPEC VALUE rb_reg_source(VALUE regexp);
RUBY_DLLSPEC int rb_reg_options(VALUE regexp);
RUBY_DLLSPEC VALUE rb_reg_regcomp(VALUE str);

static inline VALUE RREGEXP_SRC(VALUE reg) { return rb_reg_source(reg); }
static inline VALUE RREGEXP_OPTIONS(VALUE reg) {  return rb_reg_options(reg); }

// 1.9 provides these
// define RUBY_UBF_IO ((rb_unblock_function_t *)-1)
// define RUBY_UBF_PROCESS ((rb_unblock_function_t *)-1)

/** Release the GIL and let func run in a parallel */
// typedef VALUE rb_blocking_function_t(void *);

// typedef void rb_unblock_function_t(void *);
// RUBY_DLLSPEC VALUE rb_thread_blocking_region(rb_blocking_function_t func, void* data, rb_unblock_function_t, void*);

/** Block other threads and wait until the system select returns */
// RUBY_DLLSPEC int rb_thread_select(int max, fd_set * read, fd_set * write, fd_set * except, struct timeval *timeout);

// RUBY_DLLSPEC void rb_thread_wait_fd_rw(int fd, int read);
// RUBY_DLLSPEC void rb_thread_wait_fd(int f);
// RUBY_DLLSPEC int rb_thread_fd_writable(int f);

/** The currently executing thread */
RUBY_DLLSPEC VALUE rb_thread_current(void);

/** Calls pass on the Ruby thread class */
RUBY_DLLSPEC void rb_thread_schedule();

/** Fake placeholder. Always returns 0 */
static int rb_thread_alone(void) { return 0; }

/** Get and set thread locals */
RUBY_DLLSPEC VALUE rb_thread_local_aset(VALUE thread, ID id, VALUE value);
RUBY_DLLSPEC VALUE rb_thread_local_aref(VALUE thread, ID id);
// RUBY_DLLSPEC VALUE rb_thread_create(VALUE (*fn)(ANYARGS), void* arg); // defer

RUBY_DLLSPEC VALUE rb_time_new(long sec, long usec);

// Fake out
static inline void rb_thread_stop_timer_thread(void) { return; }
static inline void rb_thread_start_timer_thread(void) { return; }
static inline void rb_thread_stop_timer(void) { return; }
static inline void rb_thread_start_timer(void) { return; }

/** Global flag which marks the currently executing thread critical. */
// extern RUBY_DLLSPEC VALUE rb_thread_critical;

/* Global Module objects. */
RUBY_DLLSPEC extern VALUE rb_mKernel;
RUBY_DLLSPEC extern VALUE rb_mComparable;
RUBY_DLLSPEC extern VALUE rb_mEnumerable;
RUBY_DLLSPEC extern VALUE rb_mErrno;
RUBY_DLLSPEC extern VALUE rb_mFileTest;
// RUBY_DLLSPEC extern VALUE rb_mGC;    // should not be used
RUBY_DLLSPEC extern VALUE rb_mMath;
RUBY_DLLSPEC extern VALUE rb_mProcess;

/* Global Class objects */
RUBY_DLLSPEC extern VALUE rb_cObject;
RUBY_DLLSPEC extern VALUE rb_cArray;
RUBY_DLLSPEC extern VALUE rb_cBignum;
RUBY_DLLSPEC extern VALUE rb_cBinding;
RUBY_DLLSPEC extern VALUE rb_cClass;
RUBY_DLLSPEC extern VALUE rb_cDir;
RUBY_DLLSPEC extern VALUE rb_cData;
RUBY_DLLSPEC extern VALUE rb_cFalseClass;
RUBY_DLLSPEC extern VALUE rb_cFile;
RUBY_DLLSPEC extern VALUE rb_cFixnum;
RUBY_DLLSPEC extern VALUE rb_cFloat;
RUBY_DLLSPEC extern VALUE rb_cHash;
RUBY_DLLSPEC extern VALUE rb_cInteger;
RUBY_DLLSPEC extern VALUE rb_cIO;
RUBY_DLLSPEC extern VALUE rb_cMatch;
RUBY_DLLSPEC extern VALUE rb_cMethod;
RUBY_DLLSPEC extern VALUE rb_cModule;
RUBY_DLLSPEC extern VALUE rb_cNilClass;
RUBY_DLLSPEC extern VALUE rb_cNumeric;
RUBY_DLLSPEC extern VALUE rb_cProc;
RUBY_DLLSPEC extern VALUE rb_cRange;
RUBY_DLLSPEC extern VALUE rb_cRegexp;
RUBY_DLLSPEC extern VALUE rb_cString;
RUBY_DLLSPEC extern VALUE rb_cStruct;
RUBY_DLLSPEC extern VALUE rb_cSymbol;
RUBY_DLLSPEC extern VALUE rb_cThread;
RUBY_DLLSPEC extern VALUE rb_cTime;
RUBY_DLLSPEC extern VALUE rb_cTrueClass;

/* Exception classes. */
RUBY_DLLSPEC extern VALUE rb_eException;
RUBY_DLLSPEC extern VALUE rb_eStandardError;
RUBY_DLLSPEC extern VALUE rb_eSystemExit;
RUBY_DLLSPEC extern VALUE rb_eInterrupt;
RUBY_DLLSPEC extern VALUE rb_eSignal;
#ifndef MAGLEV_LINT
RUBY_DLLSPEC extern VALUE rb_eFatal;  
#endif
RUBY_DLLSPEC extern VALUE rb_eArgError;
RUBY_DLLSPEC extern VALUE rb_eEOFError;
RUBY_DLLSPEC extern VALUE rb_eIndexError;
RUBY_DLLSPEC extern VALUE rb_eStopIteration;
RUBY_DLLSPEC extern VALUE rb_eRangeError;
RUBY_DLLSPEC extern VALUE rb_eIOError;
RUBY_DLLSPEC extern VALUE rb_eRuntimeError;
RUBY_DLLSPEC extern VALUE rb_eSecurityError;
RUBY_DLLSPEC extern VALUE rb_eSystemCallError;
RUBY_DLLSPEC extern VALUE rb_eThreadError;
RUBY_DLLSPEC extern VALUE rb_eTypeError;
RUBY_DLLSPEC extern VALUE rb_eZeroDivError;
RUBY_DLLSPEC extern VALUE rb_eNotImpError;
RUBY_DLLSPEC extern VALUE rb_eNoMemError;
RUBY_DLLSPEC extern VALUE rb_eNoMethodError;
RUBY_DLLSPEC extern VALUE rb_eFloatDomainError;
RUBY_DLLSPEC extern VALUE rb_eLocalJumpError;
RUBY_DLLSPEC extern VALUE rb_eSysStackError;
RUBY_DLLSPEC extern VALUE rb_eRegexpError;
RUBY_DLLSPEC extern VALUE rb_eScriptError;
RUBY_DLLSPEC extern VALUE rb_eNameError;
RUBY_DLLSPEC extern VALUE rb_eSyntaxError;
RUBY_DLLSPEC extern VALUE rb_eLoadError;

#define rb_notimplement() rb_raise(rb_eNotImpError, "in ")
#define rb_eof_error() rb_raise(rb_eEOFError, "end of file reached")
static VALUE rb_f_notimplement(int argc, VALUE *argv, VALUE obj) {
    rb_notimplement();
    // not reached
    return (VALUE)NULL;
}

#define ruby_verbose (rb_gv_get("$VERBOSE"))
#define ruby_debug (rb_gv_get("$DEBUG"))

#define RUBY_METHOD_FUNC(func) ((VALUE (*)(ANYARGS))func)

#define ALLOCA_N(type,n) (type*)alloca(sizeof(type)*(n))

#ifdef	__cplusplus
}
#endif

#endif	/* Maglev_RUBYCEXT_H */
