## MagLev implementation of Ruby C extensions API.

This document explains the state of C extensions on Maglev. This is
meant to become an exhaustive listing of the restrictions in the C API
layer, their reasons, and how to work around them.

We also provide code examples for some workarounds at the end of this
document.

If more restrictions are found, or you have code examples, please feel
free to add them.

### Objectives

Provide a high performance implementation of the Ruby C extension
API without compromising garbage collector performance,
and without introducing expensive object identifer translations.

The Ruby C VALUE  is the same as the OopType from the Gemstone C API( GCI)
The Maglev VM already knows how to associate an object identifier
with an object.  This identifier is used both for object identity
of persistent objects on disk , and as the identifier exported through
GCI function calls.  We use this same object identifier for VALUE's
in the Ruby C API.

### Restrictions

#### Special objects

* **Qnil, Qfalse, Qtrue**

    *Problem:* Maglev does not support C comparisons expecting Qfalse
    to have C integer value zero.  Qnil, Qtrue, Qfalse are all
    non-zero values in Maglev.
    
    *Workaround:* You must use the RTEST macro before doing a C
    comparision of a VALUE to zero. Based on looking at a few C
    extensions, it would appear that all uses of C expressions of the
    form x ? a : b need to be examined for missing RTEST(x). If you
    want to test for Qnil, use the NIL_P macro.

* **NULL**

    *Problem:* Casting the C value NULL to the type VALUE is not
    supported. In Maglev (VALUE)0 is not a legal object
    identifier. Attempts to use it will raise a StandardError, 'object
    does not exist'.
    
    *Workaround:* In most cases Qnil should be used instead of
    (VALUE)NULL.  Corresponding changes to use RTEST instead of a C
    compare to zero or other code changes may be required.

#### Strings

* **RSTRING_PTR**

    *Problem:* Has a result type of const char* in Maglev. Maglev will
    replicate the body of a String object to C memory on request.
    That body is then read-only from C.
    
    *Workaround:* To change bytes in a String object, you must use one
    of
    
        void rb_store_string_l(VALUE str, long offset, const char* value, size_t len);       rb_store_string(VALUE str, long offset, const char* value);    
        VALUE rb_str_buf_append(VALUE str, VALUE val);
        VALUE rb_str_buf_cat(VALUE str, const char* bytes, long len);
        VALUE rb_str_buf_cat2(VALUE str, const char* cstring);
        VALUE rb_str_append(VALUE str, VALUE arg);
	
    After a String has been modified by one of the above store
    functions, or by any execution of Ruby code, a subsequent
    RSTRING_PTR, rb_str_ptr, or rb_str_ptr_readonly (all of which call
    rb_str2cstr) will return a pointer to updated C memory.  If the
    String has grown since the previous RSTRING_PTR call, the returned
    memory may be a different C memory pointer, otherwise contents of
    the previously allocated C memory will be updated by the
    rb_str2cstr() implementation to reflect the current contents of
    the String .

    The lifetime of the C memory referenced by the result of RSTRING_PTR
    is the same as the garbage collection lifetime of the corresponding 
    String object.
    
    C code must copy string data to it's own C memory if it expects
    indefinite lifetime of the C memory, or to control lifetime by
    free(). Based on the "Lifetimes of temporary objects" below, C
    code needs to copy the result of RSTRING_PTR to other C memory if
    the C memory is needed beyond the lifetime of the current Ruby to
    C extension invocation, unless it is known that the String will be
    kept alive by longer lived Ruby objects, and will not be changed.
    
    The result of RSTRING_PTR must not be passed to free()

* **rb_str_set_len**

    *Problem:* This will change the logical size of the string, and if
    it is used to shrink the string and then grow it all bytes beyond
    the size produced by the shrink will be zero after the string is
    grown again, because Maglev doesn't keep an underlying char*
    buffer that is still larger than the logical size
    
    *Workaround:* Do not rely on this MRI behavior

* **rb_str_buf_new(x)**

    *Problem:* The argument to rb_str_buf_new is ignored, which makes
    this equivalent to rb_str_new("", 0). The physical size of a
    String object is not accessible from Ruby, Smalltalk, or Ruby C
    extensions, so the logical and physical sizes are always the same
    from the point of view of C extensions.
    
    *Workaround:* Since the result of RSTRING_PTR is read-only, one of
    the rb_str_buf_append or rb_str_buf_cat* functions must be used to
    append to the result of rb_str_buf_new .

#### Arrays

* **RARRAY_PTR**

    *Problem:* Unsupported. In MRI this macro returns a pointer to the
    body of an Array. Arrays are Smalltalk objects on Maglev, so we
    cannot simply expose them as a C array. RARRAY_PTR is not
    supported in Maglev because the number of uses in a typical C
    extension is small, and if it were supported it could only be via
    const VALUE* RARRAY_PTR(VALUE ary); semantics - updates would have
    to go through a function call to rb_ary_store.  It is easier to
    change a C extension to use rb_ary_entry instead of RARRAY_PTR
    than it would to change C code to substitute a function calls for
    a RSTRING_PTR calls, because RARRAY_PTR is usually used to access
    individual elements of an Array.  Thus we have chosen not to
    implement object memory to C memory replication of Array bodies.

    *Workaround:* To access elements of an Array in Maglev, use
    rb_ary_entry or rb_ary_store which are also available in JRuby and
    Rubinius.

#### Hash

* **rb_iterate_each_pair**

    *Problem:* Only supports iteration on Array arguments for now
    
    *Workaround:* None

* **RHASH**, **RHASH_TBL**
    
    *Problem:* Like on JRuby and Rubinius, these macros aren't supported.
    
* **st.h**
    
    *Problem:* The Maglev VM does not implement the MRI st.h API.
    
    *WorkaroundL* A C extension which needs to use the C hash
    implementation of st.h must ship and build it's own copy of
    st.c. (observed in nokogiri)

#### Structs

* **DATA_PTR**

    *Problem:* Direct access is not supported.

    *Workaround:* To change the C data wrapped by an object, use
    
        void rb_rdata_store(VALUE obj, void *p);
    To fetch the C data use
    
        void* rb_rdata_fetch(VALUE obj);
    
    rb_rdata_store will run the free function previously installed by
    Data_Wrap_Struct if appropriate, and will decide how to allocate
    the meta information for the object to hold the C data pointer if
    no C pointer was previously associated with the object.
    
* **Data_Wrap_Struct**

    *Problem:* Calling Data_Wrap_Struct with a non-NULL mark function
    is not supported yet.

    *Workaround:* If the referenced object(s) to be kept alive exist
    at the point of the Data_Wrap_Struct call, use rb_ivar_set to
    create an object reference to them.

#### Test-functions

* **rb_obj_is_kind_of**, **rb_obj_is_instance_of**, **rb_ivar_defined**, ...

    *Problem:* MRI, JRuby and Rubinius all define
    
        VALUE rb_obj_is_kind_of(VALUE, VALUE);
        VALUE rb_obj_is_instance_of(VALUE, VALUE);
        VALUE rb_ivar_defined(VALUE, ID);

    and they are commented as /** Returns true-ish ... */. The common
    usage pattern, however, appears to be like
    
        if (! (rb_obj_is_kind_of(rb_node, cNokogiriXmlNode)))  
    
    which fails to apply RTEST to the result of rb_obj_is_* functions
    before doing the C comparision to zero (see also restrictions on
    special values Qfalse and Qtrue, above).

    *Workaround:* Add RTEST macro calls as needed around the results
    of these functions. Use MAGLEV_LINT (see below) to help find
    possibly bad uses.
    
#### Globals and GC

* **rb_global_variable**, **rb_gc_register_address**, **rb_gc_unregister_address**

    *Problem:* To implement these requires supporting an arbitrary
    number of garbage collection roots, which reduces garbage
    collector performance.  A fast garbage collector needs to have a
    small root set, so that a scavenge only needs to examine a minimum
    number of young objects.  In the Maglev VM, the root set includes
    the stacks of recently active Ruby Threads, and only a small
    number of well known objects needed by the VM.  All other objects
    are reachable from those well known objects, and thus only need to
    be examined by a scavenge if they have been recently created or
    dirtied since a previous scavenge.

    *Workarounds:*
      * To define global variables, use rb_gv_set instead.
      * To protect a temporary C array of VALUE from garbage
	collection, create an Array object and store values into it with
	rb_ary_store
      * Maglev defines the additional function rb_funcall2_ to take a
        Ruby array argument

            VALUE rb_funcall2(VALUE obj, ID meth, int cnt, VALUE* args);
	    VALUE rb_funcall2_(VALUE obj, ID meth, int cnt, VALUE args_array);

* **rb_gv_get** 

    *Problem:* Retrieving $~ is not supported from C

    *Workaround:* None.

* **Object lifetime**

    *Problem:* C-references aren't kept alive/valid after returning
    from C extension code.
    
    Each call from Ruby into a C extension initializes an IdentitySet
    known to the garbage collector that is kept alive for the duration
    of that entry into a C extension.  This IdentitySet is the 'export
    set' for that entry into the C extension.  The IdentitySet is
    dereferenced upon return from C back to Ruby.

    The following are automatically added to the export set, to
    ensure objects stay alive at least for the duration of the Ruby
    to C call .
      * Newly created objects, such as results of rb_ary_new* ,
	rb_str_new*, rb_str_buf_new* .
      * Objects returned from Ruby execution, such as results from rb_funcall*
      * Objects for which C memory pointers were returned by
        RSTRING_PTR or rb_str2cstr.

    *Workaround:* For a newly created object to stay alive after
    returning from C to Ruby it must have been stored as the value of
    a Ruby global variable, Ruby constant, or stored into an instance
    variable of some other object reachable from top level Ruby state,
    or be reachable from the VALUE returned from C back to Ruby.

#### Exception Handling

* **rb_rescue**

    *Problem:* In Ruby code invoked from the C rescue function (second
    procedure arg to rb_rescue or rb_rescue2, $!  does not reflect the
    exception being rescued by the application's C code. $! only
    reflects exceptions being rescued by Ruby rescue blocks.
    
    *Workaround:* None

* **rb_eException**

    *Problem:* The maglev class Exception does not define the instance
    variables 'mesg' or 'backtrace', so they cannot be accessed
    directly from C (observed in Psych)
    
    *Workaround:* Initialization of instances of Exception should use
    rb_obj_call_init, instead of rb_iv_set. Accessing the backtrace of
    an Exception should use an appropriate variant of rb_funcall to
    call a public method in Exception.

#### Numerics

* **rb_big2ulong**

    *Problem:* Raises a RangeError when passed a negative argument.
    
    *Workaround:* Argument checking.

#### Regexps

* **rb_reg_nth_match**

    *Problem:* Regexp matches invoked from C do not set $1, $2 in the
    Ruby code which called the C code.
    
    *Workaround:* Retrieve and store or pass the matches explicitely


### Lint

Many of the aforementioned problems aren't immediately obvious, and
might not lead compiler errors or SEGVs straight away, but rather
create subtle and hard-to-track bugs in C extension execution. To help
find possible causes for C extension bugs on Maglev, the MAGLEV_LINT
macro can be used.

Maglev defines some functions for which we have seen problematic usage
patterns (like rb_obj_is_\*, rb_\*_defined) only if MAGLEV_LINT is
undefined. Thus, to check your extension for those functions and the
probable bad usage of them, you can use use `-DMAGLEV_LINT=1` on a C
compile to trigger compiler errors. Then use this document to apply
the appropriate workarounds as needed.

### Bugs and status of rubyspecs (as of 17 Feb 2011)

For the rubyspecs in optional/capi/ , the following specs are skipped
with 'not_compliant_on :maglev do' because they are not implemented at
all (and some of them never might be)

 * rb_ary_new2_assign
 * rb_ary_to_s
 * rb_block_proc
 * rb_call_super
 * rb_define_hooked_variable
 * rb_define_readonly_variable
 * rb_define_variable
 * rb_exec_recursive
 * rb_hash
 * rb_hash_delete_if
 * rb_hash_foreach
 * rb_mem_clear
 * rb_num_zerodiv
 * rb_reg_match
 * rb_set_kcode
 * rb_str_buf_new
 * rb_str2cstr_replace
 * rb_struct_aref
 * rb_struct_aset
 * rb_struct_new
 * rb_str_hash
 * rb_throw
 * rb_big2ll
 * rb_ll2inum
 * RARRAY
 * RARRAY_PTR_assign
 * RBIGNUM_SIGN
 * RFLOAT
 * RSTRING
 * uses of RSTRING_PTR which attempt to change the string
 * uses of STR2CSTR  which attempt to change the string
    
The following functions have known bugs that we will fix
  
 * rb_cvar_defined has some failures
 * rb_const_get_from , rb_const_get_at fail to call const_missing
 * The protections of methods installed by rb_define_private_method,
 * rb_define_protected_method  is not correct.
 * rb_is_type_data(Time.now)  returns false ; it should be true

### Code samples

Following are some code examples of changes required to use C
extensions on Maglev.

##### examples of  (VALUE)NULL   problems

    --- a/ext/nokogiri/xml_syntax_error.c
    +++ b/ext/nokogiri/xml_syntax_error.c
    @@ -3,19 +3,19 @@
     void Nokogiri_error_array_pusher(void * ctx, xmlErrorPtr error)
     {
       VALUE list = (VALUE)ctx;
    -  rb_ary_push(list,  Nokogiri_wrap_xml_syntax_error((VALUE)NULL, error));
    +  rb_ary_push(list,  Nokogiri_wrap_xml_syntax_error( Qnil , error));
     }
     
     void Nokogiri_error_raise(void * ctx, xmlErrorPtr error)
     {
    -  rb_exc_raise(Nokogiri_wrap_xml_syntax_error((VALUE)NULL, error));
    +  rb_exc_raise(Nokogiri_wrap_xml_syntax_error( Qnil , error));
     }
     
     VALUE Nokogiri_wrap_xml_syntax_error(VALUE klass, xmlErrorPtr error)
     {
       VALUE msg, e;
     
    -  if(!klass) klass = cNokogiriXmlSyntaxError;
    +  if( klass == Qnil ) klass = cNokogiriXmlSyntaxError;
     
    ----- ext/nokogiri/xml_node.c

    @@ -1113,7 +1115,7 @@ static VALUE new(int argc, VALUE *argv, VALUE klass)
       NOKOGIRI_ROOT_NODE(node);
     
       rb_node = Nokogiri_wrap_xml_node(
    -      klass == cNokogiriXmlNode ? (VALUE)NULL : klass,
    +      klass == cNokogiriXmlNode ? Qnil : klass,

##### examples of missing RTEST

    -----  ext/psych/emitter.c
     
    @@ -160,7 +163,7 @@ static VALUE start_document(VALUE self, VALUE version, VALUE tags, VALUE imp
                (RARRAY_LEN(version) > 0) ? &version_directive : NULL,
                head,
                tail,
    -           imp ? 1 : 0
    +           RTEST(imp) ? 1 : 0
     
     
    @@ -182,7 +185,7 @@ static VALUE end_document(VALUE self, VALUE imp)
    -    yaml_document_end_event_initialize(&event, imp ? 1 : 0);
    +    yaml_document_end_event_initialize(&event, RTEST(imp) ? 1 : 0);
    tatic VALUE scalar(
            Check_Type(tag, T_STRING);
            tag = rb_str_export_to_enc(tag, encoding);
         }
    +#else
    +    if(!NIL_P(anchor)) {
    +        Check_Type(anchor, T_STRING);
    +    }
    +    if(!NIL_P(tag)) {
    +        Check_Type(tag, T_STRING);
    +    }
     #endif

##### psych contains code dependent on the name of an MRI instVar

    --- a/ext/psych/to_ruby.c
    +++ b/ext/psych/to_ruby.c
    @@ -10,7 +10,8 @@ static VALUE build_exception(VALUE self, VALUE klass, VALUE mesg)
     {
         VALUE e = rb_obj_alloc(klass);
     
    -    rb_iv_set(e, "mesg", mesg);
    +    // rb_iv_set(e, "mesg", mesg);  // MRI implementation dependent
    +    rb_obj_call_init(e, 1, &mesg);

##### avoidance of rb_gc_register_address , from nokogiri

    +++ b/ext/nokogiri/xml_xpath_context.c
    @@ -53,7 +53,7 @@ static void ruby_funcall(xmlXPathParserContextPtr ctx, int nargs)
     {
       VALUE xpath_handler = Qnil;
       VALUE result;
    -  VALUE *argv;
    +  // VALUE *argv;
       VALUE doc;
       VALUE node_set = Qnil;
       xmlNodeSetPtr xml_node_set = NULL;
    @@ -68,10 +68,11 @@ static void ruby_funcall(xmlXPathParserContextPtr ctx, int nargs)
     
       xpath_handler = (VALUE)(ctx->context->userData);
     
    -  argv = (VALUE *)calloc((size_t)nargs, sizeof(VALUE));
    -  for (i = 0 ; i < nargs ; ++i) {
    -    rb_gc_register_address(&argv[i]);
    -  }
    +  //argv = (VALUE *)calloc((size_t)nargs, sizeof(VALUE));
    +  //for (i = 0 ; i < nargs ; ++i) {
    +  //  rb_gc_register_address(&argv[i]);
    +  //}
    +  VALUE argv = rb_ary_new2(nargs);
     
       doc = DOC_RUBY_OBJECT(ctx->context->doc);
     
    @@ -80,46 +81,51 @@ static void ruby_funcall(xmlXPathParserContextPtr ctx, int nargs)
         obj = valuePop(ctx);
         switch(obj->type) {
           case XPATH_STRING:
    -        argv[i] = NOKOGIRI_STR_NEW2(obj->stringval);
    +        rb_ary_store( argv, i, NOKOGIRI_STR_NEW2(obj->stringval));
             break;
    ......

##### example for missing Data_Wrap_Struct mark-function support from nokogiri
    
    static VALUE sym_iv_doc = Qnil;
        
    void init_xml_node() {
      ...
      sym_iv_doc = rb_intern("@doc");
      ...
    }
     
    VALUE Nokogiri_wrap_xml_node(VALUE klass, xmlNodePtr node) {
      ...
      if (DOC_RUBY_OBJECT_TEST(node->doc)) {  // maglev workaround , no gc mark yet
        VALUE ref = DOC_RUBY_OBJECT(node->doc);
        if (ref != 0 && ref != Qnil && sym_iv_doc != Qnil) {
          rb_node = Data_Wrap_Struct(klass, NULL, debug_node_dealloc, node) ;
          rb_ivar_set(rb_node, sym_iv_doc, ref);
        }
      }
      ...
    }
       
