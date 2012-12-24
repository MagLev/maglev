/*
 * $Id: ossl_bio.c 32591 2011-07-20 22:11:56Z akr $
 * 'OpenSSL for Ruby' team members
 * Copyright (C) 2003
 * All rights reserved.
 */
/*
 * This program is licenced under the same licence as Ruby.
 * (See the file 'LICENCE'.)
 */
#include "ossl.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

BIO *
ossl_obj2bio(VALUE obj)
{
    BIO *bio;

    if (TYPE(obj) == T_FILE) {
	FILE *fp;
	int fd;

	fd = rb_io_fd(obj);
	rb_io_check_readable_(obj);
	if (fd < 0){
	    rb_sys_fail(0);
	}
	if (!(fp = fdopen(fd, "r"))){
	    close(fd);
	    rb_sys_fail(0);
	}
	if (!(bio = BIO_new_fp(fp, BIO_CLOSE))){
	    fclose(fp);
	    ossl_raise(eOSSLError, NULL);
	}
    }
    else {
	StringValue(obj);
	bio = BIO_new_mem_buf(RSTRING_PTR(obj), RSTRING_LENINT(obj));
	if (!bio) ossl_raise(eOSSLError, NULL);
    }

    return bio;
}

BIO *
ossl_protect_obj2bio(VALUE obj, int *status)
{
     BIO *ret = NULL;
     ret = (BIO*)rb_protect((VALUE(*)_((VALUE)))ossl_obj2bio, obj, status);
     return ret;
}

VALUE
ossl_membio2str0(BIO *bio)
{
    VALUE ret;
    BUF_MEM *buf;

    BIO_get_mem_ptr(bio, &buf);
    ret = rb_str_new(buf->data, buf->length);

    return ret;
}

VALUE
ossl_protect_membio2str(BIO *bio, int *status)
{
    return rb_protect((VALUE(*)_((VALUE)))ossl_membio2str0, (VALUE)bio, status);
}

VALUE
ossl_membio2str(BIO *bio)
{
    VALUE ret;
    int status = 0;

    ret = ossl_protect_membio2str(bio, &status);
    BIO_free(bio);
    if(status) rb_jump_tag(status);

    return ret;
}
