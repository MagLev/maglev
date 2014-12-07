#!/bin/sh

for i in `grep '#if !defined(HAVE_' openssl_missing.c | sed -e 's/\#if\ \!defined(HAVE_//' -e 's/).*//' `
do
    if nm ../../../../../gemstone/lib/libssl*.dylib | grep -qi "$i"
    then 
	echo Has $i; 
    else 
	echo Doesnt have $i; 
    fi
done
