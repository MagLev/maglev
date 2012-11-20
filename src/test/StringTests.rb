# encoding: utf-8
require File.expand_path('simple', File.dirname(__FILE__))

########################################################################
####


#### squeeze

test('folder/file.rb'.squeeze('/'),    'folder/file.rb',     "String.squeeze / ")
test('folder//file.rb'.squeeze('/'),    'folder/file.rb',     "String.squeeze // ")
test('folder///file.rb'.squeeze('/'),    'folder/file.rb',     "String.squeeze /// ")
test('///file.rb/////'.squeeze('/'),    '/file.rb/',     "String.squeeze at start and end ")

#### eql?

test('asdf'.eql?('asdf'), true, 'ascii equals')
test('asdf'.eql?('sdf'), false, 'ascii equals')
test('asdf'.eql?('ösdf'), false, 'unequal unicode')
test('ösdf'.eql?('ösdf'), true, 'equal unicode')
test('ösdf'.eql?('öäsdf'), false, 'unequal unicode')

#### split

test('asdfasdf'.split('a'), ["", "sdf", "sdf"], 'split into three')
test('asdfasdf'.split('as'), ["", "df", "df"], 'split into three with two characters')
test('asdfasdf'.split('t'), ['asdfasdf'], 'no split')
test('111ä111ä111'.split('ä'), ["111", "111", "111"], 'split ä')

#### slicing backwards

test( "123"[1..0], "", 'begin inside' )
test( "123"[3..0], "", 'begin inside (2)' )
test( "123"[4..0], nil, 'begin outside' )


########################################################################
report
true
