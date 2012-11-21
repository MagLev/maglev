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

#### http://www.ruby-doc.org/core-1.9.3/String.html#method-i-5B-5D
a = "hello there"
test( a[1]                   , "e", 'mri slice test 1' )
test( a[2, 3]                , "llo", 'mri slice test 2' )
test( a[2..3]                , "ll", 'mri slice test 3'  )
test( a[-3, 2]               , "er", 'mri slice test 4' )
test( a[7..-2]               , "her", 'mri slice test 5' )
test( a[-4..-2]              , "her", 'mri slice test 6' )
test( a[-2..-4]              , "", 'mri slice test 7' )
test( a[12..-1]              , nil, 'mri slice test 8' )
#test( a[%r[aeiou](.)\11//]      , "ell", 'mri slice test 9' ) ## from website but not even working in MRI :(
#test( a[%r[aeiou](.)\11//, 0]   , "ell", 'mri slice test 10' ) ## from website but not even working in MRI :(
#test( a[%r[aeiou](.)\11//, 1]   , "l", 'mri slice test 11' ) ## from website but not even working in MRI :(
#test( a[%r[aeiou](.)\11//, 2]   , nil, 'mri slice test 12' ) ## from website but not even working in MRI :(
test( a["lo"]                , "lo", 'mri slice test 13' )
test( a["bye"]               , nil, 'mri slice test 14' )

########################################################################
report
true
