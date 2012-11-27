# encoding: utf-8
require File.expand_path('simple', File.dirname(__FILE__))

########################################################################
#### get content of string

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
#### change content of string

#### change one character
s = "0123456789"
test( s[3] = "3", "3", "no change of content returns string passed to it" )
test( s[2] = "3", "3", "slice of content returns string passed to it" )
test( s, "0133456789", "string has changed")

s = "0123456789"
test( s[2..100] = "987", "987", "slice with range returns string assigned to it" )
test( s, "01987", "slice to cut and append end" )

#### change many characters

s = "0123456789"
s[10..12] = "12"
test( s, "012345678912", "slice appends to end")

#### make string smaller

s = ":ангел"
s[2...4] = "р"
test(s, ":арел", "replace in middle of unicode" )

#### make string bigger

s = "фрукт"
s[5..5] = "овый!"
test( s, "фруктовый!", "slice appends to end unicode" )

#### errors if out of bounds

def testForRangeError( arg1, arg2 = nil)
  err = nil
  s = "зад"
  begin
    if arg2
      s[arg1, arg2] = "12" 
    else 
      s[arg1] = "12" 
    end
  rescue RangeError => err
    test( err.message["out of range"], "out of range", "slicing off from outside raises and RangeError" )
  rescue IndexError => err
    test( err.message["out of string"], "out of string", "slicing off with length from outside raises and IndexError" )
  end
  test( err.eql?(nil), false, "the error has risen!" )
end

s = "зад"
testForRangeError(5..20)
s = "Том" # 6 bytes but 3 characters
testForRangeError(-5..4)
testForRangeError(-5, 4)
testForRangeError(5, 4)

########################################################################
report
true
