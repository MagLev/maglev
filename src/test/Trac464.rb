require File.dirname(__FILE__) + '/simple'
s = 'test'

test(s[-2,0] = '!',    '!', 'Test 1')
test(s,            'te!st', 'Test 2')

s = 'test'
test(s,            'test',  'Test 3')

# Now test with a positive offset
s = 'test'

test(s[2,0] = '!',     '!', 'Test 4')
test(s,            'te!st', 'Test 5')

s = 'test'
test(s,            'test',  'Test 6')

# Now test with counts larger than 0
s = 'test'
test(s[-2,1] = '!',     '!', 'Test 7')
test(s,              'te!t', 'Test 8')

s = 'test'
test(s[2,1] = '!',     '!', 'Test 9')
test(s,             'te!t', 'Test 10')

s = 'test'
test(s[-2,2] = '!',     '!', 'Test 11')
test(s,               'te!', 'Test 12')

s = 'test'
test(s[2,2] = '!',     '!', 'Test 13')
test(s,              'te!', 'Test 14')


# Now test with counts larger than the string size
s = 'test'
test(s[-2,3] = '!',     '!', 'Test 15')
test(s,               'te!', 'Test 16')

s = 'test'
test(s[2,3] = '!',     '!', 'Test 17')
test(s,              'te!', 'Test 18')

report
#################### Trac Info
# ID:         464
# Summary:    Modifying strings in place with negative indices corrupts the data
# Changetime: 2009-04-24 22:49:28+00:00
###

#  There may be more than one problem here; modifying a string in place produces incorrect results ''and'' apparently makes it impossible to reassign the variable in question.
#  {{{
#  (markus@glass) ~/maglev/MagLev-21530.Linux/irb2> maglev-irb
#  >> s = 'test'
#  => "test"
#  *> s[-2,0] = '!'
#  => "!"
#  *> s
#  => "te!t\000"
#  }}}
#  Under MRI we'd see {{{"te!st"}}}.  Trying again:
#  {{{
#  *> s = 'test'
#  => "test"
#  *> s[2,0] = '!'
#  => "!"
#  *> s
#  => "te!t\000\000"
#  *> s = 'test'
#  => "test"
#  *> s[2,0] = '!'
#  => "!"
#  *> s
#  => "te!t\000\000\000"
#  }}}
#  Note that an additional {{{\000}}} is appearing each time, even though we are always resetting {{{s}}} to {{{"test"}}}.  How is it remembering the count?
#  {{{
#  *> s = 'test'
#  => "test"
#  *> s
#  => "te!t\000\000\000"
#  }}}
#  So assigning a new value to {{{s}}} no longer works.  Odd.
#  
#  And that's as far as I chased it.
#  