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
