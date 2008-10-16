require File.expand_path('simple', File.dirname(__FILE__))

#     BEGIN TEST CASES

test(''.class == String, true, "''.class == String")

test(String.class == Class, true, "String.class == Class")
test(String.class.eql?(Class), true, "String.class.eql? Class")
test(String.class.equal?(Class), true, "String.class.equal? Class")

test(String.class == Fixnum.class, true, "String.class == Fixnum.class")

report
