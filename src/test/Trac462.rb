require File.expand_path('simple', File.dirname(__FILE__))

test("test".gsub(/e(.)/, '\1e'), 'tset', 'Trac462 A')

report
