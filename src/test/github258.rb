require File.expand_path('simple', File.dirname(__FILE__))

test("a %{x}" % {:x=>'b'}, 'a b', "String formating")

report