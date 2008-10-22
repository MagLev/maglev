require File.expand_path('simple', File.dirname(__FILE__))

test(ENV.keys.size > 0,    true, 'ENV.keys A')
test(ENV.keys[0,2].length, 2,    'ENV.keys B')

report
