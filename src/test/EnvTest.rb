require File.expand_path('simple', File.dirname(__FILE__))

test(ENV.keys.size > 0,    true, 'ENV.keys A')
test(ENV.keys[0,2].length, 2,    'ENV.keys B')


ev = ENV 
unless ev['foo'] == nil ; raise 'err'; end
ev['foo'] = 'abc'
unless ev['foo'] == 'abc' ; raise 'err'; end
ev.delete('foo')
unless ev['foo'] == nil ; raise 'err'; end
ev['foo'] = 'abc'
unless ev['foo'] == 'abc' ; raise 'err'; end
ev.delete_if() { |k , v| k == 'foo' }
unless ev['foo'] == nil ; raise 'err'; end

x = 0
[ 'MAGLEV_HOME' , 'MAGLEVX', 'GEMSTONE', 'GEMSTONEX' ].each { | k |
  begin
    ev[k] = '99'
    nil.pause  # should not reach here
  rescue
    x += 1
  end
  begin
    ev.delete(k)
    nil.pause  # should not reach here
  rescue
    x += 1
  end
}
unless x == 8 ; raise 'error'; end
report
true
