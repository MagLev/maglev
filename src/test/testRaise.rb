n = 0
begin
  n = n + 1
  raise
  n = n + 10
  rescue
    n = n + 100
  else
    n = n + 1000
   ensure
     n = n + 10000
end
unless n == 10101
  raise 'ERR'
end

n = 0
begin
  n = n + 1
  n = n + 10
  rescue
    n = n + 100
  else
    n = n + 1000
  ensure
    n = n + 10000
end
unless n == 11011
  raise 'ERR'
end

n = 0
begin
  n = n + 5
  raise ScriptError
  n = n + 10
  rescue ScriptError
    n = n + 100
  else
    n = n + 1000
  ensure
    n = n + 10000
end
unless n == 10105
  raise 'ERR'
end

n = 0
begin
  n = n + 6
  raise SyntaxError
  n = n + 10
  rescue ScriptError, SyntaxError
    n = n + 100
end
unless n == 106
  raise 'ERR'
end

n = 0
begin
  n = n + 8
  elist = [ ScriptError, SyntaxError ]
  begin
    raise SyntaxError
    n = n + 10
    rescue *elist
      n = n + 100
    else
      n = n + 1000
    ensure
      n = n + 10000
  end
  unless n == 10108
    raise 'ERR'
  end
end

true

begin
  a = 88
  begin
    raise ScriptError
    rescue SyntaxError
      a = 99
    rescue ScriptError  # This rescue clause generates an unexpected token
      a = 101
  end
  unless a == 101
    raise 'ERR'
  end
end

begin
  a = 99
  y = 98
  y = $!
  unless y.equal?(nil)
    raise 'ERR'
  end
  begin
    a.frob
    rescue NoMethodError => ex
      unless ex.class.equal?(NoMethodError)
        raise 'ERR'
      end
      a = 97
  end
  unless a == 97
     raise 'ERR'
  end
end

begin
  n=0
  while n < 10
    n+=1
    begin
    rescue
    end
  end
  unless n == 10
     raise 'ERR'
  end
end

begin
  b = 0
  c = 0
  d = nil
  e = nil
  begin
    x = 5 / 0
  rescue
    b = $!
    if RUBY_VERSION == "1.8.7"
      $! = ArgumentError.new
      c = $!
    end
  end
  d = $!
  if RUBY_VERSION == "1.8.7"
    $! = TypeError.new
    e = $!
  end
  puts [b,c,d,e].inspect
  unless b.class.equal?( ZeroDivisionError) ; raise 'err' ; end

  if RUBY_VERSION == "1.8.7"
    unless c.class.equal?( ArgumentError) ; raise 'err' ; end
  end

  unless d.equal?( nil) ; raise 'err' ; end

  if RUBY_VERSION == "1.8.7"
    unless e.class.equal?( TypeError) ; raise 'err' ; end
  end
end
true

begin
  begin
    raise
  rescue
    e = $!
    unless e.class.equal?(RuntimeError)  ; raise 'err' ; end
  end
end

begin
  a = 5
  begin
    a = 6
    raise Exception
    a = 7
  rescue Object
    a = a + 20
  end
  unless a == 26 ; raise 'Error'; end
end

begin
  a = 5
  begin
    a = 6
    raise Exception
    a = 7
  rescue TypeError, Object
    a = a + 20
  end
  unless a == 26 ; raise 'Error'; end
end

begin
  a = 5
  begin
    a = 6
    raise Exception
    a = 7
  rescue Object, TypeError
    a = a + 20
  end
  unless a == 26 ; raise 'Error'; end
end

begin
  a = 5
  begin
    a = 6
    raise Exception
    a = 7
  rescue TypeError
    a = a + 10
  rescue Object
    a = a + 20
  end
  unless a == 26 ; raise 'Error'; end
end

begin
  a = 5
  begin
    a = 6
    raise Exception
    a = 7
  rescue Object
    a = a + 20
  rescue TypeError
    a = a + 10
  end
  unless a == 26 ; raise 'Error'; end
end

begin
  a = 5
  begin
    a = 6
    raise TypeError
    a = 7
  rescue TypeError
    a = a + 10
  rescue Object
    a = a + 20
  end
  unless a == 16 ; raise 'Error'; end
end

true
