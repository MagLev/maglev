n = 0
begin
  n = n + 1 
  raise
  n = n + 10 
  rescue
    n = n + 100    
    puts "rescueA"
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
  n = n + 5 
  raise ScriptError
  n = n + 10 
  rescue ScriptError
    n = n + 100    
    puts "rescueB"
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
    puts "rescueC"
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
      puts "rescueD"
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
      puts "rescue SyntaxError"
    rescue ScriptError  # This rescue clause generates an unexpected token
      a = 101
      puts "rescue ScriptError"
  end
  unless a == 101
    raise 'ERR'
  end
end
true
