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
unless n = 10101
  raise 'ERR'
end
true
