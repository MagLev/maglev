# Test case distilled from test/unit: two rescue clauses in an ensure
# clause generates an unexpected token error
#

  begin
    puts "A"
    raise ArgumentError
  rescue SyntaxError
    puts "B"
    raise ArgumentError
  rescue ArgumentError  # This rescue clause generates an unexpected token
    puts "C"
    raise SyntaxError
  end
  puts "D"
