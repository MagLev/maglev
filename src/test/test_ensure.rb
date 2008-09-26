# Test case distilled from test/unit: two rescue clauses in an ensure
# clause generates an unexpected token error
#

begin
  puts "begin"
rescue
  puts "rescue"
ensure
  begin
    puts "ensure begin"
  rescue SyntaxError
    puts "ensure rescue SyntaxError"
  rescue Exception  # This rescue clause generates an unexpected token
    puts "ensure rescue Exception"
  end
end
