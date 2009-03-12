# Check that ParseTree Gem version 3.0.3 is installed
begin
  require 'rubygems'
  rescue LoadError => exc
    puts "#{exc.class}: #{exc.message}"
    exit 1
end

if Gem.available?('ParseTree', '3.0.3')
  puts "PasrseTree 3.0.3 is installed."
  exit 0
else
  puts "PasrseTree 3.0.3 is not installed."
  exit 1
end
