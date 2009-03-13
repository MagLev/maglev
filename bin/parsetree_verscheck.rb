# Check that ParseTree Gem version 3.0.3 is installed
begin
  require 'rubygems'
  rescue LoadError => exc
    puts "#{exc.class}: #{exc.message}"
    exit 1
end

### NOTE: Other programs look for 'installed' so
### don't change it. You can change the 3.0.3 though.
if Gem.available?('ParseTree', '3.0.3')
  puts "ParseTree 3.0.3 is installed."
  exit 0
else
  puts "ParseTree 3.0.3 is missing."
  exit 1
end
