

ignore = [ /a/, /b/ ]

[ "a", "b", "c", "d" ].find do |file|
  puts "== #{file}"
  next if ignore.any? { |pattern| file =~ pattern }
  puts "== #{file} passed"
  nil
end
