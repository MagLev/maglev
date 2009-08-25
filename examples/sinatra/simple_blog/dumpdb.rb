# This just prints out the current state of the blog
Maglev::PERSISTENT_ROOT[Post].values.each { |p| p p}
p Maglev::PERSISTENT_ROOT[Tag]

puts "======== TAGS: "
Maglev::PERSISTENT_ROOT[Tag].values.each { |t| puts "TAG: #{t.name}: #{t.inspect}"}
