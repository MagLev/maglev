# Print a list of the .rb files in this directory that are not listed in
# vmunit.conf
dir = File.dirname(__FILE__)

rb_files = Dir.glob(dir + '/*.rb').map { |f| File.basename f }.sort

conf_files = File.open(dir + '/vmunit.conf') do |file|
  file.reject do |line|
    line =~ /^\s*#/
  end
end.sort.map { |f| f.chomp }

# These files are ones we know are not test files, and so we do not report
# them
skip_report = %w(lsnotrun.rb simple.rb)
puts rb_files - conf_files - skip_report
