# Print a list of the .rb files in this directory that are not listed in
# vmunit.conf
dir = File.dirname(__FILE__)

rb_files = Dir.glob(dir + '/*.rb').map { |f| File.basename f }.sort

# These files are ones we know are not test files, and so we do not report
# them
skip_report = %w(lsnotrun.rb simple.rb AutoloadHelper.rb AutoloadHelper2.rb)
rb_files = rb_files - skip_report

conf_files = File.open(dir + '/vmunit.conf') do |file|
  file.reject do |line|
    line =~ /^\s*#/
  end
end.sort.map { |f| f.chomp }

not_run_files = rb_files - conf_files
puts not_run_files
puts "-----------------------------------"
puts "Num test files: #{rb_files.size}"
puts "Num run:        #{conf_files.size}"
puts "Num skipped:    #{not_run_files.size}"
