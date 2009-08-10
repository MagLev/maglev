require File.expand_path('simple', File.dirname(__FILE__))

require 'rdoc/rdoc'

data_dir = File.expand_path(File.dirname(__FILE__) + '/test_data')
files =[
#  "#{ENV['MAGLEV_HOME']}/lib/ruby/1.8/rdoc/rdoc.rb",
  "#{data_dir}/rdoc_file_1.rb",
  ]

files.each do |f|
  puts "Processing #{f}"
  rdoc = RDoc::RDoc.new
  rdoc.document([f])
end

report
