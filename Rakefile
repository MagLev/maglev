# MagLev Rakefile

require 'rake/clean'
require 'rake/rdoctask'
require 'rakelib/maglev.rb'

verbose false  # turn off rake's chatter about all the sh commands

CLEAN.include('*.out', 'log/vmunit*.out', 'log/all*.out', 'html')

Rake::RDocTask.new do |rd|
  rd.main = "README"
  rd.rdoc_files.include('README*', 'docs/*', 'LICENSES.txt')
end

task :default => :'maglev:status'

# This initializes the environment, and then ensures that there is a
# gemstone diretory there.  Needed to pull this out, since some of the
# initialization tasks need to be performed before there is a gemstone dir
# there, but need the ENV var (i.e., need to know where gemstone should
# be).
task :gemstone do
  raise "\nBad GEMSTONE dir: '#{GEMSTONE}'" unless File.directory?(GEMSTONE)
  raise "\nNo etc/maglev.demo.key" unless File.exists?("etc/maglev.demo.key")
end

desc "Run a squeak image"
task :squeak do
  gem_tools = '/Applications/GemTools-3.0.app'
  if File.exists?(gem_tools)
    sh %{ open #{gem_tools} }
  else
    puts "The #{gem_tools} application used by the 'squeak' command was not found on your system."
    puts "To fix this, correct the 'squeak' command in the gemstone script."
  end
end
