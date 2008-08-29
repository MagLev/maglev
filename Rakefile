# MagLev Rakefile
#
# Currently, this Rakefile does many, but not all, of the functions in
# bin/gemstone.  Eventually, it will provide all of the functions in
# bin/gemstone, and other MagLev tasks as the need arises.
#
# Ideas for other tasks:
#
# * The rest of the tasks in bin/gemstone (topaz, ruby,...)
# * clean (remove logs etc.)
# * follow-parser: do a tail -f on the parser log
# * git support for typical workflows (see git support in Rubinius Rakefile)
# * allow command line control of the verbosity of the "sh" calls.
#

require 'rakelib/gemstone'
require 'rake/clean'

CLEAN.include('load*.out', 'topazerrors.log')

task :default => :'gs:status'  # TODO: Do we want to leave this as the default?

# This initializes the environment, and then ensures that there is a
# gemstone diretory there.  Needed to pull this out, since some of the
# initialization tasks need to be performed before there is a gemstone dir
# there, but need the ENV var (i.e., need to know where gemstone should
# be).
task :gemstone do
  raise "Bad GEMSTONE dir: '#{GEMSTONE}'" unless File.directory?(GEMSTONE)
end

desc "Run squeak"
task :squeak do
  gem_tools = '/Applications/GemTools-3.0.app'
  if File.exists?(gem_tools)
    sh %{ open #{gem_tools} }
  else
    puts "The #{gem_tools} application used by the 'squeak' command was not found on your system."
    puts "To fix this, correct the 'squeak' command in the gemstone script."
  end
end
