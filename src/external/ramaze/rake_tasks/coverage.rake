#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'rake'

spec_base = File.expand_path('spec/ramaze/')
example_base = File.expand_path('examples')
snippets_base = File.expand_path('spec/snippets')
# ignore files with these paths
ignores = [ './*', './helper/*', './ramaze/adapter.rb', './ramaze/request.rb', ]

files = Dir["#{spec_base}/**/*.rb"] +
        Dir["#{example_base}/**/spec/*.rb"]
ignores.each do |ignore|
  ignore_files = Dir["#{spec_base}/#{ignore}"]
  ignore_files.each do |ignore_file|
    files.delete File.expand_path(ignore_file)
  end
end

files.sort!

last = files.pop

COV_CMD = "rcov --aggregate coverage.data --%shtml -%s -x gem -x rack %s"

def sys(cmd)
 puts cmd
 system(cmd)
end

task :coverage => :clean do
  # these are the tests that can be run in parallel.
  # IMHO, ideally we should have
  # * 100% coverage of ramaze with pure tests
  # * 100% coverage with non-pure functional tests
  pure_specs =  Dir["#{snippets_base}/**/*.rb"].entries
  sys(COV_CMD % ["no-","t", pure_specs.join(' ')])

  files.each do |file|
    sys(COV_CMD % ["no-","t", file])
  end
  sys(COV_CMD % ["", "t", last] )

end
