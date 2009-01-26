begin
  require 'rubygems'
rescue LoadError
end

require 'rake'
require 'rake/clean'
require 'rake/packagetask'
require 'rake/gempackagetask'
require 'rake/rdoctask'

require 'pp'
require 'date'
require 'time'
require 'fileutils'

include FileUtils

$:.unshift File.join(File.dirname(__FILE__), "lib")

require 'ramaze/version'
load 'rake_tasks/conf.rake'
load 'rake_tasks/maintenance.rake'
load 'rake_tasks/spec.rake'
load 'rake_tasks/coverage.rake'
load 'rake_tasks/release.rake'
load 'rake_tasks/git.rake'
load 'rake_tasks/gem.rake'
load 'rake_tasks/metric.rake'

task :default => ['spec']
task :test => ['spec']
# task :package => ['jquery']

desc 'download latest jquery and put in /lib/proto/public/js/jquery.js'
task :jquery do
  require 'open-uri'
  $stdout.sync = true

  File.open('lib/proto/public/js/jquery.js', 'w+') do |jquery|
    remote = open('http://code.jquery.com/jquery-latest.js')
    print "openend remote side, copying..."
    while chunk = remote.read(4096)
      print '.'
      jquery.write(chunk)
    end
    puts " done."
  end
end

desc 'Check gemspec file list against real file list'
task :check_gemspec do
  base_dir = File.expand_path(File.dirname(__FILE__))
  gemspec_files = eval(File.read(File.join(base_dir,'ramaze.gemspec'))).files.map{|file|File.join(base_dir,file)}

  deleted_files = gemspec_files.select{|file|!File.exists?(file)}.map{|file|file[(base_dir.size+1)..-1]}

  added_files = Dir.glob(File.join(File.dirname(__FILE__),'**/*')).select do |file|
    !gemspec_files.index(file)
  end.map do |file|
    file[(base_dir.size+1)..-1]
  end.select do |file|
    !( file == 'pkg' or file =~ /.*\.gem/ )
  end

  unless deleted_files.empty?
    puts "The following files appear in the gemspec but cannot be found:"
    deleted_files.each do |file|
      puts "\t#{file}"
    end
  end
  unless added_files.empty?
    puts "The following files exist, but cannot be found in the gemspec:"
    added_files.each do |file|
      puts "\t#{file}"
    end
  end
end

task :rcov_dir do
  mkdir_p 'doc/output/tools/rcov/'
end

desc "Generate HTML coverage report"
task :rcov_summary => :rcov_dir do
  `rcov --version`
  raise LoadError, "Please `gem install rcov` first" if $?.exitstatus == 127
  raise "Run `rake coverage` to generate coverage data first" unless File.exists? 'coverage.data'
  sh "rcov --aggregate coverage.data -o doc/output/tools/rcov/"
end

desc "generate rdoc"
task :rdoc => [:clean] do
  sh "rdoc #{(RDOC_OPTS + RDOC_FILES).join(' ')}"
end

desc "generate improved allison-rdoc"
task :allison => :clean do
  opts = RDOC_OPTS
  path = `allison --path`.strip
  raise LoadError, "Please `gem install allison` first" if $?.exitstatus == 127
  opts << %W[--template '#{path}']
  sh "rdoc #{(RDOC_OPTS + RDOC_FILES).join(' ')}"
end

desc "create bzip2 and tarball"
task :distribute => :gem do
  sh "rm -rf pkg/ramaze-#{VERS}"
  sh "mkdir -p pkg/ramaze-#{VERS}"
  sh "cp -r {bin,doc,lib,examples,spec,Rakefile,README.markdown,rake_tasks} pkg/ramaze-#{VERS}/"

  Dir.chdir('pkg') do |pwd|
    sh "tar -zcvf ramaze-#{VERS}.tar.gz ramaze-#{VERS}"
    sh "tar -jcvf ramaze-#{VERS}.tar.bz2 ramaze-#{VERS}"
  end

  sh "rm -rf pkg/ramaze-#{VERS}"
end

desc "show a todolist from all the TODO tags in the source"
task :todo do
  files = Dir[File.join(BASEDIR, '{lib,spec}', '**/*.rb')]

  files.each do |file|
    lastline = todo = comment = long_comment = false

    File.readlines(file).each_with_index do |line, lineno|
      lineno += 1
      comment = line =~ /^\s*?#.*?$/
      long_comment = line =~ /^=begin/
      long_comment = line =~ /^=end/
      todo = true if line =~ /TODO/ and (long_comment or comment)
      todo = false if line.gsub('#', '').strip.empty?
      todo = false unless comment or long_comment
      if todo
        unless lastline and lastline + 1 == lineno
          puts
          puts "vim #{file} +#{lineno}"
        end

        l = line.strip.gsub(/^#\s*/, '')
        print '  ' unless l =~ /^-/
        puts l
        lastline = lineno
      end
    end
  end
end

desc "opens a simple readline that makes making requests easier"
task 'request' do
  ARGV.clear
  require 'open-uri'
  require 'pp'

  loop do
    print 'do request? [enter] '
    gets
    begin
      pp open('http://localhost:7000/xxx').read
    rescue Object => ex
      puts ex
    end
  end
end

desc 'listing of available traits per class/module'
task 'traits' do
  nodes = Hash.new{|h,k| h[k] = []}
  Dir['lib/**/*.rb'].each do |file|
    content = File.read(file)
    traits = content.grep(/^\s*trait\s*:/)
    traits.each do |trait|
      space = content[0..content.index(trait)].scan(/^\s*(?:class|module)\s+(.*)$/)
      space = space.flatten.join('::')
      nodes[space] << trait.strip
    end
  end

  nodes.each do |space, traits|
    puts space
    traits.each do |trait|
      print '  ', trait, "\n"
    end
    puts
  end
end

desc "Update doc/CHANGELOG"
task 'doc/CHANGELOG' do
  File.open('doc/CHANGELOG', 'w+') do |f|
    f.puts `git log`
  end
end

desc "#{README} to doc/README.html"
task 'doc/README.html' => [README] do
  sh "maruku #{README}"
  mv 'README.html', 'doc/README.html'
end

desc "Compile the #{README} from the parts of doc/readme"
task README do
  require 'enumerator'

  chapters = [
    'About Ramaze',         'introduction',
    'Features Overview',    'features',
    'Basic Principles',     'principles',
    'Installation',         'installing',
    'Getting Started',      'getting_started',
    'A couple of Examples', 'examples',
    'How to find Help',     'getting_help',
    'Appendix',             'appendix',
    'And thanks to...',     'thanks',
  ]

  File.open(README, 'w+') do |readme|
    readme.puts COPYRIGHT.map{|l| l[1..-1]}, ''

    chapters.each_slice(2) do |title, file|
      file = File.join('doc', 'readme_chunks', "#{file}.txt")
      chapter = File.read(file)
      readme.puts "# #{title}", '', chapter
      readme.puts '', '' unless title == chapters[-2]
    end
  end
end
