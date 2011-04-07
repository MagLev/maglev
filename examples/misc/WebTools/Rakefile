task :default => :run

MH = ENV['MAGLEV_HOME']

desc "Run the WebTools Sinatra App"
task :run do
  sh "#{MH}/bin/rackup -Ilib config.ru"
end

desc "Commit an interesting example class, AValidPerson, to the repository"
task :meta do
  sh "maglev-ruby -Mcommit meta_demo.rb"
end

desc "Run the Smalltalk WebTools code from ruby."
task :smalltalk do
  file = "#{MH}/gemstone/examples/www/install.tpz"
  raise "Can't find #{file}" unless File.exists? file
  cd(MH) { sh "rake maglev:input_file[#{file}]" }

  sh "maglev-ruby -d lib/webtools/smalltalk_tools.rb"
end

task :test do
  FileList['tests/test_*'].each do |file|
    sh "maglev-ruby -rubygems -Ilib #{file}"
  end
end

desc "Add some objects to Maglev::PERSISTENT_ROOT"
task :demodata do
  sh "maglev-ruby -Mcommit demo_data.rb"
end
