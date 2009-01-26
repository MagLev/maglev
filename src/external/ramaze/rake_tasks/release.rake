def latest_release_id
  require 'open-uri'
  require 'hpricot'

  url = "http://rubyforge.org/frs/?group_id=3034"
  doc = Hpricot(open(url))
  a = (doc/:a).find{|a| a[:href] =~ /release_id/}

  version = a.inner_html
  release_id = Hash[*a[:href].split('?').last.split('=').flatten]['release_id']
end

def update_version_rb(version)
  File.open('lib/ramaze/version.rb', 'w+') do |v|
    v.puts COPYRIGHT
    v.puts
    v.puts "module Ramaze"
    v.puts "  VERSION = #{version.to_s.dump}"
    v.puts "end"
  end
end

def prepare_package(version)
  update_version_rb(version)
  spec = generate_gemspec(version)

  Rake::GemPackageTask.new(spec) do |pkg|
    pkg.need_tar = true
    pkg.need_zip = true
  end

  Rake::Task['package'].invoke
end

namespace :release do
  desc 'Nightly release to gems.ramaze.net'
  task 'nightly' do
    prepare_package(version_today)

    location = 'web/gems/'

    sh "scp pkg/*.{gem,tgz,zip} ramaze@ramaze.net:#{location}"
    sh "ssh ramaze@ramaze.net '
source ~/.zsh/export.sh
cd #{location}
gem generate_index'"
  end

  desc 'Monthly release to rubyforge'
  task 'monthly' do
    prepare_package(v = version_month)

    sh "rubyforge login"
    sh "rubyforge add_release ramaze ramaze #{v} pkg/ramaze-#{v}.gem"

    release_id = latest_release_id

    files = Dir['pkg/*'].reject{|f| File.directory?(f) or f =~ /\.gem$/ }
    files.each do |file|
      sh "rubyforge add_file ramaze ramaze #{release_id} '#{file}'"
    end
  end

  desc 'Prepare and push gemspec to github'
  task 'gemspec' => 'gemspec-prepare' do
    sh "git commit -m 'Update ramaze.gemspec' ramaze.gemspec"
    sh 'git fetch'
    sh 'git rebase origin/master'
    sh 'git push'
  end

  desc 'Prepare gemspec for push to github'
  task 'gemspec-prepare' do
    update_gemspec(generate_gemspec(version_month))
  end
end
