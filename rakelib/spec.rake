# Rake tasks that run the specs.

namespace :spec do

  # NOTE: "-t m" handled in $MAGLEV_HOME/default.mspec
  # Running all of the specs in one VM requires bigger tmp obj size
  PSPEC = %{MAGLEV_OPTS="--tocsz 1000000 $MAGLEV_OPTS" #{ENV['MAGLEV_HOME']}/spec/mspec/bin/mspec }

  desc "Run one rubyspec file: rake spec:run[spec/rubyspec/.../foo_spec.rb]"
  task :run, :spec do |t, args|
    check_spec_file(args.spec)
    sh "#{PSPEC} -V #{args.spec}"
  end

  desc "Run ci specs: there should be NO failures and NO errors."
  task :ci do
    rm_f "rubyspec_temp/*"
    sh "#{PSPEC} -V -G fails"
  end

  desc "Retag the ci files (works only with hacked mspec-tag.rb)"
  task :retag do
    sh "#{PSPEC} tag -G fails"
  end

  desc "Run failing specs and untag ones that now pass (works only with hacked mspec-tag.rb)"
  task :untag do
    sh "#{PSPEC} tag --del fails"
  end

  desc "Run the named specs and tag the failing ones"
  task :tag, :file do |t, args|
    sh "#{PSPEC} tag -G fails #{args.file}"
  end

  def check_spec_file(f)
    raise "No spec defined with: spec=..." unless f
    raise "Can't find file #{f}" unless File.exists? f
  end
  
end

