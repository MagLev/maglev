# Rake tasks that run the specs.

namespace :spec do

  # NOTE: "-t m" handled in $MAGLEV_HOME/default.mspec
  PSPEC = "#{ENV['MAGLEV_HOME']}/spec/mspec/bin/mspec"

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

  def check_spec_file(f)
    raise "No spec defined with: spec=..." unless f
    raise "Can't find file #{f}" unless File.exists? f
  end
end

