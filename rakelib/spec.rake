# Rake tasks that run the specs.

namespace :spec do

  # NOTE: "-t m" handled in $MAGLEV_HOME/default.mspec
  PSPEC = "#{ENV['MAGLEV_HOME']}/spec/mspec/bin/mspec"

  desc "Run ci specs: there should be NO failures and NO errors."
  task :ci do
    rm_f "rubyspec_temp/*"
    sh "#{PSPEC} -V -G fails"
  end
end

