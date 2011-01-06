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

# namespace :spec do

#   SPEC_DIR  = File.dirname(__FILE__) + '/../spec'
#   RSPEC_DIR = "#{SPEC_DIR}/rubyspec/1.8/"
#   MSPEC_CMD = "#{SPEC_DIR}/mspec/bin/mspec"
#   DEBUG     = "-B #{MAGLEV_HOME}/debug.mspec -T -d"

#   desc "Run the continuous integration specs against MRI"
#   task :mri do
#     sh "#{MSPEC_CMD} -t ruby #{RSPEC_DIR}"
#   end

#   desc "Run the verbose continuous integration specs on MagLev"
#   task :civ do
#     sh "#{MSPEC_CMD} ci -V"
#   end

#   desc "Run the integration specs on MagLev"
#   task :ci do
#     sh "#{MSPEC_CMD} ci"
#   end

#   desc "Run mspec run --help"
#   task :help do
#     sh "#{MSPEC_CMD} run -t ruby --help"
#   end

#   desc "Run the continuous integration specs on MagLev with debug"
#   task :debugci do
#     sh "#{MSPEC_CMD} ci #{DEBUG}"
#   end

#   desc "Run an mspec file: spec=<dir_or_file_name>"
#   task :run do
#     check_spec_file
#     sh "#{MSPEC_CMD} -V #{ENV['spec']}"
#   end

#   desc "Run an mspec file with -G fails: spec=<dir_or_file_name>"
#   task :runnofail do
#     check_spec_file
#     sh "#{MSPEC_CMD} -G fails #{ENV['spec']}"
#   end

#   desc "Debug an mspec file: spec=<dir_or_file_name>"
#   task :debug do
#     check_spec_file
#     sh "#{MSPEC_CMD} #{DEBUG} #{ENV['spec']}"
#   end

#   desc "Run the named specs and tag the failing ones"
#   task :tag do
#     check_spec_file
#     sh "#{MSPEC_CMD} tag -G fails #{ENV['spec']}"
#   end

#   desc "Run specs currently tagged as fails, and untag ones now passing"
#   task :untag do
#     check_spec_file
#     sh "#{MSPEC_CMD} tag --del fails #{ENV['spec']}"
#   end

#   desc "List the specs currently tagged as fails for the dir"
#   task :lsfails do
#     spec = ENV['spec'] || RSPEC_DIR
#     sh "#{MSPEC_CMD} tag --list fails #{spec}"
#   end

#   def check_spec_file
#     raise "No spec defined with: spec=..." unless ENV['spec']
#     spec = ENV['spec']
#     raise "Can't find file #{spec}" unless File.exists? spec
#   end

# end
