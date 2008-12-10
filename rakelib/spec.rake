# Rake tasks that run the specs.

namespace :spec do

  SPEC_DIR  = File.dirname(__FILE__) + '/../spec'
  RSPEC_DIR = "#{SPEC_DIR}/rubyspec/1.8/"
  MSPEC_CMD = "#{SPEC_DIR}/mspec/bin/mspec"
  DEBUG     = "-T -d"   # flags to mspec to pass -d onto maglev

  desc "Run the continuous integration specs against MRI"
  task :mri do
    sh "#{MSPEC_CMD} -t ruby #{RSPEC_DIR}"
  end

  desc "Run the continuous integration specs (was passingpsecs) on MagLev"
  task :ci do
    sh "#{MSPEC_CMD} ci"
  end

  desc "Run the continuous integration specs on MagLev with debug"
  task :debugci do
    sh "#{MSPEC_CMD} ci #{DEBUG}"
  end

  desc "Run an mspec file: spec=<dir_or_file_name>"
  task :run do
    check_spec_file
    sh "#{MSPEC_CMD} #{ENV['spec']}"
  end

  desc "Debug an mspec file: spec=<dir_or_file_name>"
  task :debug do
    check_spec_file
    sh "#{MSPEC_CMD} #{DEBUG} #{ENV['spec']}"
  end

  desc "Run the named specs and tag the failing ones"
  task :tag do
    check_spec_file
    sh "#{MSPEC_CMD} tag -G fails #{ENV['spec']}"
  end
  def check_spec_file
    raise "No spec defined with: spec=..." unless ENV['spec']
    spec = ENV['spec']
    raise "Can't find file #{spec}" unless File.exists? spec
  end

end

# namespace :oldspec do

#   # ######################################################################
#   #                           TOPAZ COMMAND STRINGS
#   # ######################################################################
#   # The following +tc_*+ methods generate topaz command strings based on the
#   # parameters passed to them.

#   # Returns a topaz command string that loads the mspec library, sets the
#   # +DEBUG_SPEC+ flag per +debug+ parameter and runs the single +spec_file+.
#   def tc_mspec(spec_file, debug=false)
#     <<-END.margin
#     |output push spec.out
#     |run
#     |RubyContext load.
#     |RubyContext default globals at: #DEBUG_SPEC put: #{debug} .
#     |RubyContext requireFileNamed: 'mspec.rb'.
#     |RubyCompiler new evaluateString: '\\$formatter = DottedFormatter.new; \\$formatter.register'.
#     |RubyContext loadFileNamed: '#{spec_file}'.
#     |RubyCompiler new evaluateString: '\\$formatter.finish'
#     |%
#     |abort
#   END
#   end
# end
