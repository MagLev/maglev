# Rake tasks that run the specs.
#
# These tasks are used during early MagLev development, until MagLev can
# run the mspec infrastructure.

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

  desc "Run all passing specs (should be same as :ci, but isn't..."
  task :passing do
    sh "#{MSPEC_CMD} -G fails #{RSPEC_DIR}/core/array"
  end
end

namespace :oldspec do
  desc "Run the passing specs as defined in ../gss64bit_30/tests/rubytst/passingspecs.conf)"
  task :passing do
    run_topaz tc_run_passing_specs
    puts "Log files in log/spec*"
  end

  def check_spec_file
    raise "No spec defined with: spec=..." unless ENV['spec']
    spec = ENV['spec']
    raise "Can't find file #{spec}" unless File.exists? spec
  end

  # ######################################################################
  #                           TOPAZ COMMAND STRINGS
  # ######################################################################
  # The following +tc_*+ methods generate topaz command strings based on the
  # parameters passed to them.

  # Returns a topaz command string that loads the mspec library, sets the
  # +DEBUG_SPEC+ flag per +debug+ parameter and runs the single +spec_file+.
  def tc_mspec(spec_file, debug=false)
    <<-END.margin
    |output push spec.out
    |run
    |RubyContext load.
    |RubyContext default globals at: #DEBUG_SPEC put: #{debug} .
    |RubyContext requireFileNamed: 'mspec.rb'.
    |RubyCompiler new evaluateString: '\\$formatter = DottedFormatter.new; \\$formatter.register'.
    |RubyContext loadFileNamed: '#{spec_file}'.
    |RubyCompiler new evaluateString: '\\$formatter.finish'
    |%
    |abort
  END
  end

  # Returns a topaz command string that runs the set of passing specs
  # in src/test/vmunit.conf
  def tc_run_passing_specs
    <<-END.margin
    |inp #{"rakelib/passingspecs.inp"}
  END
  end
end
