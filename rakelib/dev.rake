# Rake tasks for MagLev core developers.
#
# These tasks depend on the conventions used by the GemStone MagLev
# engineering team.  The following assumptions are made:
#
#  ENV['MAGLEV_HOME'] is the path to the top level development directory.
#  Everything is done here.  Typical value is ~/MagLev/maglev-git.
#
#  ENV['PREVIOUS'], ENV['CURRENT'] are the directories holding the previous
#  and current maglev product-nnn.tgz and *.mcz files.  These variables are
#  only used for the "dev:previous" and "dev:current" targets to find the
#  tgz and mcz files to create the maglev product from.

namespace :dev do
  require 'rakelib/dev.rb'

  desc "Stop current server and install MagLev build from ENV['PREVIOUS']"
  task :previous => :ensure_stopped do
    previous = ENV['PREVIOUS'] || '/Users/pmclain/MagLev/PREVIOUS' # TODO rm pmclain
    raise ArgumentError, "No PREVIOUS directory found: '#{previous}'" unless
      File.directory? previous

    ensure_std_directories
    rm_current_gemstone
    untar_product_to_gemstone File.join(previous, 'product.tgz')
    puts "=== Start GemStone Server"
    Rake::Task['gs:start'].invoke
    Rake::Task['gs:status'].invoke
  end

  desc "Make sure the gemstone server is stopped."
  task :ensure_stopped do
    # We can't depend on the gs:stop task, as that depends on there being a
    # gemstone/ directory, but it might not exist yet.  But if there *is* a
    # gemstone dir, then we may as well ensure there is no instance
    # running.
    if File.directory?(GEMSTONE)
      puts "=== Stopping the GemStone Server"
      Rake::Task['gs:stop'].invoke
    else
      puts "=== No GemStone Server running"
    end
  end







#   desc "run mspecs known to work"
#   task :mspecs => :initenv do
#     prologue = <<'END_PROLOGUE'
# run
# RubyContext load.
# RubyContext requireFileNamed: 'mspec.rb'.
# RubyCompiler new evaluateString: '\$formatter = DottedFormatter.new; \$formatter.register'.
# END_PROLOGUE
#     finale = <<'END_FINALE'
# RubyCompiler new evaluateString: '\$formatter.finish'
# %
# exit
# END_FINALE

#     load_files = ''
#     %w(concat_spec.rb).each do |file_name|
#       load_files << "RubyContext loadFileNamed: '#{Dir.pwd}/rubyspec/1.8/core/array/#{file_name}'.\n"
#     end
#     sh %{ #{TOPAZ_CMD} #{prologue}#{load_files}#{finale} }
#   end

#   task :mspecss => :initenv do
#     load_files = ''
#     %w(concat_spec.rb).each do |file_name|
#       load_files << "RubyContext loadFileNamed: '#{Dir.pwd}/rubyspec/1.8/core/array/#{file_name}'."
#     end
#     puts "LOAD FILES:"
#     puts load_files
#     sh %{
#       #{TOPAZ_CMD} <<'EOF'
# run
# RubyContext load.
# RubyContext requireFileNamed: 'mspec.rb'.
# RubyCompiler new evaluateString: '\$formatter = DottedFormatter.new; \$formatter.register'.
# #{load_files}
# RubyCompiler new evaluateString: '\$formatter.finish'
# %
# exit
# EOF
#     }
#
#  end
end
