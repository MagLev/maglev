# -*-ruby-*-
#
# Config file for running RubySpecs / MSpec with MagLev
class MSpecScript

  # Instead of passing "-t m", this sets the ruby-under-test to maglev-ruby.
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'

  # Set the default list of files
  files = ['spec/rubyspec/language', 'spec/rubyspec/core']

  # The following files either hang or break before specs run
  files << "^spec/rubyspec/language/super_spec.rb"
  files << "^spec/rubyspec/language/for_spec.rb"
  files << "^spec/rubyspec/language/block_spec.rb"

  files << "^spec/rubyspec/core/argf/seek_spec.rb"
  files << "^spec/rubyspec/core/kernel/exec_spec.rb"
  files << "^spec/rubyspec/core/process/wait_spec.rb"
  files << "^spec/rubyspec/core/process/wait2_spec.rb"
  files << "^spec/rubyspec/core/process/waitall_spec.rb"
  files << "^spec/rubyspec/core/string/modulo_spec.rb"
  files << "^spec/rubyspec/core/string/multiply_spec.rb"
  files << "^spec/rubyspec/core/string/valid_encoding_spec.rb"
  files << "^spec/rubyspec/core/thread/alive_spec.rb"
  files << "^spec/rubyspec/core/thread/exit_spec.rb"
  files << "^spec/rubyspec/core/thread/inspect_spec.rb"
  files << "^spec/rubyspec/core/thread/kill_spec.rb"
  files << "^spec/rubyspec/core/thread/raise_spec.rb"
  files << "^spec/rubyspec/core/thread/run_spec.rb"
  files << "^spec/rubyspec/core/thread/status_spec.rb"
  files << "^spec/rubyspec/core/thread/stop_spec.rb"
  files << "^spec/rubyspec/core/thread/terminate_spec.rb"
  files << "^spec/rubyspec/core/thread/wakeup_spec.rb"

  set :files, files

  # The set of substitutions to transform a spec filename into a tag
  # filename.  The transformations are applied, in the given sequence, to a
  # filename, yielding a tag file name.
  set :tags_patterns, [ [%r(spec/), 'spec/tags/'] ]

  puts "FILES: #{files.inspect}"
end
