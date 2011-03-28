# -*-ruby-*-
#
# Config file for running RubySpecs / MSpec with MagLev
#
class MSpecScript

  # Instead of passing "-t m", this sets the ruby-under-test to maglev-ruby.
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'

  lang_files = ['spec/rubyspec/language',
                "^spec/rubyspec/language/super_spec.rb",
                "^spec/rubyspec/language/for_spec.rb",
                "^spec/rubyspec/language/block_spec.rb"]

  core_files = ['spec/rubyspec/core',
                "^spec/rubyspec/core/argf/seek_spec.rb",
            "^spec/rubyspec/core/basicobject",
            "^spec/rubyspec/core/fixnum/right_shift_spec.rb",
            "^spec/rubyspec/core/kernel/system_spec.rb",
                "^spec/rubyspec/core/kernel/exec_spec.rb",
                "^spec/rubyspec/core/process/wait_spec.rb",
                "^spec/rubyspec/core/process/wait2_spec.rb",
                "^spec/rubyspec/core/process/waitall_spec.rb",
                "^spec/rubyspec/core/string/modulo_spec.rb",
                "^spec/rubyspec/core/string/multiply_spec.rb",
                "^spec/rubyspec/core/string/valid_encoding_spec.rb",

                "^spec/rubyspec/core/string/unpack/a_spec.rb",
                "^spec/rubyspec/core/string/modulo_spec.rb",

                "^spec/rubyspec/core/thread/alive_spec.rb",
                "^spec/rubyspec/core/thread/exit_spec.rb",
                "^spec/rubyspec/core/thread/inspect_spec.rb",
                "^spec/rubyspec/core/thread/kill_spec.rb",
                "^spec/rubyspec/core/thread/raise_spec.rb",
                "^spec/rubyspec/core/thread/run_spec.rb",
                "^spec/rubyspec/core/thread/status_spec.rb",
                "^spec/rubyspec/core/thread/stop_spec.rb",
                "^spec/rubyspec/core/thread/terminate_spec.rb",
                "^spec/rubyspec/core/thread/wakeup_spec.rb"]

  lib_files = ['spec/rubyspec/library',
           "^spec/rubyspec/library/date/civil_spec.rb",
               "^spec/rubyspec/library/net/http",
               "^spec/rubyspec/library/generator",
               "^spec/rubyspec/library/prime",
               "^spec/rubyspec/library/socket/basicsocket/send_spec.rb",
               "^spec/rubyspec/library/socket/tcpsocket/open_spec.rb",
               "^spec/rubyspec/library/scanf",
               "^spec/rubyspec/library/syslog",
               "^spec/rubyspec/library/zlib/inflate/append_spec.rb",
               "^spec/rubyspec/library/zlib/inflate/inflate_spec.rb"]

  set :files, lang_files + core_files + lib_files

  # The set of substitutions to transform a spec filename into a tag
  # filename.  The transformations are applied, in the given sequence, to a
  # filename, yielding a tag file name.
  set :tags_patterns, [
    [%r(spec/rubyspec/), 'spec/tags/'],
    [/_spec.rb$/, '_tags.txt']
  ]
end
