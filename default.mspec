# -*-ruby-*-
#
# Config file for running RubySpecs / MSpec with MagLev
#
class MSpecScript

  # Instead of passing "-t m", this sets the ruby-under-test to maglev-ruby.
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'

  lang_files = ['spec/rubyspec/language',
                "^spec/rubyspec/language/array_spec.rb",
                "^spec/rubyspec/language/case_spec.rb",
                "^spec/rubyspec/language/literal_lambda_spec.rb",
                "^spec/rubyspec/language/for_spec.rb",
                "^spec/rubyspec/language/block_spec.rb",
                "^spec/rubyspec/language/match_spec.rb",
                "^spec/rubyspec/language/send_spec.rb",
                "^spec/rubyspec/language/super_spec.rb",
                "^spec/rubyspec/language/symbol_spec.rb",
                "^spec/rubyspec/language/predefined_spec.rb",
                "^spec/rubyspec/language/break_spec.rb",
                "^spec/rubyspec/language/predefined/data_spec.rb",
                "^spec/rubyspec/language/variables_spec.rb"]

  core_files = ['spec/rubyspec/core',
                "^spec/rubyspec/core/argf/gets_spec.rb",
                "^spec/rubyspec/core/argf/readline_spec.rb",
                "^spec/rubyspec/core/argf/seek_spec.rb",
                "^spec/rubyspec/core/basicobject/",
                "^spec/rubyspec/core/fixnum/right_shift_spec.rb",
                "^spec/rubyspec/core/io/ungetc_spec.rb",
                "^spec/rubyspec/core/kernel/eval_spec.rb",              # 2011-12-01
                "^spec/rubyspec/core/kernel/exec_spec.rb",
                "^spec/rubyspec/core/kernel/system_spec.rb",
                "^spec/rubyspec/core/kernel/define_singleton_method_spec.rb",
                "^spec/rubyspec/core/method/parameters_spec.rb",
                "^spec/rubyspec/core/module/name_spec.rb",
                "^spec/rubyspec/core/module/define_method_spec.rb",
                "^spec/rubyspec/core/numeric/to_c_spec.rb",
                "^spec/rubyspec/core/proc/arity_spec.rb",
                "^spec/rubyspec/core/proc/parameters_spec.rb",
                "^spec/rubyspec/core/process/wait2_spec.rb",
                "^spec/rubyspec/core/process/wait_spec.rb",
                "^spec/rubyspec/core/process/waitall_spec.rb",
                "^spec/rubyspec/core/string/modulo_spec.rb",
                "^spec/rubyspec/core/string/unpack/a_spec.rb",
                "^spec/rubyspec/core/symbol/encoding_spec.rb",
                "^spec/rubyspec/core/symbol/length_spec.rb",
                "^spec/rubyspec/core/symbol/size_spec.rb",
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
               "^spec/rubyspec/library/erb/new_spec.rb",
               "^spec/rubyspec/library/complex/divide_spec.rb",
               "^spec/rubyspec/library/complex/exponent_spec.rb",
               "^spec/rubyspec/library/complex/minus_spec.rb",
               "^spec/rubyspec/library/complex/modulo_spec.rb",
               "^spec/rubyspec/library/complex/multiply_spec.rb",
               "^spec/rubyspec/library/complex/plus_spec.rb",
               "^spec/rubyspec/library/uri/parser",
               "^spec/rubyspec/library/prime",
               "^spec/rubyspec/library/scanf",
               "^spec/rubyspec/library/socket",                         # Orpheus, Grace
               "^spec/rubyspec/library/syslog",
               "^spec/rubyspec/library/zlib/inflate/append_spec.rb",
               "^spec/rubyspec/library/zlib/inflate/inflate_spec.rb"]

  set :files, lang_files + core_files + lib_files

  MSpec.enable_feature :fiber_library
  MSpec.enable_feature :continuation_library

  # The set of substitutions to transform a spec filename into a tag
  # filename.  The transformations are applied, in the given sequence, to a
  # filename, yielding a tag file name.
  set :tags_patterns, [
    [%r(spec/rubyspec/), 'spec/tags/'],
    [/_spec.rb$/, '_tags.txt']
  ]
end
