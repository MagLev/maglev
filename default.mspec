# -*-ruby-*-
#
# Config file for running RubySpecs / MSpec with MagLev
#
class MSpecScript

  # Instead of passing "-t m", this sets the ruby-under-test to maglev-ruby.
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'

  lang_files = ['spec/rubyspec/language',
                "^spec/rubyspec/language/for_spec.rb",
                "^spec/rubyspec/language/block_spec.rb",
                "^spec/rubyspec/language/module_spec.rb",               # 2011-12-01
                "^spec/rubyspec/language/super_spec.rb",
                "^spec/rubyspec/language/predefined_spec.rb",
                "^spec/rubyspec/language/break_spec.rb",
                "^spec/rubyspec/language/predefined/data_spec.rb"]

  core_files = ['spec/rubyspec/core',
                "^spec/rubyspec/core/argf/gets_spec.rb",
                "^spec/rubyspec/core/argf/readline_spec.rb",
                "^spec/rubyspec/core/argf/seek_spec.rb",
                "^spec/rubyspec/core/array/pack/p_spec.rb",             # 2011-12-01
                "^spec/rubyspec/core/basicobject/basicobject_spec.rb",
                "^spec/rubyspec/core/basicobject/not_equal_spec.rb",
                "^spec/rubyspec/core/basicobject/not_spec.rb",
                "^spec/rubyspec/core/enumerator/enum_for_spec.rb",      # 2011-12-01
                "^spec/rubyspec/core/enumerator/next_spec.rb",          # 2011-12-01
                "^spec/rubyspec/core/enumerator/to_enum_spec.rb",       # 2011-12-01
                "^spec/rubyspec/core/fixnum/fdiv_spec.rb",              # 2011-12-01
                "^spec/rubyspec/core/fixnum/right_shift_spec.rb",
                "^spec/rubyspec/core/fixnum/uminus_spec.rb",            # 2011-12-01
                "^spec/rubyspec/core/float/abs_spec.rb",                # 2011-12-01
                "^spec/rubyspec/core/io/binmode_spec.rb",               # 2011-12-01
                "^spec/rubyspec/core/io/sysopen_spec.rb",               # 2011-12-01
                "^spec/rubyspec/core/kernel/eval_spec.rb",              # 2011-12-01
                "^spec/rubyspec/core/kernel/exec_spec.rb",
                "^spec/rubyspec/core/kernel/local_variables_spec.rb",   # 2011-12-01
                "^spec/rubyspec/core/kernel/method_missing_spec.rb",    # 2011-12-01
                "^spec/rubyspec/core/kernel/sprintf_spec.rb",           # 2011-12-01
                "^spec/rubyspec/core/kernel/system_spec.rb",
                "^spec/rubyspec/core/marshal/dump_spec.rb",             # OpenSSL::X509::Name
                "^spec/rubyspec/core/marshal/load_spec.rb",             # OpenSSL::X509::Name
                "^spec/rubyspec/core/module/attr_accessor_spec.rb",     # 2011-12-01
                "^spec/rubyspec/core/module/attr_reader_spec.rb",       # 2011-12-01
                "^spec/rubyspec/core/module/attr_spec.rb",              # 2011-12-01
                "^spec/rubyspec/core/module/attr_writer_spec.rb",       # 2011-12-01
                "^spec/rubyspec/core/numeric/fdiv_spec.rb",             # 2011-12-01
                "^spec/rubyspec/core/numeric/step_spec.rb",             # 26876
                "^spec/rubyspec/core/numeric/uminus_spec.rb",           # 2011-12-01
                "^spec/rubyspec/core/object/instance_exec_spec.rb",     # 2011-12-01
                "^spec/rubyspec/core/process/exit_spec.rb",             # 2011-12-01
                "^spec/rubyspec/core/process/wait2_spec.rb",
                "^spec/rubyspec/core/process/wait_spec.rb",
                "^spec/rubyspec/core/process/waitall_spec.rb",
                "^spec/rubyspec/core/range/step_spec.rb",               # 2011-12-07
                "^spec/rubyspec/core/string/modulo_spec.rb",
                "^spec/rubyspec/core/string/unpack/a_spec.rb",
                "^spec/rubyspec/core/string/unpack/p_spec.rb",          # 2011-12-01
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
               "^spec/rubyspec/library/bigdecimal/round_spec.rb",       # 2011-12-01
               "^spec/rubyspec/library/complex/divide_spec.rb",
               "^spec/rubyspec/library/complex/exponent_spec.rb",
               "^spec/rubyspec/library/complex/float/angle_spec.rb",    # 2011-12-01
               "^spec/rubyspec/library/complex/float/arg_spec.rb",      # 2011-12-01
               "^spec/rubyspec/library/complex/minus_spec.rb",
               "^spec/rubyspec/library/complex/modulo_spec.rb",
               "^spec/rubyspec/library/complex/multiply_spec.rb",
               "^spec/rubyspec/library/complex/plus_spec.rb",
               "^spec/rubyspec/library/enumerator/enum_for_spec.rb",    # 2011-12-01
               "^spec/rubyspec/library/enumerator/next_spec.rb",        # 2011-12-01
               "^spec/rubyspec/library/enumerator/to_enum_spec.rb",     # 2011-12-01
               "^spec/rubyspec/library/iconv/iconv_spec.rb",            # 2011-12-01
               "^spec/rubyspec/library/net/http/http/request_spec.rb",  # 2011-12-01
               "^spec/rubyspec/library/net/http/http/send_request_spec.rb",  # 2011-12-01
               "^spec/rubyspec/library/prime",
               "^spec/rubyspec/library/scanf",
               "^spec/rubyspec/library/socket",                         # Orpheus, Grace
               "^spec/rubyspec/library/syslog",
               "^spec/rubyspec/library/zlib/gzipwriter/write_spec.rb",  # 2011-12-01
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
