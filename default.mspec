# -*-ruby-*-
#
# Config file for running RubySpecs / MSpec with MagLev
#
class MSpecScript

  # Instead of passing "-t m", this sets the ruby-under-test to maglev-ruby.
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'

  lang_files = ['spec/rubyspec/language',
                "^spec/rubyspec/language/block_spec.rb",
                "^spec/rubyspec/language/class_spec.rb",              # 26876
                "^spec/rubyspec/language/constants_spec.rb",
                "^spec/rubyspec/language/for_spec.rb",
                "^spec/rubyspec/language/proc_spec.rb",
                "^spec/rubyspec/language/singleton_class_spec.rb",
                "^spec/rubyspec/language/super_spec.rb",
                "^spec/rubyspec/language/yield_spec.rb"]

  core_files = ['spec/rubyspec/core',
                "^spec/rubyspec/core/argf/gets_spec.rb",              # Orpheus
                "^spec/rubyspec/core/argf/readline_spec.rb",          # Orpheus
                "^spec/rubyspec/core/argf/seek_spec.rb",
                "^spec/rubyspec/core/array/pack/b_spec.rb",           # 26876
                "^spec/rubyspec/core/array/pack/p_spec.rb",           # 26876
                "^spec/rubyspec/core/basicobject/basicobject_spec.rb",
                "^spec/rubyspec/core/basicobject/not_equal_spec.rb",
                "^spec/rubyspec/core/basicobject/not_spec.rb",
                "^spec/rubyspec/core/class/allocate_spec.rb",
                "^spec/rubyspec/core/class/dup_spec.rb",
                "^spec/rubyspec/core/enumerable/cycle_spec.rb",
                "^spec/rubyspec/core/file/flock_spec.rb",             # Grace
                "^spec/rubyspec/core/file/ftype_spec.rb",             # Orpheus
                "^spec/rubyspec/core/file/stat/ftype_spec.rb",        # Orpheus
                "^spec/rubyspec/core/file/utime_spec.rb",
                "^spec/rubyspec/core/fixnum/right_shift_spec.rb",
                "^spec/rubyspec/core/float/round_spec.rb",            # 26876
                "^spec/rubyspec/core/io/select_spec.rb",
                "^spec/rubyspec/core/kernel/catch_spec.rb",           # 26876
                "^spec/rubyspec/core/kernel/exec_spec.rb",
                "^spec/rubyspec/core/kernel/system_spec.rb",
                "^spec/rubyspec/core/marshal/dump_spec.rb",
                "^spec/rubyspec/core/numeric/step_spec.rb",           # 26876
                "^spec/rubyspec/core/marshal/load_spec.rb",
                "^spec/rubyspec/core/proc/arity_spec.rb",
                "^spec/rubyspec/core/process/detach_spec.rb",
                "^spec/rubyspec/core/process/wait2_spec.rb",
                "^spec/rubyspec/core/process/waitall_spec.rb",
                "^spec/rubyspec/core/process/waitall_spec.rb",        # Grace
                "^spec/rubyspec/core/process/wait_spec.rb",
                "^spec/rubyspec/core/range/step_spec.rb",             # 26876
                "^spec/rubyspec/core/string/modulo_spec.rb",
                "^spec/rubyspec/core/string/modulo_spec.rb",
                "^spec/rubyspec/core/string/multiply_spec.rb",
                "^spec/rubyspec/core/string/unpack/a_spec.rb",
                "^spec/rubyspec/core/string/unpack/p_spec.rb",        # 26876
                "^spec/rubyspec/core/string/unpack/u_spec.rb",        # 26876
                "^spec/rubyspec/core/systemexit/success_spec.rb",     # 26876
                "^spec/rubyspec/core/thread/alive_spec.rb",
                "^spec/rubyspec/core/thread/exit_spec.rb",
                "^spec/rubyspec/core/thread/inspect_spec.rb",
                "^spec/rubyspec/core/thread/kill_spec.rb",
                "^spec/rubyspec/core/thread/raise_spec.rb",
                "^spec/rubyspec/core/thread/run_spec.rb",
                "^spec/rubyspec/core/thread/status_spec.rb",
                "^spec/rubyspec/core/thread/stop_spec.rb",
                "^spec/rubyspec/core/thread/terminate_spec.rb",
                "^spec/rubyspec/core/thread/wakeup_spec.rb",
                "^spec/rubyspec/core/time/strftime_spec.rb"]          # 26876, Jupiter

  lib_files = ['spec/rubyspec/library',
               "^spec/rubyspec/library/complex/angle_spec.rb",
               "^spec/rubyspec/library/complex/arg_spec.rb",
               "^spec/rubyspec/library/complex/conj_spec.rb",
               "^spec/rubyspec/library/complex/conjugate_spec.rb",
               "^spec/rubyspec/library/complex/divide_spec.rb",
               "^spec/rubyspec/library/complex/equal_value_spec.rb",
               "^spec/rubyspec/library/complex/exponent_spec.rb",
               "^spec/rubyspec/library/complex/math/acos_spec.rb",    # Grace
               "^spec/rubyspec/library/complex/math/asin_spec.rb",    # Grace
               "^spec/rubyspec/library/complex/math/log10_spec.rb",   # Grace
               "^spec/rubyspec/library/complex/math/log_spec.rb",     # Grace
               "^spec/rubyspec/library/complex/math/sqrt_spec.rb",
               "^spec/rubyspec/library/complex/minus_spec.rb",
               "^spec/rubyspec/library/complex/modulo_spec.rb",
               "^spec/rubyspec/library/complex/multiply_spec.rb",
               "^spec/rubyspec/library/complex/numeric/angle_spec.rb",
               "^spec/rubyspec/library/complex/numeric/arg_spec.rb",
               "^spec/rubyspec/library/complex/plus_spec.rb",
               "^spec/rubyspec/library/etc/getgrgid_spec.rb",         # Orpheus, Grace
               "^spec/rubyspec/library/generator",
               "^spec/rubyspec/library/iconv",                        # Grace
               "^spec/rubyspec/library/matrix/I_spec.rb",             # 26876
               "^spec/rubyspec/library/matrix/clone_spec.rb",         # 26876
               "^spec/rubyspec/library/matrix/collect_spec.rb",       # 26876
               "^spec/rubyspec/library/matrix/column_vector_spec.rb", # 26876
               "^spec/rubyspec/library/matrix/columns_spec.rb",       # 26876
               "^spec/rubyspec/library/matrix/divide_spec.rb",        # 26876
               "^spec/rubyspec/library/matrix/identity_spec.rb",      # 26876
               "^spec/rubyspec/library/matrix/inspect_spec.rb",       # 26876
               "^spec/rubyspec/library/matrix/inv_spec.rb",           # 26876
               "^spec/rubyspec/library/matrix/inverse_spec.rb",       # 26876
               "^spec/rubyspec/library/matrix/map_spec.rb",           # 26876
               "^spec/rubyspec/library/matrix/minor_spec.rb",         # 26876
               "^spec/rubyspec/library/matrix/minus_spec.rb",         # 26876
               "^spec/rubyspec/library/matrix/multiply_spec.rb",      # 26876
               "^spec/rubyspec/library/matrix/plus_spec.rb",          # 26876
               "^spec/rubyspec/library/matrix/row_vector_spec.rb",    # 26876
               "^spec/rubyspec/library/matrix/t_spec.rb",             # 26876
               "^spec/rubyspec/library/matrix/transpose_spec.rb",     # 26876
               "^spec/rubyspec/library/matrix/unit_spec.rb",          # 26876
               "^spec/rubyspec/library/matrix/zero_spec.rb",          # 26876
               "^spec/rubyspec/library/net/http/httpheader/canonical_each_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/content_range_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_capitalized_name_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_capitalized_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_header_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_key_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_name_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/each_value_spec.rb",
               "^spec/rubyspec/library/net/http/httpheader/fetch_spec.rb",
               "^spec/rubyspec/library/net/http/http/request_spec.rb",
               "^spec/rubyspec/library/net/http/http/request_spec.rb",
               "^spec/rubyspec/library/net/http/httpresponse/body_spec.rb",
               "^spec/rubyspec/library/net/http/httpresponse/entity_spec.rb",
               "^spec/rubyspec/library/net/http/httpresponse/read_body_spec.rb",
               "^spec/rubyspec/library/net/http/httpresponse/read_new_spec.rb",
               "^spec/rubyspec/library/net/http/http/send_request_spec.rb",
               "^spec/rubyspec/library/net/http/http/send_request_spec.rb",
               "^spec/rubyspec/library/openstruct/element_set_spec.rb",
               "^spec/rubyspec/library/prime",
               "^spec/rubyspec/library/scanf",
               "^spec/rubyspec/library/socket",                       # Orpheus, Grace
               "^spec/rubyspec/library/syslog",
               "^spec/rubyspec/library/zlib/inflate/append_spec.rb",
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
