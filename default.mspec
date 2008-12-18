# -*-ruby-*-
# Default RubySpec files for MagLev.

class MSpecScript
  # Define the continuous integration specs (the ones known to pass)
  DIR = File.dirname(__FILE__)
  set :ci_files, [
#    DIR + '/spec/rubyspec/1.8/language/alias_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/and_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/array_spec.rb',
#  DIR + '/spec/rubyspec/1.8/language/block_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/break_spec.rb',
# #     DIR + '/spec/rubyspec/1.8/language/case_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/catch_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/class_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/constants_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/def_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/defined_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/else_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/ensure_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/execution_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/file_spec.rb',
#    DIR + '/spec/rubyspec/1.8/language/for_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/hash_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/if_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/line_spec.rb',
    DIR + '/spec/rubyspec/1.8/language/loop_spec.rb',
    # TODO: metaclass_spec blows up in C
#   DIR + '/spec/rubyspec/1.8/language/metaclass_spec.rb',

#    DIR + '/spec/rubyspec/1.8/language/method_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/module_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/next_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/not_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/numbers_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/or_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/order_spec.rb',
#     # TODO: precedence_spec blows up big time
# #     DIR + '/spec/rubyspec/1.8/language/precedence_spec.rb',
#     # TODO: predefined cant find stringio
# #  DIR + '/spec/rubyspec/1.8/language/predefined_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/private_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/raise_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/redo_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/regexp_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/rescue_spec.rb',
# #    DIR + '/spec/rubyspec/1.8/language/retry_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/return_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/string_spec.rb',
# #     DIR + '/spec/rubyspec/1.8/language/super_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/symbol_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/throw_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/undef_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/unless_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/until_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/variables_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/while_spec.rb',
#     DIR + '/spec/rubyspec/1.8/language/yield_spec.rb',









#    DIR + '/spec/rubyspec/1.8/core/argf',
    DIR + '/spec/rubyspec/1.8/core/array',
    DIR + '/spec/rubyspec/1.8/core/bignum',
    DIR + '/spec/rubyspec/1.8/core/binding',
    DIR + '/spec/rubyspec/1.8/core/class',
    DIR + '/spec/rubyspec/1.8/core/comparable',
#    DIR + '/spec/rubyspec/1.8/core/continuation',
#    DIR + '/spec/rubyspec/1.8/core/dir',
    DIR + '/spec/rubyspec/1.8/core/enumerable',
    DIR + '/spec/rubyspec/1.8/core/env',
    DIR + '/spec/rubyspec/1.8/core/exception',
    DIR + '/spec/rubyspec/1.8/core/false',
    DIR + '/spec/rubyspec/1.8/core/file',
#    DIR + '/spec/rubyspec/1.8/core/filetest',
    DIR + '/spec/rubyspec/1.8/core/fixnum',
    DIR + '/spec/rubyspec/1.8/core/float',
    DIR + '/spec/rubyspec/1.8/core/gc',
    DIR + '/spec/rubyspec/1.8/core/hash',
    DIR + '/spec/rubyspec/1.8/core/integer',
    #    DIR + '/spec/rubyspec/1.8/core/io',

    # The case_compare_spec fixtures try to override equal?, which MagLev
    # doesn't allow.  As a result, MagLev can't even load that file w/o an
    # error (i.e., tagging it as fails still generates an error).  So, for
    # the kernel specs, we turn all of them on except for case_compare_spec...
    #DIR + '/spec/rubyspec/1.8/core/kernel',


    DIR + '/spec/rubyspec/1.8/core/kernel/Array_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/Float_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/Integer_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/String_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/__id___spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/__send___spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/abort_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/at_exit_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/autoload_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/backtick_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/binding_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/block_given_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/callcc_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/caller_spec.rb',

    # See note above.
#    DIR + '/spec/rubyspec/1.8/core/kernel/case_compare_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/catch_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/chomp_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/chop_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/class_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/clone_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/display_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/dup_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/eql_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/equal_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/equal_value_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/eval_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/exec_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/exit_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/extend_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/fail_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/fixtures',
    DIR + '/spec/rubyspec/1.8/core/kernel/fork_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/format_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/freeze_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/frozen_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/getc_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/gets_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/global_variables_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/gsub_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/hash_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/id_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/initialize_copy_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/inspect_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/instance_eval_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/instance_of_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/instance_variable_defined_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/instance_variable_get_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/instance_variable_set_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/instance_variables_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/is_a_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/iterator_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/kind_of_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/lambda_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/load_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/local_variables_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/loop_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/match_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/method_missing_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/method_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/methods_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/nil_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/object_id_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/open_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/p_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/print_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/printf_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/private_methods_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/proc_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/protected_methods_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/public_methods_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/putc_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/puts_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/raise_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/rand_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/readline_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/readlines_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/remove_instance_variable_spec.rb',

    # TODO: Depends on FileUtils
#    DIR + '/spec/rubyspec/1.8/core/kernel/require_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/respond_to_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/scan_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/select_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/send_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/set_trace_func_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/shared',
    DIR + '/spec/rubyspec/1.8/core/kernel/singleton_method_added_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/singleton_method_removed_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/singleton_method_undefined_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/singleton_methods_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/sleep_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/split_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/sprintf_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/srand_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/sub_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/syscall_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/system_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/taint_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/tainted_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/test_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/throw_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/to_a_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/to_s_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/trace_var_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/trap_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/type_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/untaint_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/untrace_var_spec.rb',
    DIR + '/spec/rubyspec/1.8/core/kernel/warn_spec.rb',


    DIR + '/spec/rubyspec/1.8/core/marshal',
    DIR + '/spec/rubyspec/1.8/core/matchdata',
    DIR + '/spec/rubyspec/1.8/core/math',
    DIR + '/spec/rubyspec/1.8/core/method',
    DIR + '/spec/rubyspec/1.8/core/module',
    DIR + '/spec/rubyspec/1.8/core/nil',
    DIR + '/spec/rubyspec/1.8/core/numeric',
    DIR + '/spec/rubyspec/1.8/core/object',
#    DIR + '/spec/rubyspec/1.8/core/objectspace',
    DIR + '/spec/rubyspec/1.8/core/precision',

    #  TODO: core/proc/arity_spec.rb has a parse error which blows up the whole rest
    #  of that test.  When the parse error is fixed, then uncomment and tag core/proc.
#    DIR + '/spec/rubyspec/1.8/core/proc',


#    DIR + '/spec/rubyspec/1.8/core/process',

    # TODO: range may cause topaz to infinite loop.  It takes an awful long time...
#    DIR + '/spec/rubyspec/1.8/core/range',


    DIR + '/spec/rubyspec/1.8/core/regexp',
    DIR + '/spec/rubyspec/1.8/core/signal',
    DIR + '/spec/rubyspec/1.8/core/string',
    DIR + '/spec/rubyspec/1.8/core/struct',
    DIR + '/spec/rubyspec/1.8/core/symbol',
    DIR + '/spec/rubyspec/1.8/core/systemexit',
#    DIR + '/spec/rubyspec/1.8/core/thread',
#    DIR + '/spec/rubyspec/1.8/core/threadgroup',
    DIR + '/spec/rubyspec/1.8/core/time',
    DIR + '/spec/rubyspec/1.8/core/true'  # Need to add comma...
#    DIR + '/spec/rubyspec/1.8/core/unboundmethod'
  ]

  # The set of substitutions to transform a spec filename into a tag
  # filename.  The transformations are applied, in the given sequence, to a
  # filename, yielding a tag file name.
  set :tags_patterns, [
    [%r(spec/rubyspec/), 'spec/frozen/'],
    [%r(spec/), 'spec/tags/'],
    [/_spec.rb$/, '_tags.txt']
  ]

  # By default, run maglev
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'
end
