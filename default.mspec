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
#    DIR + '/spec/rubyspec/1.8/core/kernel',
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

    # TODO: string generates a SIGSEGV (haven't tracked down which one yet...)
#    DIR + '/spec/rubyspec/1.8/core/string',

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
