# -*-ruby-*-
# Default RubySpec files for MagLev.

class MSpecScript
  # Define the continuous integration specs (the ones known to pass)
  DIR = File.dirname(__FILE__)
  set :ci_files, [
    ###################
    #### LANGUAGE SPECS
    ###################
    DIR + '/spec/rubyspec/1.8/language',

    '^' + DIR + '/spec/rubyspec/1.8/language/else_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/loop_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/not_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/private_spec.rb',
    # return_spec is suspicious...
    '^' + DIR + '/spec/rubyspec/1.8/language/return_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/undef_spec.rb',
    # variables_spec has an error???
    #DIR + '/spec/rubyspec/1.8/language/variables_spec.rb',

    '^' + DIR + '/spec/rubyspec/1.8/language/alias_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/block_spec.rb',
    # syntax error, unexpected kELSE, expecting kWHEN
    '^' + DIR + '/spec/rubyspec/1.8/language/case_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/def_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/for_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/language/metaclass_spec.rb',

    #  topaz 1> ......eval:3: both block arg and actual block given
    #  ..........eval:3: syntax error, unexpected tINTEGER, expecting kDO
    #  or '{' or '('
    #  f 4, f 5, 6
    #          ^
    '^' + DIR + '/spec/rubyspec/1.8/language/method_spec.rb',

    #  topaz 1> ...eval:3: no .<digit> floating literal anymore; put 0 before dot
    #  eval:3: syntax error, unexpected '.'
    #  eval:3: no .<digit> floating literal anymore; put 0 before dot
    #  eval:3: syntax error, unexpected '.'
    '^' + DIR + '/spec/rubyspec/1.8/language/numbers_spec.rb',

    # TODO: precedence_spec blows up with almost OOM error and chews up all
    # CPU
    '^' + DIR + '/spec/rubyspec/1.8/language/precedence_spec.rb',

    # return_spec just hangs...
    '^' + DIR + '/spec/rubyspec/1.8/language/retry_spec.rb',

    ###################
    #### CORE SPECS
    ###################
    DIR + '/spec/rubyspec/1.8/core',

    # Dir depends on fileutils and that still isn't quite working
    '^' + DIR + '/spec/rubyspec/1.8/core/dir',
    '^' + DIR + '/spec/rubyspec/1.8/core/continuation',
    '^' + DIR + '/spec/rubyspec/1.8/core/filetest',

    # The case_compare_spec fixtures try to override equal?, which MagLev
    # doesn't allow.  As a result, MagLev can't even load that file w/o an
    # error (i.e., tagging it as fails still generates an error).  So, for
    # the kernel specs, we turn all of them on except for case_compare_spec...
    '^' + DIR + '/spec/rubyspec/1.8/core/kernel/case_compare_spec.rb',
    '^' + DIR + '/spec/rubyspec/1.8/core/objectspace',

    #  TODO: core/proc/arity_spec.rb has a parse error which blows up the whole
    #  rest of that test.
    # An exception occurred during: loading
    #spec/rubyspec/1.8/core/proc/arity_spec.rb ERROR StandardError: User
    #defined error, 'parser fail'
    '^' + DIR + '/spec/rubyspec/1.8/core/proc/arity_spec.rb',

    '^' + DIR + '/spec/rubyspec/1.8/core/process',
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
