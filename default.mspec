# -*-ruby-*-
# Default RubySpec files for MagLev.

class MSpecScript
  # Define the continuous integration specs (the ones known to pass)
  DIR = File.dirname(__FILE__)
  set :ci_files, [
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
#    DIR + '/spec/rubyspec/1.8/core/gc',
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
    DIR + '/spec/rubyspec/1.8/core/range',
    DIR + '/spec/rubyspec/1.8/core/regexp',
    DIR + '/spec/rubyspec/1.8/core/signal',
    DIR + '/spec/rubyspec/1.8/core/string',
    DIR + '/spec/rubyspec/1.8/core/struct',
    DIR + '/spec/rubyspec/1.8/core/symbol',
    DIR + '/spec/rubyspec/1.8/core/systemexit',
    DIR + '/spec/rubyspec/1.8/core/thread',
#    DIR + '/spec/rubyspec/1.8/core/threadgroup',
    DIR + '/spec/rubyspec/1.8/core/time',
    DIR + '/spec/rubyspec/1.8/core/true',
    DIR + '/spec/rubyspec/1.8/core/unboundmethod'
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
