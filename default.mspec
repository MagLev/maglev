# -*-ruby-*-
# Default RubySpec files for MagLev.

class MSpecScript

  DIR = File.dirname(__FILE__)
  ci_files = [
    DIR + '/spec/rubyspec/1.8/language',
    DIR + '/spec/rubyspec/1.8/core'
  ]

  File.open('spec/do_not_run_specs') do |f|
    f.each do |line|
      # Skip blank lines and lines beginning with '#'
      line =~ /^#|^\s*$/ && next

      entry = '^' + DIR + '/' + line.chomp
      ci_files << entry
    end
  end

  # Define the continuous integration specs (the ones known to pass)
  set :ci_files, ci_files

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
