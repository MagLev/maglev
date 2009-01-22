# -*-ruby-*-
# Default RubySpec files for MagLev.


DIR = File.dirname(__FILE__)
$ci_files = [ 'spec/rubyspec/language' ]

File.open(DIR + '/spec/do_not_run_specs') do |f|
  f.each do |line|
    # Skip blank lines and lines beginning with '#'
    line =~ /^#|^\s*$/ && next

    entry = '^' +  line.chomp
#    entry[entry.rindex('/'),1] = '//'
    $ci_files << entry
  end
end
# $ci_files = [
#   'spec/rubyspec/language',
#   'spec/rubyspec/language/constants_spec.rb',
#   'spec/rubyspec/language/block_spec.rb',
#   'spec/rubyspec/language/for_spec.rb',
#   'spec/rubyspec/language/next_spec.rb',
#   'spec/rubyspec/language/precedence_spec.rb',
#   'spec/rubyspec/language/retry_spec.rb',
# ]
puts $ci_files.inspect

class MSpecScript

  # Define the continuous integration specs (the ones known to pass)
  set :ci_files, $ci_files

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
