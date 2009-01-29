# -*-ruby-*-
# A config file for running mspec that runs all, and only, the files listed
# in svn/tests/rubytst/passingspecs.conf.  This is used by the rake tasks.

class MSpecScript
  spec_dir =  File.dirname(__FILE__) + '/spec/rubyspec/'
  conf_file = File.dirname(__FILE__) + '/../svn/tests/rubytst/passingspecs.conf'

  # Read the conf file, removing trailing newlines and filter out comments
  # and empty lines.
  passing_specs = IO.readlines(conf_file).map { |line| "#{spec_dir}#{line.chomp}"}
  passing_specs.reject! { |line| line =~ /^\s*$|^\s*#/ }

  # An ordered list of the directories containing specs to run
  set :files, passing_specs

  # The default implementation to run the specs.
  set :target, 'maglev-ruby'

  # The set of substitutions to transform a spec filename into a tag
  # filename.  The transformations are applied, in the given sequence, to a
  # filename, yielding a tag file name.
  set :tags_patterns, [
    [%r(spec/rubyspec/), 'spec/frozen/'],
    [%r(spec/), 'spec/tags/'],
    [/_spec.rb$/, '_tags.txt']
  ]
end
