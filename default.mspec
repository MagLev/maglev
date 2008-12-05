# -*-ruby-*-
# Default RubySpec files for MagLev.

class MSpecScript
  set :ci_files, [
    File.dirname(__FILE__) + 'spec/rubyspec/1.8/language',
    File.dirname(__FILE__) + 'spec/rubyspec/1.8/core'
  ]
  set :target, File.dirname(__FILE__) + '/bin/maglev-ruby'
end
