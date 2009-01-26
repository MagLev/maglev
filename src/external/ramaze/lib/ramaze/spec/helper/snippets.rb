if caller
  snippet = caller.grep(%r!spec/snippets/!).first.split(':').first
  require File.expand_path(snippet).gsub('/spec/', '/lib/ramaze/')

  if defined?(Ramaze::CoreExtensions)
    Ramaze::CoreExtensions.constants.each do |const|
      ext = Ramaze::CoreExtensions.const_get(const)
      into = Module.const_get(const)
      into.__send__(:include, ext)
    end
  end
end

require 'lib/ramaze/spec/helper/bacon'
