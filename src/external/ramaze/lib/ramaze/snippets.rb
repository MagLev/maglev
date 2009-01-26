#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

dir  = File.join(File.expand_path(File.dirname(__FILE__)), 'snippets')
glob = File.join(dir, '**', '*.rb')

Dir[glob].each do |snippet|
  require(snippet)
end

Ramaze::CoreExtensions.constants.each do |const|
  ext = Ramaze::CoreExtensions.const_get(const)
  into = Module.const_get(const)

  collisions = ext.instance_methods & into.instance_methods

  if collisions.empty?
    into.__send__(:include, ext)
  else
    warn "Won't include %p with %p, %p exists" % [into, ext, collisions]
  end
end
