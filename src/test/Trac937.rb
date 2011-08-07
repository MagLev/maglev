autoload :Trac937Foo, File.expand_path("../Trac937_autoload.rb", __FILE__)

Kernel.class_eval do
  def trac937Foo_loaded?
    false
  end
end

# Autoloaded constants should be defined, but not loaded when using #defined?
raise 'fail' unless defined?(Trac937Foo)
raise 'fail' if trac937Foo_loaded?

# Autoloaded constants should be loaded, if asking #defined? on their nested components
defined?(Trac937Foo::Trac937Bar)
raise 'fail' unless trac937Foo_loaded?

true
