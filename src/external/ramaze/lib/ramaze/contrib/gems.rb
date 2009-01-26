module Ramaze
  module Gems
    @gems = []

    class << self
      def gem(name, version = nil, options = {})
        case version
        when String
          options[:version] = version
        when Hash
          options.merge!(version)
        end

        @gems << GemStone.new(name, options)
      end

      # options include:
      #  :install_dir => where to install gems
      #  :extconf     => additional options for building extensions

      def options opts = {}
        @options ||= {}
        @options.merge! opts unless opts.empty?
        @options
      end

      def setup opts = {}
        options(opts)
        @gems.each{|gem| gem.setup }
      end
    end

    class GemStone
      attr_reader :name, :options

      def initialize(name, options = {})
        @name, @options = name, options
        require 'rubygems/dependency_installer'
        @installer = Gem::DependencyInstaller.new(@options)
      end

      def setup(ran = false)
        Gem.activate(name, *[options[:version]].compact)
        require options[:lib] || name
      rescue LoadError => error
        puts error
        return if ran
        install
        setup(ran = true)
      end

      def install
        if extconf = (options[:extconf] || Gems.options[:extconf])
          old_argv = ARGV.clone
          ARGV.replace extconf.split(' ')
        end

        print "Installing #{name}..."
        @installer.install name, options[:version]
        puts "done.\n\n"
      ensure
        ARGV.replace old_argv if extconf
      end
    end
  end
end

__END__
Usage example:

module Ramaze::Gems
  gem 'haml'
  gem 'sequel', '>=1.2.0'
  gem 'hpricot', :source => 'http://code.whytheluckystiff.net'
  gem 'aws-s3', :lib => 'aws/s3'

  setup
end
