#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # Loads and sets up contrib modules.
  #
  # For more information @see Ramaze::Contrib.load
  #
  # Usage:
  #
  #   Ramaze.contrib :gzip_filter

  def self.contrib(*args)
    Contrib.load(*args)
  end

  # A module used for loading contrib modules.

  module Contrib

    # Loads and sets up contrib modules from /ramaze/contrib.
    #
    # === Usage:
    #
    #   Ramaze::Contrib.load :gzip_filter
    #
    # === Creating a contrib module:
    #
    #   module Ramaze::Contrib::Test
    #     def self.setup
    #       Log.info "Test module set up"
    #     end
    #   end
    #   class TestController < Ramaze::Controller
    #     map '/test'
    #     def index; "Chunky Bacon!"; end
    #   end
    #
    #   # Save that to your <app dir>/contrib and use like:
    #   Ramaze.contrib :test

    def self.load(*contribs)
      contribs.each do |name|
        mod_name = name.to_s.camel_case
        begin
          const = Contrib.const_get(mod_name)
          Global.contribs << const
          const.startup if const.respond_to?(:startup)
          Log.dev "Loaded contrib: #{const}"
        rescue NameError
          files = Dir["{contrib,#{BASEDIR}/ramaze/contrib}/#{name}.{so,bundle,rb}"]
          raise LoadError, "#{mod_name} not found" unless files.any?
          require(files.first) ? retry : raise
        end
      end
    end

    # Will call .shutdown on all contrib modules which support it.
    #---
    # .shudown gets called by Ramaze :essentials.
    #+++

    def self.shutdown
      Global.contribs.each do |contrib|
        contrib.shutdown if contrib.respond_to?(:shutdown)
      end
    end

    # Will load all contrib modules from options[:contrib]
    #---
    # .startup gets called by Ramaze :essentials.
    #+++

    def self.startup(options = {})
      return unless options[:contrib]
      [options[:contrib]].flatten.each do |contrib|
        load(contrib)
      end
    end
  end # class Contrib
end # class Ramaze
