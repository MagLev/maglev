module Ramaze
  def self.setup(start = true, &block)
    SetupEnvironment.new(&block) if block_given?
    self.start if start
  end

  class SetupEnvironment
    # FIXME:
    # * This is weird, class scope includes Global, yet it's skipped
    #   by Ruby on the lookup only because it's not in the scope of the block
    #   at the point of creation, shouldn't instance_eval take the binding of
    #   the SetupEnvironment instance in acocunt?

    def initialize(&block)
      instance_eval(&block)
    end

    # Shortcut for Ramaze::Gems::gem
    def gem(*args)
      require 'ramaze/contrib/gems'
      Ramaze::Gems::gem(*args)
      Ramaze::Gems::setup
    end

    # Shortcut if you don't need specific versions but tons of gems
    def gems(*args)
      args.each{|g| gem(g) }
    end

    def global(hash = nil)
      if hash
        Global.merge!(hash)
      else
        Global
      end
    end
    alias option global
  end
end

__END__
Usage example:

Ramaze.setup do
  gem 'json'

  option :middleware => true
  global.adapter = :thin
  option.port = 7000
end
