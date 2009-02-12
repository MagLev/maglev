#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ruby-growl'

module Ramaze
  module Logger

    # Informer for the growl notification system on OSX.

    class Growl < ::Growl

      trait :defaults => {
        :name => 'walrus',
        :host => 'localhost',
        :password => 'walrus',
        :all_notifies => %w[error warn debug info dev],
        :default_notifies => %w[error warn info]
      }

      # Takes the options from the default trait for merging.

      def initialize(options = {})
        options = class_trait[:defaults].merge(options).values_at(:host, :name, :all_notifies, :default_notifies, :password)
        super(*options)
      end

      # integration to Logging

      def log(tag, *args)
        notify(tag.to_s, Time.now.strftime("%X"), args.join("\n")[0..100])
      rescue Errno::EMSGSIZE
        # Send size was to big (not really), ignore
      end
    end

  end
end
