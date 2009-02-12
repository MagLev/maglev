#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module Logger

    # Informer for the Knotify notfication system used on KDE.
    # Doesn't need any special libraries.

    class Knotify
      include Logging

      trait :present => 16

      # Please see for more information on the API used here:
      # http://lukeplant.me.uk/articles.php?id=3

      def log(tag, *messages)
        present = class_trait[:present]
        tag = tag.to_s.capitalize
        messages.flatten.each do |message|
          system(%{dcop knotify default notify Ramaze "#{tag}" "#{message}" '' '' #{present} 0})
        end
      end
    end

  end
end
