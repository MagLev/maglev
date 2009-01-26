#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions
    # Extensions for String

    module String
      { :reset          =>  0,
        :bold           =>  1,
        :dark           =>  2,
        :underline      =>  4,
        :blink          =>  5,
        :negative       =>  7,
        :black          => 30,
        :red            => 31,
        :green          => 32,
        :yellow         => 33,
        :blue           => 34,
        :magenta        => 35,
        :cyan           => 36,
        :white          => 37,
      }.each do |key, value|
        define_method key do
          "\e[#{value}m" + self + "\e[0m"
        end
      end
    end

  end
end
