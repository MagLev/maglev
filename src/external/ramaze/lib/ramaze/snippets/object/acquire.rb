#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for Kernel

    module Object

      # Require all .rb and .so files on the given globs, utilizes Dir::[].
      #
      # Examples:
      #   # Given following directory structure:
      #   # src/foo.rb
      #   # src/bar.so
      #   # src/foo.yaml
      #   # src/foobar/baz.rb
      #   # src/foobar/README
      #
      #   # requires all files in 'src':
      #   acquire 'src/*'
      #
      #   # requires all files in 'src' recursive:
      #   acquire 'src/**/*'
      #
      #   # require 'src/foo.rb' and 'src/bar.so' and 'src/foobar/baz.rb'
      #   acquire 'src/*', 'src/foobar/*'

      def acquire(*globs)
        Ramaze.deprecated('Object#acquire', 'Ramaze::acquire')
        Ramaze.acquire(*globs)
      end
    end

  end
end
