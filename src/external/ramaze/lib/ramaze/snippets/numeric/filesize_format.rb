#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  module CoreExtensions

    # Extensions for Numeric
    module Numeric
      FILESIZE_FORMAT = [
        ['%.1fT', 1 << 40],
        ['%.1fG', 1 << 30],
        ['%.1fM', 1 << 20],
        ['%.1fK', 1 << 10],
      ]

      # Output this number as easily readable filesize.
      # Usage:
      #   100_000.filesize_format             # => "97.7K"
      #   100_000_000.filesize_format         # => "95.4M"
      #   100_000_000_000.filesize_format     # => "93.1G"
      #   100_000_000_000_000.filesize_format # => "90.9T"
      def filesize_format
        FILESIZE_FORMAT.each do |format, size|
          return format % (self.to_f / size) if self >= size
        end

        self.to_s
      end
    end

  end
end
