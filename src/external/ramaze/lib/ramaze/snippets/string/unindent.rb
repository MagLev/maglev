module Ramaze
  module CoreExtensions

    # Extensions for String

    module String

      # Useful for writing indented String and unindent on demand, based on the
      # first line with indentation.
      def unindent
        find_indent = proc{ |l| l.find{|l| !l.strip.empty?}.to_s[/^(\s+)/, 1] }

        lines = self.split("\n")
        space = find_indent[lines]
        space = find_indent[lines.reverse] unless space

        strip.gsub(/^#{space}/, '')
      end
      alias ui unindent

      # Destructive variant of undindent, replacing the String
      def unindent!
        self.replace unindent
      end
      alias ui! unindent!
    end
  end
end
