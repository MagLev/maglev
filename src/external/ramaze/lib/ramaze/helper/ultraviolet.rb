module Ramaze
  module Helper
    module Ultraviolet
      trait :ultraviolet => {
        :output       => 'xhtml',
        :syntax       => nil, # syntax_name, nil|false indicates automatic detection
        :line_numbers => false,
        :style        => 'classic', # render_style
        :headers      => false, # ouput document with all headers
      }

      # Parse and output the file at the given path.
      # Please head over to the Ultraviolet documentation for more information
      # on possible options.
      def ultraviolet(path, options = {})
        o = ancestral_trait[:ultraviolet].merge(options)
        output, syntax, lines, style, headers =
          o.values_at(:output, :syntax, :line_numbers, :style, :headers)

        syntax ||= Uv.syntax_for_file(path).first.first
        code = File.read(path)

        p [code, output, syntax, lines, style, headers]
        Uv.parse(code, output, syntax, lines, style, headers)
      end

      # Return absolute path to the css of given name.
      #
      # Usually this will point to somewhere in the gem tree.
      #
      # It seems like Uv will add support for user-defined PATH in the future,
      # so we will, to be future-proof, traverse the Uv.path even though it
      # currently will only have one directory.

      def ultraviolet_css(theme)
        Uv.path.each do |path|
          Dir[path/"render/xhtml/files/css/*.css"].each do |css|
            return css if File.basename(css, '.css') == theme
          end
        end
      end
    end
  end
end
