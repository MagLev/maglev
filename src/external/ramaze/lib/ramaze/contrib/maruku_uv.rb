# A hack to make MaRuKu use UltraViolet for highlighting
# To specify the style to use use for example:
#     uv_style: amy
#     html_use_syntax: true
#     code_lang: ruby
# In the document header area.

require 'maruku'

module MaRuKu
  module Out
    module HTML
      def uv_highlight(source, lang, style)
        require 'uv'

        html = Uv.parse(source, 'xhtml', lang, lines = false, style)

        # Prepare <code> containing <pre>
        code = Document.new(html, :respect_whitespace => :all).root
        code.name = 'code'
        code.attributes['class'] = lang
        code.attributes['lang'] = lang

        # Prepare <pre>
        pre = Element.new('pre')
        pre << code
        pre.attributes['class'] = style
        pre
      rescue => ex
        puts ex
        to_html_code_using_pre(source)
      end

      def to_html_code
        source = self.raw_code
        use_syntax = get_setting(:html_use_syntax)
        uv_style = get_setting(:uv_style)

        lang = self.attributes[:lang] || @doc.attributes[:code_lang]
        lang ||= 'ruby' if attributes[:ruby]

        # we always use syntax highlighting, this is a doc wiki
        if lang and use_syntax
          element = uv_highlight(source, lang, uv_style)
        else
          element = to_html_code_using_pre(source)
        end

        color = get_setting(:code_background_color)

        if color != Globals[:code_background_color]
          element.attributes['style'] = "background-color: #{color};"
        end

        add_ws element
      end
    end # HTML
  end # Out
end # MaRuKu
