module Ramaze::Helper
  module XHTML
    def css(name, media = 'screen', options = {})
      if options.empty?
        "<link rel='stylesheet' href='/css/#{name}.css' type='text/css' media='#{media}' />"
      elsif options[:only] == 'IE'
        "<!--[if IE]>#{css(name, media)}<![endif]-->"
      end
    end

    def css_for(*names)
      names.flatten.map{|name| css(name) }.join("\n")
    end

    def js(name)
      "<script src='/js/#{name}.js' type='text/javascript'></script>"
    end

    def js_for(*names)
      names.flatten.map{|name| js(name) }.join("\n")
    end
  end
end
