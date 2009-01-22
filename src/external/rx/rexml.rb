require 'rx/reader'

module RX

  # %w{ doctype doctype_end instruction tag_end tag_start text xmldecl }

  class RXToStreamListener
    def initialize(rexml_listener)
      @listener = rexml_listener
    end
    
    def stag(e)
      index = 1
      attrs = {}
      while index < e.strings.length
        attrs[e.strings[index].to_s] = e.strings[index + 1].ustr
        index += 2     
      end
      @listener.tag_start(e.type.to_s, attrs)
      nil
    end

    def etag(e)
      @listener.tag_end(e.type)
      nil
    end

    def text(text)
      @listener.text text
      nil
    end

    def end
      true
    end
  end
end
