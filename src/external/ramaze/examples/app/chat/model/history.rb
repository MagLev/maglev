class History
  include Ramaze::Helper::CGI

  def initialize(size = 13)
    @size = size
    @history = []
  end

  def write(nick, text)
    text.strip!
    return if text.empty?
    @history.shift until @history.size < @size
    @history << Message.new(h(nick), h(text), Time.now)
    true
  end

  def to_html
    @history.map {|message|
      '<div class="message">' <<
          [:time, :nick, :text].map{|key| span_for(message, key)}.join("\n") <<
      '</div>'
    }.join("\n")
  end

  def span_for(message, key)
    "<span class='#{key}'>#{message[key]}</span>"
  end

  include Enumerable

  def each
    @history.sort.each do |message|
      yield message
    end
  end
end
