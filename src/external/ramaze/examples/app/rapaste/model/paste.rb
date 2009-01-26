class Paste < Sequel::Model(:paste)
  set_schema do
    primary_key :id

    text :text, :null => false
    text :syntax, :null => false
    time :created
  end

  def text_fragment(style = STYLE)
    fragment = lines[0...10].join("\n")
    uv(fragment, style, lines = false)
  end

  def lines
    text.scan(/^.*?$/)
  end

  def view(format, style = STYLE)
    case format.downcase
    when /x?html/
      uv(text, style)
    else
      text
    end
  end

  def uv(text, style = STYLE, lines = true)
    Uv.parse(text, "xhtml", syntax, lines, style)
  rescue ArgumentError => ex
    Ramaze::Log::error(ex)
    "<pre>#{text}</pre>"
  end

  def syntax_name
    SYNTAX_LIST[syntax]
  end

  # Create prioritized and sorted list of syntaxes

  list = Ramaze::Dictionary.new

  syntaxes = Uv.instance_variable_get('@syntaxes')

  UV_PRIORITY_NAMES.each do |key|
    list[key] = syntaxes[key].name
  end

  (syntaxes.keys - UV_PRIORITY_NAMES).
    sort_by{|key| syntaxes[key].name}.
    each do |key|
    list[key] = syntaxes[key].name
  end

  SYNTAX_LIST = list
end

Paste.create_table! unless Paste.table_exists?
