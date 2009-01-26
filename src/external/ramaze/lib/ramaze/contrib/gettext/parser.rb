# suppress load warning message
verbose = $VERBOSE
$VERBOSE = nil
require 'gettext/rgettext'
$VERBOSE = verbose

module Ramaze::Tool::Gettext::Parser
  module_function

  TARGETS = Ramaze::Template::ENGINES.values.inject([]) do |exts, list|
    list += exts
  end

  def target?(file)
    TARGETS.include?(File.extname(file)[1..-1])
  end

  def parse(file, ary)
    regex = Ramaze::Tool::Gettext.trait[:regex]
    body = File.read(file)
    body.gsub!(regex) do
      msg = $1
      unless msg.to_s.empty?
        line_number = body[0..(body.index(msg))].split("\n").size
        add_message(ary, msg, file, line_number)
      end
    end
    return ary
  end

  def add_message(ary, msg, file, line_number)
    loc = message_location(file, line_number)
    if value = ary.assoc(msg)
      value << loc
    else
      ary << [msg, loc]
    end
    return ary
  end

  def message_location(file, line_number)
    file + ":line" + line_number.to_s
  end
end

GetText::RGetText.add_parser(Ramaze::Tool::Gettext::Parser)
