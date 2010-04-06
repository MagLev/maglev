require 'mustache'

puts Mustache.render("123Hello321 {{planet}}", :planet => "World")

class Simple < Mustache
  def name
    "Chris"
  end

  def value
    10_000
  end

  def taxed_value
    value - (value * 0.4)
  end

  def in_ca
    true
  end
end

p Simple.render
