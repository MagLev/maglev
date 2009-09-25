# Since the JSON library isn't yet working, here is a light-weight
# fake JSON implementation....
class Post
  # TODO: Need to properly quote fields...
  def to_json
    { "id" => object_id,
      "title" => @title,
      "text"  => @text,
      "tags"  => @tags,
      "date"  => @date
    }.to_json
  end
end

class Fixnum
  alias to_json to_s
end

class Hash
  def to_json
    json_for_contents = inject("") do |json,(key, value)|
      json << "\"#{key.to_s}\" : #{value.to_json},"
    end
    json_for_contents.chop! if size > 0
    "{ #{json_for_contents} }"
  end
end

class String
  alias to_json inspect
end

class Array
  def to_json
    json_for_contents = inject("") do |json,el|
      json << el.to_json << ','
    end
    json_for_contents.chop! if size > 0
    "[#{json_for_contents}]"
  end
end

class Symbol
  def to_json
    ":#{to_s}".to_json
  end
end

class NilClass
  def to_json
    "null"
  end
end

class Time
  def to_json
    %Q{ "#{to_s}" }
  end
end
if $0 == __FILE__
  puts 159511297.to_json
  puts ":x   #{:x.to_json}"
  puts "===== HASH ====="
  h = { }
  puts h.to_json
  h[:a] = "Foo"
  puts h.to_json
  h["Foo"] = :bar
  puts h.to_json

  puts "===== ARRAY ====="
  a = []
  puts a.to_json
  a << "foo"
  puts a.to_json

  unless defined? Maglev
    puts "---- Testing roundtrip"
    # Have MRI test roundtrip
    h = { "id" => object_id,
      "title" => 'The blog title',
      "text"  => 'Some blog text',
      "tags"  => ['maglev', 'blog'],
      "date"  => Time.now
    }

    json = h.to_json

    # Do the requires after we've calculated the json, as the require of
    # json will load the real json....
    require 'rubygems'
    require 'json'


    h2 = JSON.parse json
    p h2

    p Time.now.to_json
  end
end
