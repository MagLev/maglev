Maglev::System.session_temp_put( :MAGLEV_allIvsDynamic, true )

class Application
  puts "start body"
  @@instance = nil

  class << self
    puts "meta body"
    def inherited(base)
      puts "start inherited"
      super
      @@instance = base.instance
    end

    def instance
      # puts "Calling freeze on #{self}"
      # freeze
      @@instance ||= new
    end
    puts "end meta body"
  end
  puts "end body"
end


class MyApp < Application
  puts "MyApp body"
end

p Application.instance
