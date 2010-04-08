Maglev::System.session_temp_put( :MAGLEV_allIvsDynamic, true )

class Application
  @@instance = nil

  class << self
    def inherited(base)
      super
      @@instance = base.instance
    end

    def instance
      # puts "Calling freeze on #{self}"
      # freeze
      @@instance ||= new
    end
  end
end


class MyApp < Application
end

p Application.instance
