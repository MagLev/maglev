require 'maruku'

module Ramaze
  module Helper::Maruku
    def maruku(text)
      Maruku.new(text).to_html
    end
  end
end
