module RX

  class NullAPI

    def method_missing(m)
      # whatever
      nil
    end
  end

end
