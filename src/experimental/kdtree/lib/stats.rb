module Stats
  class Sample
    def initialize(name='A Sample')
      @name = name
      @min = nil
      @max = nil
#      @samples = Array.new
      @count = 0
      @sum = 0
    end

    def datum(d)
      @count += 1
      @min ||= d
      @max ||= d
      @min = d if d < @min
      @max = d if d > @max
      #      @samples << d
      @sum += d
      self
    end

    def to_s
      "#{name}: count #{@count} min #{@min} max #{@max} sum #{@sum} mean #{@sum.to_f / @count.to_f}"
    end
  end
end
