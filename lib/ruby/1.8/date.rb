# Author: Tadayoshi Funaba 1998-2008
# Maglev file date.rb

#  first openings of Date classes, optimization to get fixed instVars

class Date
  ITALY     = 2299161 # 1582-10-15  

  def initialize(ajd=0, of=0, sg=ITALY) 
    @ajd, @of, @sg = ajd, of, sg 
  end

  class Infinity < Numeric
    def initialize(d=1) @d = d <=> 0 end

    def d() @d end

    protected :d
  end
end

require 'date2.rb'
