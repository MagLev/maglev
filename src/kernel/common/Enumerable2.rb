module Enumerable
  class Sort
    def sort_by(xs)
      # The ary and its elements sould be inmutable while sorting

      elements = xs.map { |x| SortedElement.new(x, yield(x)) }
      sort(elements).map { |e| e.value }
    end
  end
end
Enumerable.__freeze_constants
