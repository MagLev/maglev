# 
# Benchmark block of code from ruby-benchmark-suite/real-world
# FAILS - No mathn support at this time
#

require 'benchmark'
require 'test/hilbert'

Benchmark.bm(7) do |bmr|
 
  bmr.report('bm_hilbert_matrix '){
    require 'hilbert' # also brings in mathn, matrix, rational and complex

    def run_hilbert(dimension)
        m = hilbert(dimension)
        print "Hilbert matrix of dimension #{dimension} times its inverse = identity? "
        k = m * m.inv
        print "#{k==Matrix.I(dimension)}\n"
        m = nil # for the garbage collector
        k = nil
    end

    dimension = (ARGV[0] || 60).to_i
    run_hil
  }
  
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
