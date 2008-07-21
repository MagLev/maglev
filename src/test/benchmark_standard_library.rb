# 
# Benchmark blocks of code from ruby-benchmark-suite/standard-library
# FAILS - No complex module support at this time
#

require 'benchmark'
require 'complex'

Benchmark.bm(7) do |bmr|
 
  bmr.report('bm_app_mandelbrot '){

    def mandelbrot?(z)
      i = 0
      while i<100
        i+=1
        z = z * z
        return false if z.abs > 2
      end
      true
    end

    ary = []

    (0..100).each{|dx|
      (0..100).each{|dy|
        x = dx / 50.0
        y = dy / 50.0
        c = Complex(x, y)
        ary << c if mandelbrot?(c)
      }
    }

    p ary

  }
  
end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
