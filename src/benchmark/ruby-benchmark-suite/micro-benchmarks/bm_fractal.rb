require File.dirname(__FILE__) + '/../lib/benchutils'

label = File.expand_path(__FILE__).sub(File.expand_path("..") + "/", "")
iterations = ARGV[-3].to_i
timeout = ARGV[-2].to_i
report = ARGV.last

BAILOUT = 16
MAX_ITERATIONS = 1000

class Mandelbrot
	
	def initialize
		puts "Rendering"
		for y in -39...39 do
			puts
			for x in -39...39 do
				i = iterate(x/40.0,y/40.0)
				if (i == 0)
					print "*"
				else
					print " "
				end
			end
		end
	end

	def iterate(x,y)
		cr = y-0.5
		ci = x
		zi = 0.0
		zr = 0.0
		i = 0
		
		while(1)
			i += 1
			temp = zr * zi
			zr2 = zr * zr
			zi2 = zi * zi
			zr = zr2 - zi2 + cr
			zi = temp + temp + ci
			return i if (zi2 + zr2 > BAILOUT)
			return 0 if (i > MAX_ITERATIONS)
		end
	end

end

benchmark = BenchmarkRunner.new(label, iterations, timeout)
benchmark.run do
  5.times do
    Mandelbrot.new
  end
end
  
File.open(report, "a") {|f| f.puts "#{benchmark.to_s},n/a" }
