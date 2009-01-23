require 'benchmark'
puts '==> Start benchmark: ./real-world/bm_hilbert_matrix.rb'
Benchmark.bm(7) do |bmr|
  bmr.report {
### End header added by script ###

  # Submitted by M. Edward (Ed) Borasky
  
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
  run_hilbert(dimension)

### Begin footer added by script ###
}
end
puts '==> End benchmark: ./real-world/bm_hilbert_matrix.rb'
