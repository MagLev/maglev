
x = srand
unless x.is_a?(Integer) ; raise 'fail'; end
x = srand(7)
unless x.is_a?(Integer) ; raise 'fail'; end
x = srand
unless x.equal?(7) ; raise 'fail'; end
x = rand

true
