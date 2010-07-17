# MagLev was getting this error:
# #<NoMethodError: NoMethodError: undefined method `zip' for NilClass>`zip' called

count = 0
combo = nil
(1..10).to_a.combination(5).each do |c|
  count+= 1; combo = c
end
p count, combo
unless count == 252 ; raise 'bad count'; end
unless combo == [6, 7, 8, 9, 10]; raise 'bad combo'; end
true
