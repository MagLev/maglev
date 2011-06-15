
x1 = Object.new
x2 = Object.new
x3 = Object.new
x4 = Object.new
x5 = Object.new
$g499 = [] 
x1.instance_eval "def test;puts 'test 1' ; $g499 << 51 ;end"
x2.instance_eval "def test;puts 'test 2' ; $g499 << 52 ;end"
x3.instance_eval "def test;puts 'test 3' ; $g499 << 53 ;end"
x4.instance_eval "def test;puts 'test 4' ; $g499 << 54 ;end"
x5.instance_eval "def test;puts 'test 5' ; $g499 << 55 ;end"
x1.test
x2.test
x3.test
x4.test
x5.test

unless (ax = $g499) == [ 51, 52, 53, 54, 55 ] ; raise 'error'; end
