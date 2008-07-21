require 'test/numerics_test'

ra = Array.new #result array
test = NumericsTest.new

ra[0] = test.addIntSimple
ra[1] = test.addIntCompound
ra[2] = test.subtractIntSimple
ra[3] = test.subtractIntCompound
ra[4] = test.divideIntSimple
ra[5] = test.divideIntCompound
ra[6] = test.multiplyIntSimple
ra[7] = test.multiplyIntCompound
ra[8] = test.addFloatSimple
ra[9] = test.addFloatCompound
ra[10] = test.subtractFloatSimple
ra[11] = test.subtractFloatCompound
ra[12] = test.multiplyFloatSimple
ra[13] = test.multiplyFloatCompound
ra[14] = test.divideFloatSimple
ra[15] = test.divideFloatCompound
ra[16] = test.modulusInt
ra[17] = test.modulusFloat
ra[18] = test.powerInt
ra[19] = test.powerFloat
ra[20] = test.rightShift
ra[21] = test.leftShift
ra[22] = test.parallel(3,7)
puts ra[22][0]
puts ra[22][1]

ra