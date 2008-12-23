require File.expand_path('simple', File.dirname(__FILE__))

require 'stringio'

# From PickAxe

sio = StringIO.new("time flies like an arrow")
test(sio.read(5), "time ", "Test 1")
test(sio.read(5), "flies", "Test 2")

sio.pos = 19
test(sio.read(5), "arrow", "Test 3")

test(sio.rewind, 0, "Test 4")
test(sio.write("fruit"), 5, "Test 5")

sio.pos = 16
test(sio.write("a banana"), 8, "Test 6")

test(sio.rewind, 0, "Test 7")
test(sio.read, "fruitflies like a banana", "Test 8")



report

true
