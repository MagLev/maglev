# Reproduces bug where first arg to rubySend:withArguments: is an Array instead of a Symbol.
class TestSend

  def hello(*args)
    puts 'Hello ' + args.join(' ')
  end

  def sendToHello(*args)
    send :hello, *args
  end
end

testSymbol = :hello
testArg =  ["ArgOne"]

obj = TestSend.new
obj.sendToHello(testSymbol, testArg)
