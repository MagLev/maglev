fiber = Fiber.new do
  Fiber.yield 1
  2
end

raise "wrong yield value" unless fiber.resume == 1
raise "wrong return value" unless fiber.resume
raise "Fiber should be dead" if fiber.alive?

fiber = Fiber.new do |first|
  second = Fiber.yield first + 2
end

raise "wrong yield value" unless fiber.resume(10) == 12
raise "wrong yield value" unless fiber.resume(14) == 14
raised = false
begin
  fiber.resume 18
rescue FiberError
  raised = true
end
raise "Dead fiber shouldn't be resumeable" unless raised

fiber = Fiber.new do |first|
  raise "Fiber.current not set" unless Fiber.current == fiber
  second = Fiber.yield first + 2
end
fiber.resume 10

fiber = Fiber.new do
  error = false
  begin
    fiber.resume
  rescue FiberError
    error = true
  end
  raise "double resume should be prohibited" unless error
end
fiber.resume
