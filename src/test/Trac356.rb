
def test356
  ignore = [ /a/, /b/ ]
  x = []
  y = []

  [ "a", "b", "c", "d" ].find do |file|
    x << file
    if ignore.any? { |pattern| file =~ pattern }
      next
    end
    y << file
    nil
  end
  unless x == ["a", "b", "c", "d"] ; raise 'err' ; end
  unless y == ["c", "d"] ; raise 'err' ; end
end

test356
true
