begin
  a = FXX
rescue NameError => ea
  unless ea.message.start_with?('uninitialized constant')
    raise 'failed'
  end
end
Module.remove_method( :const_missing )
begin
  b = FXX
rescue NameError => ea
  unless (mx = ea.message).start_with?('NoMethodError: undefined method')
    raise 'failed'
  end
end
true
