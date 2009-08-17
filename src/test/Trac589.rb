begin
  catch(:foo) do
    throw :bar  # generate name error: there is no catch for :bar
  end
rescue NameError => ne
  raise "Expecting name to be :bar but was #{ne.name.inspect}" unless ne.name == :bar
end
