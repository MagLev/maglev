require 'lib/ramaze/spec/helper/snippets'

describe 'locals' do
  should 'find locals' do
    a = 1
    b = 2
    binding.locals.should == {'a' => 1, 'b' => 2}
  end
end
