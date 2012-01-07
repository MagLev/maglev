require File.expand_path('../shared/size', __FILE__)

describe "RCQueue#length" do
  it_behaves_like(:rcqueue_size, :length)
end
