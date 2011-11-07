require File.expand_path('../shared/size', __FILE__)

describe "RCQueue#size" do
  it_behaves_like(:rcqueue_size, :size)
end
