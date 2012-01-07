require File.expand_path('../shared/remove', __FILE__)

describe "RCQueue#shift" do
  it_behaves_like(:rcqueue_remove, :shift)
end
