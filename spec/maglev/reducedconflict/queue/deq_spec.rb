require File.expand_path('../shared/remove', __FILE__)

describe "RCQueue#deq" do
  it_behaves_like(:rcqueue_remove, :deq)
end
