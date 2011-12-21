require File.expand_path('../shared/add', __FILE__)

describe "RCQueue#enq" do
  it_behaves_like(:rcqueue_add, :enq)
end
