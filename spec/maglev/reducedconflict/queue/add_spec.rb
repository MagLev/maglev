require File.expand_path('../shared/add', __FILE__)

describe "RCQueue#add" do
  it_behaves_like(:rcqueue_add, :add)
end
