require File.expand_path('../shared/add', __FILE__)

describe "RCQueue#push" do
  it_behaves_like(:rcqueue_add, :push)
end

describe "RCQueue#<<" do
  it_behaves_like(:rcqueue_add, :<<)
end
