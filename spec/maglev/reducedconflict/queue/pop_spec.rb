require File.expand_path('../shared/remove', __FILE__)

describe "RCQueue#pop" do
  it_behaves_like(:rcqueue_remove, :pop)
end
