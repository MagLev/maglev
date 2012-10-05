require File.expand_path('../shared/remove', __FILE__)

describe "RCQueue#remove" do
  it_behaves_like(:rcqueue_remove, :remove)
end
