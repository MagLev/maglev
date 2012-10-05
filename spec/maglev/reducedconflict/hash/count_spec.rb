require File.expand_path('../shared/length', __FILE__)

describe "RCHash#count" do
  it_behaves_like(:rchash_length, :count)
end
