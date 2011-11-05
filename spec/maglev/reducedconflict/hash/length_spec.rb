require File.expand_path('../shared/length', __FILE__)

describe "RCHash#length" do
  it_behaves_like(:rchash_length, :length)
end
