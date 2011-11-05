require File.expand_path('../shared/length', __FILE__)

describe "RCHash#size" do
  it_behaves_like(:rchash_length, :size)
end
