require File.expand_path('../../../../spec_helper', __FILE__)
require 'maglev/rchash'
require File.expand_path('../shared/each', __FILE__)
require File.expand_path('../shared/iteration', __FILE__)

describe "RCHash#each" do
  quarantine! do
    it_behaves_like(:rchash_each, :each)
    it_behaves_like(:rchash_iteration_no_block, :each)
  end
end
