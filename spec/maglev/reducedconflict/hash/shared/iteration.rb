describe :rchash_iteration_no_block, :shared => true do
  before(:each) do
    @hsh = RCHash.new
    @hsh[1] = 2
    @hsh[3] = 4
    @hsh[5] = 6
    @empty = RCHash.new
  end

  ruby_version_is "1.8.7" do
    it "raises a LocalJumpError when called on a non-empty hash without a block" do
      lambda { @hsh.send(@method) }.should raise_error(LocalJumpError)
    end

    it "returns the receiver when called on an empty hash without a block" do
      @empty.send(@method).should eql(@empty)
    end
  end
end
