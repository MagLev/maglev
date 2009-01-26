require 'spec/helper'

spec_require 'ruby-prof'

Ramaze.contrib :profiling

class MainController < Ramaze::Controller
  def index
    100.times {"h" + "e" + "l" + "l" + "o"}
  end
end

output = StringIO.new
Ramaze::Log.loggers << Ramaze::Logger::Informer.new(output)

describe 'Profiling' do
  behaves_like "http"
  ramaze :public_root => __DIR__(:public)

  it "should profile" do
    output = StringIO.new
    Ramaze::Log.loggers << Ramaze::Logger::Informer.new(output)

    get('/')
    output.string.should =~ /Thread ID:\s-?\d+/
    output.string.should =~ /Total:/
    output.string.should =~ /self\s+total\s+self\s+wait\s+child\s+call/
  end
end
