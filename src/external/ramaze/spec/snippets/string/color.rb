require 'lib/ramaze/spec/helper/snippets'

describe 'String#color' do
  it 'should define methods to return ANSI strings' do
    %w[reset bold dark underline blink negative
    black red green yellow blue magenta cyan white].each do |m|
      "string".respond_to? m
      "string".send(m).should.match(/\e\[\d+mstring\e\[0m/)
    end
  end
end
