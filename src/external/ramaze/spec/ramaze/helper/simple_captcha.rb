require 'spec/helper'
require 'ramaze/helper/simple_captcha'

describe Ramaze::Helper::SimpleCaptcha do
  extend Ramaze::Helper::SimpleCaptcha

  # mock session
  SESSION = {}
  def session; SESSION; end

  should 'generate question and answer' do
    simple_captcha
    question = SESSION[:CAPTCHA][:question]
    question.should =~ /^\d+ [+-] \d+$/
    lh, m, rh = question.split

    answer = SESSION[:CAPTCHA][:answer]
    answer.should =~ /^\d+$/

    lh.to_i.send(m, rh.to_i).should == answer.to_i
  end
end
