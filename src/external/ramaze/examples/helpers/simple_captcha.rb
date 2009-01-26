require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  helper :simple_captcha

  def index
    %|
<form action="/answer" method="POST">
  <fieldset>
    <legend>#{simple_captcha}</legend>
    <input type="text" name="answer" />
    <input type="submit" />
  </fieldset>
</form>
     |
  end

  def answer
    redirect_referrer unless request.post?

    answer = request[:answer]
    is = check_captcha(answer) ? 'Correct' : 'Wrong'

    "<h1>#{is}</h1><a href='/'>Back</a>"
  end
end

Ramaze.start
