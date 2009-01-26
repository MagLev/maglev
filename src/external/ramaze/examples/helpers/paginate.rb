require 'rubygems'
require 'ramaze'

USERS = DATA.read.split("\n")

class MainController < Ramaze::Controller
  helper :paginate

  trait :paginate => {
    :limit => 6,
    :var => 'user_page',
  }

  def index
    @pager = paginate(USERS)
%q~
<html>
  
  <head><title>Users</title></head>
  <body>
    #{@pager.navigation if @pager.needed?}
    <pre>
      <?r @pager.each do |user| ?>
        #{user}
      <?r end ?>
    </pre>
    #{@pager.navigation if @pager.needed?}
  </body>
</html>
~
  end
end

Ramaze.start

__END__
Michael Fellinger       1868 [ 77.13% ]
Aman Gupta               211 [  8.71% ]
Jonathan Buch             89 [  3.67% ]
Gabriele Renzi            49 [  2.02% ]
Ara T. Howard             44 [  1.82% ]
Clive Crous               37 [  1.53% ]
Keita Yamaguchi           27 [  1.11% ]
Ryan Grove                17 [  0.70% ]
Pistos                    16 [  0.66% ]
Wang, Jinjing             11 [  0.45% ]
Colin Shea                 9 [  0.37% ]
Sam Carr                   6 [  0.25% ]
Stephan Maka               5 [  0.21% ]
samcarr                    4 [  0.17% ]
raggi                      3 [  0.12% ]
Clinton R. Nixon           3 [  0.12% ]
Chris Duncan               3 [  0.12% ]
Richard Outten             2 [  0.08% ]
Andy Smith                 2 [  0.08% ]
skaar                      1 [  0.04% ]
Yasushi Abe                1 [  0.04% ]
Thomas Leitner             1 [  0.04% ]
Rob Lievaart               1 [  0.04% ]
Riku Raisaenen             1 [  0.04% ]
Matt Rubens                1 [  0.04% ]
Martin Hilbig              1 [  0.04% ]
Leo Borisenko              1 [  0.04% ]
Lars Olsson                1 [  0.04% ]
Jeremy Evans               1 [  0.04% ]
Jean-Francois Chevrette    1 [  0.04% ]
James Tucker               1 [  0.04% ]
Fabian Buch                1 [  0.04% ]
Christian Neukirchen       1 [  0.04% ]
Carlo Zottmann             1 [  0.04% ]
Andrew Farmer              1 [  0.04% ]
