# rack-0.4.0/lib/rack/utils.rb (from the Rack distribution) defines a class
# that derives from Proc. A limited subset of Rack does load and run, but
# their lobster demo does not.

class Context < Proc

end
