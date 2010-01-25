# From Rack 1.1.0
#
# Maglev seems to assign a block to @middleware, rather than a Hash.
#
# def middleware
#   @middleware ||= begin
#                     m = Hash.new {|h,k| h[k] = []}
#                     m["deployment"].concat  [lambda {|server| server.server =~ /CGI/ ? nil : [Rack::CommonLogger, $stderr] }]
#                     m["development"].concat m["deployment"] + [["Rack::ShowExceptions"], ["Rack::Lint"]]
#                     m
#                   end
# end

def middleware
  @middleware ||= begin
                    m = Hash.new
                    m["development"] = :foo
                    m
                  end
end

x = middleware
p x.class        # MRI prints "Hash"   MagLev prints "ExecBlock"

y = x["development"]
p y.class        # MRI prints "Array"   MagLev prints "Hash"
