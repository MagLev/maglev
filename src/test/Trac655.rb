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

class CM
  def middleware
    @middleware ||= begin
                    m = Hash.new
                    m["development"] = :foo
                    m
                  end
  end
end

x = CM.new.middleware
unless x == { 'development' => :foo } ; raise 'error'; end
puts "OK"
true
