
require 'rack/request'
require 'rack/response'

module Rack
  # Paste has a Pony, Rack has a Lobster!
  class Lobster
    LobsterString = <<'EOS'
                         ,.---._
               ,,,,     /       `,
                \\\\   /    '\_  ;
                 |||| /\/``-.__\;'
                 ::::/\/_
 {{`-.__.-'(`(^^(^^^(^ 9 `.========='
{{{{{{ { ( ( (  (   (-----:=
 {{.-'~~'-.(,(,,(,,,(__6_.'=========.
                 ::::\/\
                 |||| \/\  ,-'/,
                ////   \ `` _/ ;
               ''''     \  `  .'
                         `---'
EOS

    LambdaLobster = lambda { |env|
      if env["QUERY_STRING"].include?("flip")
        lobster = LobsterString.split("\n").
          map { |line| line.ljust(42).reverse }.
          join("\n")
        href = "?"
      else
        lobster = LobsterString
        href = "?flip"
      end

      [200, {"Content-Type" => "text/html"},
       ["<title>Lobstericious!</title>",
        "<pre>", lobster, "</pre>",
        "<a href='#{href}'>flip!</a>"]
      ]
    }

    def call(env)
      req = Request.new(env)
      if req.GET["flip"] == "left"
        lobster = LobsterString.split("\n").
          map { |line| line.ljust(42).reverse }.
          join("\n")
        href = "?flip=right"
      elsif req.GET["flip"] == "crash"
        raise "Lobster crashed"
      else
        lobster = LobsterString
        href = "?flip=left"
      end

      Response.new.finish do |res|
        res.write "<title>Lobstericious!</title>"
        res.write "<pre>"
        res.write lobster
        res.write "</pre>"
        res.write "<p><a href='#{href}'>flip!</a></p>"
        res.write "<p><a href='?flip=crash'>crash!</a></p>"
      end
    end

  end
end

if $0 == __FILE__
  require 'rack'
  require 'rack/showexceptions'
  Rack::Handler::WEBrick.run \
    Rack::ShowExceptions.new(Rack::Lint.new(Rack::Lobster.new)),
    :Port => 9292
end
