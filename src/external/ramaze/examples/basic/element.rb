require 'rubygems'
require 'ramaze'

class Page < Ezamar::Element
  def render
    %{
     <html>
      <head>
        <title>examples/element</title>
      </head>
      <body>
        <h1>#{@title}</h1>
        #{content}
      </body>
    </html>
    }
  end
end

class SideBar < Ezamar::Element
  def render
    %{
     <div class="sidebar">
       <a href="http://something.com">something</a>
     </div>
     }
  end
end

class MainController < Ramaze::Controller
  map '/'

  def index
    %{
    <Page title="Test">
      <SideBar />
      <p>
        Hello, World!
      </p>
    </Page>
    }
  end
end

Ramaze.start