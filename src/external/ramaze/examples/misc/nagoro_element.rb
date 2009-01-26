require 'rubygems'
require 'ramaze'

class MainController < Ramaze::Controller
  map '/'

  engine :Nagoro

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

Nagoro.element 'Page' do |content, attrs|
  %{
   <html>
    <head>
      <title>examples/element</title>
    </head>
    <body>
      <h1>#{attrs['title']}</h1>
      #{content}
    </body>
  </html>
  }
end

Nagoro.element 'SideBar' do |content, attrs|
  %{
   <div class="sidebar">
     <a href="http://something.com">something</a>
   </div>
   }
end

Ramaze.start
