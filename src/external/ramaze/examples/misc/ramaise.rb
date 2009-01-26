# Ramaise, the Ramaze version of Reprise, a minimal hAtom blog
#
# http://redflavor.com/reprise.rb
# http://www.rubyinside.com/reprise-a-ruby-powered-blogging-app-in-
#                            100-lines-including-templates-646.html
#
# Usage:
#
#   1. gem install ramaze haml bluecloth rubypants -y
#   2. wget http://darcs.ramaze.net/ramaze/examples/ramaise.rb
#   3. mkdir entries
#   4. vi entries/YYYY.MM.DD.Title.Goes.Here
#   5. ruby ramaise.rb

%w(rubygems ramaze bluecloth rubypants haml).each{|lib| require lib }

class BlogPost
  DIR = __DIR__(:entries)

  def initialize filename
    raise 'Invalid BlogPost filename' unless File.exists?(filename)
    @filename, filename = filename, File.basename(filename)
    @date = Date.strptime(filename, '%Y.%m.%d').to_s
    @title = filename[11..-1].tr('.', ' ')
  end

  def body
    RubyPants.new(BlueCloth.new(File.read(@filename)).to_html).to_html
  end

  def slug
    @slug ||= title.gsub(/[^\w\s-]/, '').gsub(/\s+/, '-').downcase
  end

  attr_reader :date, :title

  class << self
    include Enumerable

    def each
      Dir[DIR/'*'].sort.reverse.each do |file| yield BlogPost.new(file) end
    end

    def [] key
      BlogPost.find{|post| post.slug == key }
    end
  end
end

class MainController < Ramaze::Controller

  TITLE = 'Ramaise'
  AUTHOR = { :name => 'Aman Gupta', :url => 'http://ramaze.net' }

  engine :Haml

  def index slug = nil
    if slug.nil?
      @posts = BlogPost.collect
      raise Ramaze::Error::NoAction,
            'No blog posts found, create
             entries/YYYY.MM.DD.My.First.Blog.Post' unless @posts.any?
    else
      raise Ramaze::Error::NoAction,
            'Invalid blog post' unless post = BlogPost[slug]
      @title = post.title
      @posts = [ post ]
    end

    %(
      %h1
        - if @title
          %a{ :href => '/' } #{TITLE}
        - else
          #{TITLE}

      - @posts.each do |post|
        .hentry
          %h2
            %abbr.updated{ :title => Time.parse(post.date).iso8601 }= post.date
            %a.entry-title{ :href => '/'+post.slug, :rel => 'bookmark' }= post.title
          .entry-content= post.body
    ).unindent
  end

  def error
    %(
      %h1 #{TITLE}: Resource not found
      %h2= Ramaze::Dispatcher::Error.current.message + '.'

      Go back to the
      %a{ :href => '/' } the front
      page.
    ).unindent
  end

  def layout
    %(
      !!!
      %html
        %head
          %title
            #{TITLE}
            - if @title
              = ': ' + @title
          %style{ :type => 'text/css' }
            :sass
              body
                font-size: 90%
                line-height: 1.4
                width: 94%
                margin: auto
              abbr
                border: 0
              .entry-content
                -moz-column-width: 30em
                -moz-column-gap: 1.5em
                -webkit-column-width: 30em
                -webkit-column-gap: 1.5em
              h2
                border-bottom: 0.05em solid #999
        %body
          = @content
          %address.author.vcard
            %a.url.fn{ :href => '#{AUTHOR[:url]}' } #{AUTHOR[:name]}
    ).unindent
  end
  layout :layout

end

Ramaze.start :sessions => false #, :adapter => :mongrel, :port => 3000
