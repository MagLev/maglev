# This migration adds Comments to the app.  The lines with something new
# are marked with "MIGRATION"

# TODO: The class defn should also go in the migrate up...
Maglev.abort_transaction
Maglev.persistent do
  class Post
    attr_reader :comments   # MIGRATION

    # This is a bit awkward, as we need to specify the whole method, so we
    # have to copy, paste and then add our little bit at the end.
    def initialize(params)
      @title     = params[:title]
      @text      = params[:text]
      @timestamp = params[:timestamp] || Time.now
      @tags      = params[:tags]      || []
      @comments  = params[:comments]  || []   # MIGRATION
    end

    def add_comment(comment)
      @comments << comment
    end
  end

  class Comment
    attr_reader :posted_by, :email, :url, :body
    def initialize(props)
      @posted_by = props[:posted_by]
      @email = props[:email]
      @url = props[:url]
      @body = props[:body]
    end
  end
end
Maglev.commit_transaction
puts "== Committed new code"
# How do we specify that we need to add a writer for comments?
#
# Define a method transiently, so it won't
class Post
  def comments=(x)
    puts "== #{self}.comments="
    @comments = x
  end
end

def migrate_up
  MDB::Server.server['theBlogPosts'].each do |post|
    post.instance_eval(@comments ||= [])
  end
  Maglev.commit_transaction
  puts "== Updated instances"
end

def migrate_down
  # TODO: Remove inst var for comments?
  Maglev.persistent do
    Post.remove_method :comments
    Post.remove_method :add_comment
  end
  Maglev.commit_transaction
end
