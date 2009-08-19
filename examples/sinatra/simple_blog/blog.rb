# This file defines the blog classes

Maglev.persistent do
  class Post
    # The key in Maglev::PERSISTENT_ROOT for storing posts
    POSTS_KEY = :posts
    @id = -1

    def self.new(params)
      p = allocate
      p.initialize(new_id, params)
      add(p)
      p
    end

    def self.all_posts
      Maglev::PERSISTENT_ROOT[POSTS_KEY]
    end

    def self.get(id)
      Maglev::PERSISTENT_ROOT[POSTS_KEY][id.to_i]
    end

    def self.new_id
      Maglev.persistent { @id += 1 }
    end

    def self.add(post)
      all_posts[post.id] = post
    end

    attr_reader :text, :title, :id
    def initialize(id, params)
      @title = params[:title]
      @text =  params[:text]
      @id = id.to_i
    end
  end
end

Maglev::PERSISTENT_ROOT[Post::POSTS_KEY] = Array.new

Maglev.commit_transaction
puts "== Committed class Post"
