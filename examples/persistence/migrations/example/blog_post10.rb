# ======================================================================
# First version of Blog app
# ======================================================================

Maglev.persistent do
  class BlogPost
    VERSION = '1.0.0'

    def self.all_posts
      Maglev::PERSISTENT_ROOT[:BlogPost]
    end

    attr_reader :title, :text

    def initialize(title, text)
      @title = title
      @text  = text
    end
  end
end

# Initialize the posts store
Maglev::PERSISTENT_ROOT[:BlogPost] = []

Maglev.commit_transaction
