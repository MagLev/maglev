# ======================================================================
# Add @date to our app
# ======================================================================

Maglev.persistent do
  class BlogPost
    VERSION = '1.1.0'

    def self.all_posts
      Maglev::PERSISTENT_ROOT[:BlogPost]
    end
           
    attr_reader :title, :text, :date

    def initialize(title, text, date=Time.now)
      @title = title
      @text  = text
      @date  = date
    end
  end
end
Maglev.commit_transaction
