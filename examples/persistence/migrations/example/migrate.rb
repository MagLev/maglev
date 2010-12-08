
Maglev.persistent do
  class BlogPost
    # Create one Time object that we use to indicate the
    # "Unknown Publishing Date".  Set the reporting methods
    # to report "Unknown".
    UNKNOWN_TIME = Time.at(0)
    class << UNKNOWN_TIME
      def strftime(*args) "Unknown" end
      alias to_s    strftime
      alias inspect strftime
    end
    UNKNOWN_TIME.freeze

    def self.migrate_10_to_11
      BlogPost.all_posts.each do |post|
        post.instance_variable_set(:@date, UNKNOWN_TIME) if post.date.nil?
      end
      Maglev.commit_transaction  # commit data changes
    end
  end
end
Maglev.commit_transaction  # commit code changes

BlogPost.migrate_10_to_11  # execute migration
