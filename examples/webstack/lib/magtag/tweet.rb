 class Tweet
   attr_reader :text, :date

   # Create a new tweet
   #
   # @param [String] text the text of the tweet.
   # @raises [ArgumentError] if text.length > 140
   # @return [Tweet] the new tweet
   def initialize(text)
     raise ArgumentError, "Keep it short, buddy" if text.length > 140
     @text = text
     @date = Time.now
   end

   def twitterize_date(reference_date=Time.now)
     seconds_ago = (reference_date - @date).round
     case seconds_ago
     when 0..59
       "#{seconds_ago} seconds ago"
     when 60..3600
       "#{(seconds_ago / 60).round} minutes ago"
     when 3600..86400
       "#{(seconds_ago / 3600).round} hours ago"
     else
       "#{(seconds_ago / 86400).round} days ago"
     end
   end
 end
