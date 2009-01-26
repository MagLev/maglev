module Ramaze
  module Helper
    module Gravatar

      # fetches a gravatar from http//www.gravatar.com based on 'email'
      # and 'size'. Falls back to 'fallback_path' if no gravatar is found.
      # default 'fallback_path' is "/images/gravatar_default.jpg".
      # example:
      #
      # class GravatarController < Ramaze::Controller
      #    helper :gravatar
      #
      #    def index
      #      @gravatar_thumbnail_src = gravatar(session[:email] || 'riku@helloit.fi')
      #    end
      # end
      #
      #  /view/gravatar/index.html:
      #  <img src="#{@gravatar_thumbnail_src}" />

      def gravatar(email, size = 32, fallback_path = "/images/gravatar_default.jpg")
        emailhash = Digest::MD5.hexdigest(email)

        fallback = Request.current.domain
        fallback.path = fallback_path
        default = Rack::Utils.escape(fallback.to_s)

        return "http://www.gravatar.com/avatar.php?gravatar_id=#{emailhash}&default=#{default}&size=#{size}"
      end
    end
  end
end
