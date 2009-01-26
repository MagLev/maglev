require 'uuid'
require 'md5'

module Ramaze
  module Helper
    module HttpDigest

      UUID_GENERATOR = UUID.new

      SESSION_NONCE = 'httpdigest_authentication_nonce'
      SESSION_OPAQUE = 'httpdigest_authentication_opaque'

      def httpdigest_logout
        session.delete( SESSION_NONCE )
        session.delete( SESSION_OPAQUE )
      end

      def httpdigest_headers uid, realm
        session[ SESSION_NONCE ] = UUID_GENERATOR.generate
        session[ SESSION_OPAQUE ][ realm ][ uid ] = UUID_GENERATOR.generate
        response['WWW-Authenticate'] =
          %|Digest realm="#{realm}",| +
          %|qop="auth,auth-int",| +
          %|nonce="#{session[SESSION_NONCE]}",| +
          %|opaque="#{session[SESSION_OPAQUE][realm][uid]}"|
      end

      def httpdigest(uid, realm)
        session[ SESSION_OPAQUE ] ||= {}
        session[ SESSION_OPAQUE ][ realm ] ||= {}

        if request.env['HTTP_AUTHORIZATION']

          authorized = false

          if session[ SESSION_NONCE ] and session[ SESSION_OPAQUE ][ realm ][ uid ]

            auth_split = request.env['HTTP_AUTHORIZATION'].split
            authentication_type = auth_split[0]
            authorization = Rack::Auth::Digest::Params.parse( auth_split[1..-1].join(' ') )

            digest_response, username, nonce, nc, cnonce, qop, opaque =
              authorization.values_at(*%w[response username nonce nc cnonce qop opaque])

            if authentication_type == 'Digest'
              if nonce == session[SESSION_NONCE] and opaque == session[SESSION_OPAQUE][realm][uid]
                h1 = nil
                if respond_to?( :httpdigest_lookup_password )  
                  ha1 = httpdigest_lookup_password( username )
                else
                  if respond_to?( :httpdigest_lookup_plaintext_password )
                    ha1 = MD5.hexdigest( "#{username}:#{realm}:#{httpdigest_lookup_plaintext_password( username )}" )
                  else
                    if block_given?
                      ha1 = yield( username )
                    else
                      raise "No password lookup handler found"
                    end
                  end
                end
                ha2 = MD5.hexdigest([request.request_method,request.fullpath].join(':'))
                md5 = MD5.hexdigest([ha1, nonce, nc, cnonce, qop, ha2].join(':'))

                authorized = digest_response == md5
              end
            end

          end

          unless authorized
            httpdigest_headers( uid, realm )
            respond('Unauthorized', 401)
          end

        else

          httpdigest_headers( uid, realm )
          httpdigest_failure if respond_to?( :httpdigest_failure )
          respond('Unauthorized', 401)

        end

        authorization["username"]
      end

    end
  end
end
