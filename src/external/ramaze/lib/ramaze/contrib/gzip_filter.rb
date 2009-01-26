#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

# gzip_filter.rb
#
# Use this to compress "large" pages with gzip.  All major browsers support
# gzipped pages.
# This filter brought to you by your friends in #ramaze:
# Pistos, manveru, rikur and Kashia.
#
# Usage, in start.rb:
#
#   require 'ramaze/contrib/gzip_filter'
#   Ramaze::Dispatcher::Action::FILTER << Ramaze::Filter::Gzip
#
# Setting options (at any point in your code):
#
#   Ramaze::Filter::Gzip.trait(
#     :threshold    => 1024,
#     :content_type => /^text\/.+/
#   )

require 'zlib'

module Ramaze
  module Filter
    class Gzip

      trait :enabled => true,
            :content_type => /^text\/.+/,
            :threshold => 32768 # bytes

      class << self

        include Ramaze::Trinity

        # Enables being plugged into Dispatcher::Action::FILTER

        def call( response, options = {} )
          return response unless trait[ :enabled ]
          return response unless body = response.body
          return response if body.respond_to?( :read )

          accepts = request.env[ 'HTTP_ACCEPT_ENCODING' ]
          return response if accepts.nil? || ( accepts !~ /(x-gzip|gzip)/ )

          acceptable_size = body.size >= trait[ :threshold ]
          acceptable_type = response.content_type =~ trait[:content_type]

          if acceptable_type and acceptable_size
            output = StringIO.new
            def output.close
              # Zlib closes the file handle, so we want to circumvent this
              rewind
            end
            gz = Zlib::GzipWriter.new( output )
            gz.write( body )
            gz.close

            response.body = output.string
            response.header[ 'Content-encoding' ] = 'gzip'
          end

          response
        end
      end
    end
  end
end
