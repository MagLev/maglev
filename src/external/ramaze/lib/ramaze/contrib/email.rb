# EmailHelper can be used as a simple way to send basic e-mails from your app.
#
# Usage:
#
#   require 'ramaze/contrib/email'
#
#   # Set the required traits:
#   Ramaze::EmailHelper.trait :smtp_server      => 'smtp.your-isp.com',
#                             :smtp_helo_domain => "originating-server.com",
#                             :smtp_username    => 'username',
#                             :smtp_password    => 'password',
#                             :sender_address   => 'no-reply@your-domain.com'
#
#   # Optionally, set some other traits:
#   Ramaze::EmailHelper.trait :smtp_auth_type => :login,
#                             :bcc_addresses  => [ 'admin@your-domain.com' ],
#                             :sender_full    => 'MailBot <no-reply@your-domain.com>',
#                             :id_generator   => lambda { "<#{Time.now.to_i}@your-domain.com>" },
#                             :subject_prefix => "[SiteName]"
#
# To send an e-mail:
#
#   Ramaze::EmailHelper.send(
#     "foo@foobarmail.com",
#     "Your fooness",
#     "Hey, you are very fooey!"
#   )

require 'net/smtp'

module Ramaze
  class EmailHelper
    # Required to be set
    trait :smtp_server => 'smtp.your-isp.com'
    trait :smtp_helo_domain => 'your.helo.domain.com'
    trait :smtp_username => 'no-username-set'
    trait :smtp_password => ''
    trait :sender_address => 'no-reply@your-domain.com'

    # Optionally set
    trait :smtp_port => 25
    trait :smtp_auth_type => :login
    trait :bcc_addresses => []
    trait :sender_full => nil
    trait :id_generator => lambda { "<" + Time.now.to_i.to_s + "@" + trait[ :smtp_helo_domain ] + ">" }
    trait :subject_prefix => ""

    class << self
      def send(recipient, subject, message)
        {:recipient => recipient, :subject => subject, :message => message}.each do |k,v|
          if v.nil? or v.empty?
            raise(ArgumentError, "EmailHelper error: Missing or invalid #{k}: #{v.inspect}")
          end
        end
        sender = trait[:sender_full] || "#{trait[:sender_address]} <#{trait[:sender_address]}>"
        subject = [trait[:subject_prefix], subject].join(' ').strip
        id = trait[:id_generator].call
        email = %{From: #{sender}
To: <#{recipient}>
Date: #{Time.now.rfc2822}
Subject: #{subject}
Message-Id: #{id}

#{message}
}

        send_smtp( email, recipient, subject )
      end

      # the raw mail sending method used by Ramaze::EmailHelper

      def send_smtp( email, recipient, subject )
        options = trait.values_at(:smtp_server, :smtp_port, :smtp_helo_domain,
                                  :smtp_username, :smtp_password, :smtp_auth_type)

        Net::SMTP.start( *options ) do |smtp|
          smtp.send_message( email, trait[ :sender_address ], Array[ recipient, *trait[ :bcc_addresses ] ] )
          Log.info "E-mail sent to #{recipient} - '#{subject}'"
        end
      rescue => e
        Log.error "Failed to send e-mail to #{recipient}"
        Log.error [ e.class.to_s, e.message, *e.backtrace ].join( "\t\n" )
      end
    end
  end
end
