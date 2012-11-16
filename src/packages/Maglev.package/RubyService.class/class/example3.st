examples
example3
	| ruby msg html fromAddress toAddress netSmtp |
	ruby := RubyService new.
	ruby
		require: 'rubygems';
		require: 'mailfactory';
		require: 'rdiscount';
		require: 'net/smtp'.

	msg := (ruby resolve: 'MailFactory') new.
	html := ((ruby resolve: 'RDiscount') new: '## MagLev
This message was rendered from Gemstone/S, using the Ruby C-extension RDiscount via *MagLev*.') to_html.
	msg
		to: (toAddress := 'fred@here.com');
		from: (fromAddress := 'user@localhost');
		html: html;
		add_attachment: ((RubyEnv _getenv: 'MAGLEV_HOME'), '/README.rdoc') _: 'text/plain'.
	
	netSmtp := (ruby resolve: 'Net::SMTP').
	netSmtp start: 'localhost' do: [:smtp | smtp send_message: msg to_s _: fromAddress _: toAddress].
	^ msg to_s

