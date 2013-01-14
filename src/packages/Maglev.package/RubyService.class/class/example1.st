examples
example1
	"Demo creating a mail using the mailfactory gem"
	| ruby mf msg |
	ruby := RubyService new.
	ruby loadGem: 'mailfactory'.

	mf  := ruby resolve: 'MailFactory'.

	msg := mf new.
	msg to: 'fred@here.com'.
	"ruby rubyPerform: 'to=' withArguments: #('fred@here.com') on: msg."
	"ruby rubyPerform: #'html=' withArguments: #('<h1>Hi there</h1>') on: msg."
	msg html: 'fred@here.com'.
	^ msg to_s

