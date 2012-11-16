examples
example2
	"Demo using the C-ext rdiscount gem"
	| ruby rd markdown html |
	ruby := RubyService new.
	ruby loadGem: 'rdiscount'.

	rd := ruby resolve: 'RDiscount'.
	markdown := rd new: '## Hello World
                                       This is *awesome*'.
	html := markdown to_html.
	^ html

