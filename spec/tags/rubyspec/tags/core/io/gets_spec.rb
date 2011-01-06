fails:IO#gets assigns the returned line to $_
fails:IO#gets returns nil if called at the end of the stream
fails:IO#gets raises IOError on closed stream
fails:IO#gets with no separator returns the next line of string that is separated by $/
fails:IO#gets with no separator returns tainted strings
fails:IO#gets with no separator updates lineno with each invocation
fails:IO#gets with no separator updates $. with each invocation
fails:IO#gets with nil separator returns the entire contents
fails:IO#gets with nil separator returns tainted strings
fails:IO#gets with nil separator updates lineno with each invocation
fails:IO#gets with nil separator updates $. with each invocation
fails:IO#gets with an empty String separator returns the next paragraph
fails:IO#gets with an empty String separator reads until the beginning of the next paragraph
fails:IO#gets with an empty String separator returns tainted strings
fails:IO#gets with an empty String separator updates lineno with each invocation
fails:IO#gets with an empty String separator updates $. with each invocation
fails:IO#gets with an arbitrary String separator reads up to and including the separator
fails:IO#gets with an arbitrary String separator returns tainted strings
fails:IO#gets with an arbitrary String separator updates lineno with each invocation
fails:IO#gets with an arbitrary String separator updates $. with each invocation
