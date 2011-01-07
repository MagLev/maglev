fails:String#each taints substrings that are passed to the block if self is tainted
fails:String#each raises a RuntimeError if the string is modified while substituting
fails:String#each returns an enumerator when no block given
