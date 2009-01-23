# Extract the benchmark names and execution times from a benchmark run,
# dropping all the text normally output by the benchmark. Put the results
#  in a format that can be loaded directly by a spreadsheet program.
#
# NOTE: Should be rewritten in Ruby

/==> Start benchmark/ {
	bm_category = $NF
	bm_name = $NF
	next
}


/==> End benchmark/ {
	$0 = prev
	gsub ("\\(","")
	gsub ("\\)","")
	printf (bm_name)
	for (j = 1; j <= NF; j++)
        	printf ("\t%s", $j)
        printf ("\n")
	next
}

{ prev =$0 }
