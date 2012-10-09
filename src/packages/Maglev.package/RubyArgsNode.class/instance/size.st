converting
size
	| count |
	count := 0.
	arguments ifNotNil: [arguments do: [:ea | count := count + 1]].
	^ count

