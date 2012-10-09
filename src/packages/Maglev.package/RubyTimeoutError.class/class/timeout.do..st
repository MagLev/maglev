as yet unclassified
timeout: aNumber do: aBlock
	|sem process val|
	sem := Semaphore new.
	process :=
		[val := aBlock value.
		sem signal] fork.
	(sem waitForSeconds: aNumber) ifFalse:
		[process terminate.
		self signal].
	^ val

