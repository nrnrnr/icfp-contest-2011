all:V:
	echo 'CM.make "ltg.cm";' | sml

mlton:V:
	mlton -verbose 1 ltg.mlb
