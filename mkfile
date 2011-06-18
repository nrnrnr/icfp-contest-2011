all:V:
	echo 'CM.make "ltg.cm";' | sml -Ccontrol.poly-eq-warn=false

mlton:V:
	mlton -verbose 1 ltg.mlb
