all:V:
	echo 'CM.make "ltg.cm";' |
    sml -Ccontrol.poly-eq-warn=false -Ccompiler-mc.error-non-exhaustive-match=true

mlton:V:
	mlton -verbose 1 ltg.mlb
