all:V:
	echo 'CM.make "ltg.cm";' |
    sml -Ccontrol.poly-eq-warn=false -Ccompiler-mc.error-non-exhaustive-match=true

mlton:V: sim

sim: `echo *.sml *.mlb`
	mlton -output $target -verbose 1 main.mlb
