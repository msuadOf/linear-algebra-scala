run:
	mill -i __.runMain Main

clean:
	-rm -rf obj_dir logs *.log *.dmp *.vpd coverage.dat build



lint reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat