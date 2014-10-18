#!/bin/bash
while :; do
	echo -n "Check: "
	if [ inputArray.txt -nt pokus.svg ]; then
		echo regenerating;
		sleep .2;
		./t2e inputArray.txt | ./main pokus;
		neato -O -Tpng pokus.gv;
	else
		echo nop;
	fi;
	sleep 1;
done
