all:
	make -C lib
	make -C src
	cp src/sigs_freq sigs_freq

clean:

	make -C src clean
	make -C lib clean
	rm sigs_freq
