all:
	bash -c "cd lib/; make"
	bash -c "cd src/; make; cp sigs_freq ../sigs_freq"

clean:
	bash -c "cd lib/; make clean"
	bash -c "cd src/; make clean"
