all:
	(cd deps/mochiweb-src; make; cd -; mkdir -p ebin; cd src; $(MAKE))

clean:
	(cd src; $(MAKE) clean)
