include ../support/include.mk

APPLICATION=mochiweb

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

test: all
	$(ERL) -noshell -pa ../ebin -s $(APPLICATION) test -s init stop
