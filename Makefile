CXX := erlc
ERL =erl

CFLAGS=
ESCRIPT=-eval "server_app:start()"

SRC=src
ODIR = build
OUT = -o $(ODIR)

_ERL_OBJS=$(wildcard $(SRC)/*.erl) 
_BEAM_OBJS=$(patsubst $(SRC)/%.erl,%.beam,$(_ERL_OBJS))
OBJS=$(patsubst %,$(ODIR)/%,$(_BEAM_OBJS))

build: $(OBJS)

run: build
	cd build; \
	$(ERL)

debug: CFLAGS += -Ddebug
debug: build

debug-run: CFLAGS += -Ddebug
debug-run: run

$(ODIR)/%.beam: $(SRC)/%.erl
	$(CXX) $(CFLAGS) $(OUT) $<

clean:
	rm -rf build/*

.PHONY: build clean
