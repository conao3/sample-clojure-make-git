.PHONY: all
all:

define REPL_DEPS
{:deps
  {nrepl/nrepl {:mvn/version "1.3.1"}
   cider/cider-nrepl {:mvn/version "0.55.7"}}}
endef
export REPL_DEPS

define REPL_MIDDLEWARE
cider.nrepl/cider-middleware,
endef
export REPL_MIDDLEWARE

.PHONY: repl
repl:
	clj -Sdeps "$${REPL_DEPS}" -M -m nrepl.cmdline --interactive --middleware "[$${REPL_MIDDLEWARE}]"

.PHONY: uber
uber: clean
	clj -T:build uber

.PHONY: clean
clean:
	rm -rf target
