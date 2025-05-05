.PHONY: all
all: native

define REPL_DEPS
{:deps
  {nrepl/nrepl {:mvn/version "RELEASE"}
   cider/cider-nrepl {:mvn/version "RELEASE"}}}
endef
export REPL_DEPS

define REPL_MIDDLEWARE
cider.nrepl/cider-middleware,
endef
export REPL_MIDDLEWARE

.PHONY: repl
repl:
	clj -A:dev -Sdeps "$${REPL_DEPS}" -M -m nrepl.cmdline --interactive --middleware "[$${REPL_MIDDLEWARE}]"

.PHONY: update
update:
	clojure -Sdeps '{:deps {com.github.liquidz/antq {:mvn/version "RELEASE"}}}' -M -m antq.core --upgrade --force

.PHONY: uber
uber: target/mgit-standalone.jar

target/mgit-standalone.jar:
	clojure -T:build uber

.PHONY: native
native: target/mgit

target/mgit: target/mgit-standalone.jar
	native-image -jar $< \
	--features=clj_easy.graal_build_time.InitClojureClasses \
	--verbose \
	--no-fallback \
	$@

.PHONY: clean
clean:
	rm -rf target
