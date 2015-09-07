###-----------------------------------------------------------------------------
### Application info
###-----------------------------------------------------------------------------

NAME := sched
VERSION := 0.1

###-----------------------------------------------------------------------------
### Modules
###-----------------------------------------------------------------------------

SOURCES = $(wildcard src/*.erl)
MODULES = $(SOURCES:src/%.erl=%)

###-----------------------------------------------------------------------------
### Flags
###-----------------------------------------------------------------------------

ERL_COMPILE_FLAGS := \
	+debug_info \
	+warn_export_vars \
	+warn_unused_import \
	+warn_missing_spec \
	+warn_untyped_record \
	+warnings_as_errors

###-----------------------------------------------------------------------------
### Targets
###-----------------------------------------------------------------------------

VERSION_HRL=src/$(NAME)_version.hrl

###-----------------------------------------------------------------------------

.PHONY: default
default: $(NAME)

$(NAME): $(MODULES:%=ebin/%.beam)
	@rm -f $@
	@echo " GEN  $@"
	@ln -s src/$@ $@

###-----------------------------------------------------------------------------

-include $(MODULES:%=ebin/%.Pbeam)

ebin/%.beam: src/%.erl Makefile | ebin $(VERSION_HRL)
	@echo " DEPS $<"
	@erlc -o ebin -MD -MG $<
	@echo " ERLC $<"
	@erlc $(ERL_COMPILE_FLAGS) -o ebin $<

$(VERSION_HRL): version
	@echo " GEN  $@"
	@src/versions $(VERSION) > $@.tmp
	@cmp -s $@.tmp $@ > /dev/null || cp $@.tmp $@
	@rm $@.tmp

.PHONY: version
version:

ebin:
	@echo " MKDIR $@"
	@mkdir $@

###-----------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf ebin $(VERSION_HRL) $(NAME)

.PHONY: distclean
distclean: clean
	rm -f .$(NAME)_plt

###-----------------------------------------------------------------------------

DIALYZER_APPS = erts kernel stdlib compiler crypto
DIALYZER_FLAGS = -Wunmatched_returns -Wunderspecs

.PHONY: dialyze
dialyze: .$(NAME)_plt default
	dialyzer --plt $< $(DIALYZER_FLAGS) ebin/*.beam

.$(NAME)_plt:
	dialyzer --build_plt --output_plt $@ --apps $(DIALYZER_APPS) $^
