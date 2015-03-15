APPLICATIONS = \
	eserver \

#	eclient \
#	einterceptor \

all: applications
applications: $(APPLICATIONS)

define Application
$(1): $(1)/Emakefile
	cd $(1) && \
	erl -make && \
	cd ..;

$(1)/Emakefile: Emakefile
	cp $$^ $$@

$(1)/ebin/$(1).app: $(1)/src/$(1).app.src
	cp $$^ $$@

$(1)/ebin/$(1).rel: $(1)/src/$(1).rel.src
	cp $$^ $$@

$(1)_run: $(1) $(1)/ebin/$(1).app
	erl -pa $(1)/ebin/ -eval "application:start($(1),permanent)" -noshell
#	erl -pa $(1)/ebin/ -run client_server sketch -run init stop -noshell

$(1)_release: $(1)/ebin/$(1).rel
	erl -pa $(1)/ebin/ -eval "systools:make_script(\"$(1)\")" -run init stop -noshell

endef

$(foreach app,$(APPLICATIONS),$(eval $(call Application,$(app))))

.PHONY: all check $(APPLICATIONS)
