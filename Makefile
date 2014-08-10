PROJECTS=rest-stringmap/ rest-gen/ rest-core/ rest-types/ rest-discovery/ rest-happstack/ rest-wai/ rest-snap/ code-builder/ rest-example/ rest-client/
INSTALL=cabal install $(PROJECTS)

all:
	$(INSTALL) $(CABAL_FLAGS)

dry-run:
	$(INSTALL) $(CABAL_FLAGS) --dry-run

init-sandbox:
	cabal sandbox init
	cabal sandbox add-source $(PROJECTS)
