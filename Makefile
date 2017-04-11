sos_warn:
	@echo "-----------------------------------------------------------------"
	@echo "        ! File watching functionality non-operational !          "
	@echo "                                                                 "
	@echo " Install steeloverseer to automatically run tasks on file change "
	@echo "                                                                 "
	@echo " See https://github.com/schell/steeloverseer                     "
	@echo "-----------------------------------------------------------------"

GHCID_SIZE ?= 8
# no name shadowing because not all abstractions are finished
GHCI_FLAGS ?= --ghci-options=-Wno-name-shadowing --ghci-options=-Wno-type-defaults

BUILD=stack build $(GHCI_FLAGS)
GHCI=stack ghci $(GHCI_FLAGS)

ghci:
	$(GHCI)

ghcid:
	ghcid --height=$(GHCID_SIZE) --topmost "--command=$(GHCI)"

hlint:
	if command -v sos > /dev/null; then sos -p 'app/.*\.hs' -p 'src/.*\.hs' \
	-c 'hlint \0'; else $(MAKE) sos_warn; fi

codex:
	if command -v sos > /dev/null; then sos -p '.*\.hs' \
	-c 'codex update --force'; else $(MAKE) entr_warn; fi

