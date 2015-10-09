IDL=idl

.PHONY: doc userdoc env clean help

IDLDOC_PATH="+${HOME}/projects/idldoc"
COMP_UTIL_PATH="lib:src:ssw:${IDLDOC_PATH}:<IDL_DEFAULT>"


doc:
	$(IDL) -IDL_STARTUP '' -IDL_PATH ${COMP_UTIL_PATH} -e 'comp_util_make_docs'

userdoc:
	$(IDL) -IDL_STARTUP '' -IDL_PATH ${COMP_UTIL_PATH} -e 'comp_util_make_docs, /user'

env:
	$(IDL) -IDL_STARTUP '' -IDL_PATH ${COMP_UTIL_PATH}

clean:
	rm -rf api-docs
	rm -rf api-userdocs

help:
	@echo "Makefile targets:"
	@echo "  doc       generate developer documentation"
	@echo "  userdoc   generate user documentation"
	@echo "  clean     remove generated documentation"
	@echo "  help      print this message"