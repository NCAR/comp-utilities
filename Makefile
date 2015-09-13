IDL=idl

.PHONY: doc userdoc clean help


doc:
	$(IDL) -e 'comp_util_make_docs'

userdoc:
	$(IDL) -e 'comp_util_make_docs, /user'

clean:
	rm -rf api-docs
	rm -rf api-userdocs

help:
	@echo "Makefile targets:"
	@echo "  doc       generate developer documentation"
	@echo "  userdoc   generate user documentation"
	@echo "  clean     remove generated documentation"
	@echo "  help      print this message"