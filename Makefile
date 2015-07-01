IDL=idl

.PHONY: doc userdoc clean


doc:
	$(IDL) -e 'comp_util_make_docs'

userdoc:
	$(IDL) -e 'comp_util_make_docs, /user'

clean:
	rm -rf api-docs
	rm -rf api-userdocs
