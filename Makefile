.PHONY : coverage

coverage :
	rm -rf _build/coverage \
		&& dune runtest --instrument-with bisect_ppx --force \
		&& bisect-ppx-report html \
		&& open ./_coverage/index.html
