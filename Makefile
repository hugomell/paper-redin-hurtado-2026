all:
	rm -rf docs/
	mkdir docs
	cp -r _site/latest/* docs/
	cp -r _site/v1 docs/
