.PHONY: build deploy

build: site
	./site build

deploy:	build
	rsync --itemize-changes -vr _site/ `cat host`

site: site.hs
	ghc site.hs
