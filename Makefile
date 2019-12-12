.PHONY: build deploy

build: site
	./site build

deploy:	build
	rsync --itemize-changes -vr _site/ `cat host`

site: h2.cabal site.hs
	cabal v2-install --installdir . --overwrite-policy=always site
