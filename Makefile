.PHONY: build deploy

build: site
	cabal run -- site build

deploy: build
	rsync --itemize-changes -vr _site/ `cat host`

upload: deploy

site: h2.cabal site.hs
	cabal build

watch: site
	cabal run -- site watch
