.PHONY: build deploy

build: site
	cabal run --constraint="hakyll +previewserver" -- site build

deploy: build
	rsync --itemize-changes -vr _site/ `cat host`

upload: deploy

site: h2.cabal site.hs
	cabal build --constraint="hakyll +previewserver"

watch: site
	cabal run --constraint="hakyll +previewserver" -- site watch
