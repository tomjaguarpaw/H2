.PHONY: build deploy

build: site
	./site build

deploy: build
	rsync --itemize-changes -vr _site/ `cat host`

upload: deploy

site: h2.cabal site.hs
	cabal v2-install --installdir . --overwrite-policy=always site

watch:
	while true; do inotifywait --event modify posts; make build; done

serve-locally:
	python3 -m http.server --directory _site
