build: site
	./site build

site: site.hs
	ghc site.hs
