all: info

info:
	@echo "make gh-pages     - Make gh-pages from the main branch"

gh-pages:
	@if git branch | grep -q gh-pages ; then \
    git checkout gh-pages; \
  else \
    git checkout -b gh-pages; \
  fi
	git checkout master -- *.md
	git checkout master -- assets/css/style.scss
	@FILES=`git status -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	exit 1
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout main && echo 'Switched to main'; exit $$ret"