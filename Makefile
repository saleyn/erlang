MASTER=main

all: info

info:
	@echo "make gh-pages     - Make gh-pages from the main branch"

gh-pages:
	@# The git config params must be set when this target is executed by a GitHub workflow
	@[ -z "$(git config user.name)" ] && \
		git config user.name  github-actions
		git config user.email github-actions@github.com
	@if git branch | grep -q gh-pages ; then \
    git checkout gh-pages; \
  else \
    git checkout -b gh-pages; \
  fi
	git checkout $(MASTER) -- assets _layouts scripts *.md
	@FILES=`git status -uall --porcelain | sed -n '/^.. [A-Za-z0-9]/{s/.. //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; git checkout $(MASTER) && echo 'Switched to $(MASTER)'; exit $$ret"
