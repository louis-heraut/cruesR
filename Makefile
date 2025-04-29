all: doc
.PHONY: doc install check

doc:
	R -e 'devtools::document()'

install:
	R -e "remotes::install_github('louis-heraut/cruesR')"

check:
	R -e 'devtools::check()'

