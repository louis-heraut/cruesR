all: doc
.PHONY: doc install check

doc:
	R -e 'devtools::document()'

install:
	R -e "super-lou/cruesR"

check:
	R -e 'devtools::check()'

