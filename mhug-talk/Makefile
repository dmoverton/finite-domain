LATEXMK=latexmk -pdf -e "\$$pdflatex = 'pdflatex --shell-escape %O %S'"
TGT=constraints
.PHONY: FORCE_MAKE
all: $(TGT).pdf
%.pdf: %.tex FORCE_MAKE
	$(LATEXMK) $<

.PHONY: pvc
pvc:
	$(LATEXMK) -pvc -e "\$$pdf_previewer = 'start evince %O %S'" $(TGT).tex
