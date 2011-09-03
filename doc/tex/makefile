NOM=cahier_des_charges

all: compilation

lecture: compilation
	evince ../$(NOM).pdf

compilation: $(NOM).tex
	pdflatex $(NOM).tex
	mv $(NOM).pdf ../
