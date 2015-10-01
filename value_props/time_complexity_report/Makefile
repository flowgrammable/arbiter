all: report.pdf

report.pdf: report.tex algorithms.tex intrinsics.tex
	latexmk -pdf report.tex

clean:
	latexmk -C