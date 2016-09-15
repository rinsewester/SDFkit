TARGET = paper_final
PLTX = pdflatex -halt-on-error --shell-escape

paper:
	$(PLTX) $(TARGET)
	
full:
	$(PLTX) $(TARGET)
	bibtex $(TARGET)
	$(PLTX) $(TARGET)
	$(PLTX) $(TARGET)
	
clean:
	rm -f *~ *.aux *.bbl *.log $(TARGET).pdf *.blg *.ptb

