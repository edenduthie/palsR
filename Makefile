
build: 
	R CMD build pals

install: 
	R CMD INSTALL pals

clean:
	rm -rf pals_*.tar.gz

