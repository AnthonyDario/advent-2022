run: build
	out/main

build: clean
	ghc -i src/* -o out/main

clean:
	rm src/*.hi src/*.o
