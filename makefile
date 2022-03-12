test:
	nasm -f elf64 test.asm -o out/test.o
	ld out/test.o -o out/test
	./out/test

# https://stackoverflow.com/questions/20737947/how-to-generate-a-nasm-compilable-assembly-code-from-c-source-code-on-linux
testc:
	gcc -fno-asynchronous-unwind-tables -s -c -o testc.o test.c
	objconv -fnasm testc.o
	cat ./testc.asm
compile:
	node compiler
	mkdir -p out
	nasm -f elf64 out/out.asm -o out/out.o
	ld out/out.o -o out/out
	./out/out

clean:
	rm -r out
