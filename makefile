test:
	nasm -f elf64 test.asm -o out/test.o
	ld out/test.o -o out/test
	./out/test these are some args

# https://stackoverflow.com/questions/20737947/how-to-generate-a-nasm-compilable-assembly-code-from-c-source-code-on-linux
testc:
	gcc -fno-asynchronous-unwind-tables -s -c -o testc.o test.c
	objconv -fnasm testc.o
	cat ./testc.asm
runc:
	rm -f testc
	gcc test.c -o testc
	./testc
compile:
	rm -f ./out/out
	rm -f ./out/out.asm

	node --trace-uncaught compiler --compile
run:
	rm -f ./out/out
	rm -f ./out/out.asm

	node --trace-uncaught compiler
# mkdir -p out
# nasm -f elf64 out/out.asm -o out/out.o
# ld out/out.o -o out/out
# ./out/out these are some args

snake:
	rm -f ./out/out
	rm -f ./out/out.asm
	node --trace-uncaught compiler --compile
	stty raw -echo
	./out/out
	stty -raw echo

clean:
	rm -r out
