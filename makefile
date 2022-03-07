run:
	node compiler
	mkdir -p out
	nasm -f elf64 out/out.asm -o out/out.o
	ld out/out.o -o out/out
	./out/out

clean:
	rm -r out
