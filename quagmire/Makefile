UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

objs = \
	main.o \
	values.o \
	print.o \
	string.o \
	symbol.o \
	io.o \
	errors.o \

default: runtime.o

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

%.run: %.o runtime.o
	gcc -no-pie runtime.o $< -o $@

.c.o:
	gcc -no-pie -fPIC -c -g -o $@ $<

.s.o:
	nasm -g -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run

test: example.run
	@test "$(shell ./example.run)" = "$(shell racket example.rkt)"
