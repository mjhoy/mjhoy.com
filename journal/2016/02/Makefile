all: libfoo.a
libfoo.a: foo.c foo.h
	cc -c -g foo.c
	ar rs libfoo.a foo.o
	@echo "built libfoo.a!"
clean:
	rm -f *.o libfoo.a
