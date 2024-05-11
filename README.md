## use
to compile your bf code just type 

`guile bf.scm <filename>.bf > <filename>.S`

`as head.S -o head.o && as <filename>.S -o <filename>.o`

`ld -lSystem -o <filename> head.o <filename>.o -e _start`