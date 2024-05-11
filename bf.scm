(import (rnrs programs (6))
        (rnrs base)
        (rnrs io ports)
        (rnrs io simple)
        (ice-9 format))

(define (add src label inc dec mul fmt cnt)
    (let* ((next (if (null? src) '() (cdr src)))
           (char (if (null? src) '() (car src))))
        (cond
          ((eq? inc char)
           (add next label inc dec mul fmt (+ cnt mul)))
          ((eq? dec char)
           (add next label inc dec mul fmt (- cnt mul)))
          (else 
            (if (= cnt 0)
                (parse src label)
                (format #f fmt cnt (parse src label)))))))

(define (until expected src)
    (if (null? src)
        src
        (if (eq? expected (car src))
            (cdr src)
            (until expected (cdr src)))))

(define (sqloop src label)
    (let ((n (random 1000000000000)))
      (format #f "l~d:\n~a\n"
              n (parse (cdr src) (cons n label)))))

(define (putc src label)
    (let ((next (if (null? src) '() (cdr src))))
        (format #f "\tleaq\t0x5(%rip),\t%r12\n\tjmp\t_putc\n~a" (parse next label))))

(define formattable 
    `((#\+ . ,(lambda (src label) (add src label #\+ #\- 1 "\taddb\t$~d,\t(%rsp)\n~a" 0)))
      (#\- . ,(lambda (src label) (add src label #\+ #\- 1 "\taddb\t$~d,\t(%rsp)\n~a" 0)))
      (#\> . ,(lambda (src label) (add src label #\> #\< 4 "\taddq\t$~d,\t%rsp\n~a" 0)))
      (#\< . ,(lambda (src label) (add src label #\> #\< 4 "\taddq\t$~d,\t%rsp\n~a" 0)))
      (#\. . ,putc)
      (#\[ . ,sqloop) 
      (#\] . ,(lambda (src label) (format #f "\tmovb\t(%rsp),\t%al\ncmpb\t$0, %al\n\tjnz\tl~d\n~a" (car label) (parse (cdr src) (cdr label)))))))

(define (parse src label)
    (if (null? src)
        ""
        (let ((proc (assoc (car src) formattable))
              (next (if (null? src) '() (cdr src))))
           (if (eq? #f proc)
                (parse next label)
                ((cdr proc) src label)))))

(display ".section        __TEXT,__text
.globl  _start
_start:
\tmovq\t%rsp,\t%rbx 
\tleaq\t_stack(%rip),\t%rsp
")
(display (call-with-input-file 
    (cadr (command-line))
    (lambda (in-port) 
        (parse (string->list (get-string-all in-port)) '()))))
(display "
\tmovq\t%rbx,\t%rsp
\txor\t%rax,\t%rax         
\tretq
")