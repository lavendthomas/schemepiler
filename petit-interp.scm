#! /usr/bin/env -S gsi -:dar

;;; Fichier : petit-interp.scm

;;; Ce programme est une version incomplete du TP2.  Vous devez uniquement
;;; changer et ajouter du code dans la premiere section.

;;; Notez que les commentaires ne contiennent pas d'accents car cela
;;; permettra une lecture egale dans tous les editeurs...

;;;----------------------------------------------------------------------------

;;; Vous devez modifier cette section.  La fonction parse-and-execute
;;; doit etre definie, et vous pouvez modifier et ajouter des
;;; definitions de fonction afin de bien decomposer le traitement a
;;; faire en petites fonctions.  Il faut vous limiter au sous-ensemble
;;; *fonctionnel* de Scheme dans votre codage (donc n'utilisez pas
;;; set!, set-car!, vector-set!, list-set!, begin, print, display,
;;; etc).

;; La fonction parse-and-execute recoit en parametre une liste des
;; caracteres qui constituent le programme a interpreter.  La
;; fonction retourne une chaine de caracteres qui sera imprimee comme
;; resultat final du programme.  S'il y a une erreur lors de
;; l'analyse syntaxique ou lors de l'execution, cette chaine de
;; caracteres contiendra un message d'erreur pertinent.  Sinon, la
;; chaine de caracteres sera l'accumulation des affichages effectues
;; par les enonces "print" executes par le programme interprete.

(define parse-and-execute
  (lambda (inp)
    (parse inp execute)))


;; This function parses the input and then print the S-expression that
;; was generated
(define parse-and-print
  (lambda (inp)
    (parse inp pp)))

;; La fonction next-sym recoit deux parametres, une liste de
;; caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole.  La continuation
;; sera appelee avec deux parametres, la liste des caracteres restants
;; (apres le symbole analyse) et le symbole qui a ete lu (soit un
;; symbole Scheme ou une chaine de caractere Scheme dans le cas d'un
;; <id> ou un entier Scheme dans le cas d'un <int>).  S'il y a une
;; erreur d'analyse (tel un caractere inapproprie dans la liste de
;; caracteres) la fonction next-sym retourne une chaine de caracteres
;; indiquant une erreur de syntaxe, sans appeler la continuation.

(define next-sym
  (lambda (inp cont)
    (cond ((null? inp)
           (cont inp 'EOI)) ;; retourner symbole EOI a la fin de l'input
          ((blanc? (@ inp))
           (next-sym ($ inp) cont)) ;; sauter les blancs
          (else
           (let ((c (@ inp)))
             (cond ((chiffre? c)   (symbol-int inp cont))
                   ((lettre? c)    (symbol-id inp cont))
                   ((char=? c #\() (cont ($ inp) 'LPAR))
                   ((char=? c #\)) (cont ($ inp) 'RPAR))
                   ((char=? c #\;) (cont ($ inp) 'SEMI))
                   ((char=? c #\{) (cont ($ inp) 'LBRA))
                   ((char=? c #\}) (cont ($ inp) 'RBRA))
                   ((char=? c #\+) (cont ($ inp) 'PLUS))
                   ((char=? c #\-) (cont ($ inp) 'MINS))
                   ((char=? c #\*) (cont ($ inp) 'TIME))
                   ((char=? c #\/) (cont ($ inp) 'DIVD))
                   ((char=? c #\%) (cont ($ inp) 'MODO))
                   ((char=? c #\<) (let ((cnext (@ ($ inp))))
                                     (if (char=? cnext #\=)
                                       (cont ($ ($ inp)) 'LE)
                                       (cont ($ inp) 'LT))))
                   ((char=? c #\>) (let ((cnext (@ ($ inp))))
                                     (if (char=? cnext #\=)
                                       (cont ($ ($ inp)) 'GE)
                                       (cont ($ inp) 'GT))))
                   ((char=? c #\=) (let ((cnext (@ ($ inp))))
                                     (if (char=? cnext #\=)
                                       (cont ($ ($ inp)) 'EQ)
                                       (cont ($ inp) 'AS))))
                   ((char=? c #\!) (let ((cnext (@ ($ inp))))
                                     (if (char=? cnext #\=)
                                       (cont ($ ($ inp)) 'NE)
                                       (syntax-err)))) ;; TODO trow error
                   (else
                    (syntax-err))))))))

;; La fonction @ prend une liste de caractere possiblement vide et
;; retourne le premier caractere, ou le caractere #\nul si la liste
;; est vide.

(define @
  (lambda (inp)
    (if (null? inp) #\nul (car inp))))

;; La fonction $ prend une liste de caractere possiblement vide et
;; retourne la liste des caracteres suivant le premier caractere s'il
;; y en a un.

(define $
  (lambda (inp)
    (if (null? inp) '() (cdr inp))))

;; La fonction syntax-err retourne le message d'erreur indiquant une
;; erreur de syntaxe.

(define syntax-err
  (lambda ()
    "syntax error\n"))

;; La fonction syntax-err retourne le message d'erreur indiquant une
;; erreur arithmetique.

(define arithmetic-err
   (lambda (reason)
      (begin
         (print (string-append "Arithmetic error: " reason "\n")))
         (exit)))

;; La fonction blanc? teste si son unique parametre est un caractere
;; blanc.

(define blanc?
  (lambda (c)
    (or (char=? c #\space) (char=? c #\newline) (char=? c #\tab))))

;; La fonction chiffre? teste si son unique parametre est un caractere
;; numerique.

(define chiffre?
  (lambda (c)
    (and (char>=? c #\0) (char<=? c #\9))))

;; La fonction lettre? teste si son unique parametre est une lettre
;; minuscule.

(define lettre?
  (lambda (c)
    (and (char>=? c #\a) (char<=? c #\z))))

(define strip->number
  (lambda (str)
      (if (number? str)
          str
          (let ((stripped (substring str 0 (- (string-length str) 1))))
            (string->number stripped)))))
      

;; La fonction symbol-int recoit deux parametres, une liste de
;; caracteres qui debute par un chiffre et une continuation.  La liste
;; de caracteres sera analysee pour en extraire le symbole <int>.  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole <int> analyse et le symbole
;; <int> qui a ete lu (un entier Scheme qui est la valeur numerique du
;; symbole <int>).

(define symbol-int
  (lambda (inp cont)
    (symbol-int-aux inp cont 0)))

(define symbol-int-aux
  (lambda (inp cont n)
    (if (chiffre? (@ inp))
        (symbol-int-aux ($ inp)
                        cont
                        (+ (* 10 n) (- (char->integer (@ inp)) 48)))
        (cont inp n))))

;; La fonction symbol-id recoit deux parametres, une liste de
;; caracteres qui debute par une lettre minuscule et une continuation.
;; La liste de caracteres sera analysee pour en extraire le prochain
;; symbole (soit un mot cle comme "print" ou un <id>).  La
;; continuation sera appelee avec deux parametres, la liste des
;; caracteres restants apres le symbole analyse et le symbole qui a
;; ete lu (soit un symbole Scheme, comme PRINT-SYM, ou une chaine de
;; caracteres Scheme qui correspond au symbole <id>).

(define symbol-id
  (lambda (inp cont)
    (symbol-id-aux inp cont '())))

(define symbol-id-aux
  (lambda (inp cont lst)
    (if (lettre? (@ inp))
        (symbol-id-aux ($ inp) cont (cons (@ inp) lst))
        (let ((id (list->string (reverse lst))))
          (cond ((string=? id "print")
                   (cont inp 'PRINT-SYM))
                ((string=? id "if")
                   (cont inp 'IF-SYM))
                ((string=? id "while")
                   (cont inp 'WHILE-SYM))
                ((string=? id "do")“Continuation
                   (cont inp 'DO-SYM))
                ((string=? id "else")
                   (cont inp 'ELSE-SYM))
                (else
                   (cont inp id)))))))

;; La fonction expect recoit trois parametres, un symbole, une liste
;; de caracteres et une continuation.  La liste de caracteres sera
;; analysee pour en extraire le prochain symbole qui doit etre le meme
;; que le premier parametre de la fonction.  Dans ce cas la
;; continuation sera appelee avec un parametre, la liste des
;; caracteres restants apres le symbole analyse.  Si le prochain
;; symbole n'est pas celui qui est attendu, la fonction expect
;; retourne une chaine de caracteres indiquant une erreur de syntaxe.

(define expect
  (lambda (expected-sym inp cont)
    (next-sym inp
              (lambda (inp sym)
                (if (equal? sym expected-sym)
                    (cont inp)
                    (syntax-err))))))

;; La fonction parse recoit deux parametres, une liste de caracteres
;; et une continuation.  La liste de caracteres sera analysee pour
;; verifier qu'elle est conforme a la syntaxe du langage.  Si c'est le
;; cas, la continuation sera appelee avec une S-expression qui
;; represente l'ASA du programme.  Sinon la fonction parse retourne
;; une chaine de caracteres indiquant une erreur de syntaxe.

(define parse
  (lambda (inp cont)
    (<program> inp ;; analyser un <program>
               (lambda (inp program)
                 (expect 'EOI ;; verifier qu'il n'y a rien apres
                         inp
                         (lambda (inp)
                           (cont program)))))))

;; Les fonctions suivantes, <program>, <stat>, ... recoivent deux
;; parametres, une liste de caracteres et une continuation.  La liste
;; de caracteres sera analysee pour verifier qu'elle debute par une
;; partie qui est conforme a la categorie correspondante de la
;; grammaire du langage.  Si c'est le cas, la continuation sera
;; appelee avec deux parametres : une liste des caracteres restants du
;; programme et une S-expression qui represente l'ASA de ce fragment
;; de programme.  Sinon ces fonctions retournent une chaine de
;; caracteres indiquant une erreur de syntaxe.

(define <program>
  (lambda (inp cont)
    (<stat> inp cont))) ;; analyser un <stat>

(define continue-stat
   (lambda (cont)
      (lambda (inp3 cont3)
         (next-sym inp3 (lambda (inp4 sym4)
                           (if (or (equal? sym4 'EOI) (equal? sym4 'RBRA)) ;; statements can be followed by enf of file or }
                              (cont inp3 cont3)
                              (<stat> inp3 (lambda (inp4 expr)
                                                (cont inp4 (list 'SEQ cont3 expr))))))))))

(define <stat>
  (lambda (inp cont)
    (next-sym inp
              (lambda (inp2 sym)
                (case sym ;; determiner quel genre de <stat>
                  ((PRINT-SYM)
                   (<print_stat> inp2 (continue-stat cont)))
                  ((WHILE-SYM)
                    (<while_stat> inp2 (continue-stat cont)))
                  ((IF-SYM)
                    (<if_stat> inp2 (continue-stat cont)))
                  (else
                   (<expr_stat> inp (continue-stat cont))))))))

(define <while_stat>
   (lambda (inp cont)
       (<paren_expr> inp
           (lambda (inp2 expr_par)
               (<stat> inp2
                   (lambda (inp3 expr_stat)
                       (cont inp3 (list 'WHILE expr_par expr_stat))))))))

(define <if_stat>
  (lambda (inp cont)
      (<paren_expr> inp
          (lambda (inp2 expr_par)
              (<bracket_stat> inp2
                  (lambda (inp3 expr_stat)
                      (cont inp3 (list 'IF expr_par expr_stat))))))))
              

(define <print_stat>
  (lambda (inp cont)
    (<paren_expr> inp ;; analyser un <paren_expr>
                  (lambda (inp expr)
                    (expect 'SEMI ;; verifier qu'il y a ";" apres
                            inp
                            (lambda (inp)
                              (cont inp
                                    (list 'PRINT expr))))))))

(define <paren_expr>
  (lambda (inp cont)
    (expect 'LPAR ;; doit debuter par "("
            inp
            (lambda (inp)
              (<expr> inp ;; analyser un <expr>
                      (lambda (inp expr)
                        (expect 'RPAR ;; doit etre suivi de ")"
                                inp
                                (lambda (inp)
                                  (cont inp
                                        expr)))))))))
                                        
(define <bracket_stat>
  (lambda (inp cont)
    (expect 'LBRA ;; must start with "{"
            inp
            (lambda (inp)
                    (<stat> inp
                            (lambda (inp expr)
                                    (expect 'RBRA ;; must end with "}"
                                            inp
                                            (lambda (inp)
                                                    (cont inp expr)))))))))

(define <expr_stat>
  (lambda (inp cont)
    (<expr> inp ;; analyser un <expr>
            (lambda (inp expr)
              (expect 'SEMI ;; doit etre suivi de ";"
                      inp
                      (lambda (inp)
                        (cont inp
                              (list 'EXPR expr))))))))

(define <expr>
  (lambda (inp cont)
    (next-sym inp ;; verifier 1e symbole du <expr>
              (lambda (inp2 sym1)
                (next-sym inp2 ;; verifier 2e symbole du <expr>
                          (lambda (inp3 sym2)
                            (if (and (string? sym1) ;; combinaison "id =" ?
                                     (equal? sym2 'AS))
                                (<expr> inp3
                                        (lambda (inp expr)
                                          (cont inp
                                                (list 'ASSIGN
                                                      sym1
                                                      expr))))
                                (<test> inp cont))))))))

(define <test>
  (lambda (inp cont)
    (<sum> inp
      (lambda (inp2 cont2)
        (next-sym inp2
          (lambda (inp3 sym)
            (cond ((equal? sym 'LT)
                    (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'LT cont2 expr)))))
                   ((equal? sym 'LE)
                      (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'LE cont2 expr)))))
                   ((equal? sym 'GT)
                      (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'GT cont2 expr)))))
                   ((equal? sym 'GE)
                      (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'GE cont2 expr)))))
                   ((equal? sym 'EQ)
                      (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'EQ cont2 expr)))))
                   ((equal? sym 'NE)
                      (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'NE cont2 expr)))))
                (else
                      (cont inp2 cont2)))))))))

(define <sum>
  (lambda (inp cont)
    (<mult> inp
      (lambda (inp2 cont2)
         (next-sym inp2
            (lambda (inp3 sym)
               (cond ((equal? sym 'PLUS)
                        (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'ADD cont2 expr)))))  ;; ADD prend toujours 2 termes
                  ((equal? sym 'MINS)
                        (<sum> inp3 (lambda (inp4 expr) (cont inp4 (list 'SUB cont2 expr))))) ;; SUB prend toujours 2 termes
                  (else
                        (cont inp2 cont2)))))))))

(define <mult>
  (lambda (inp cont)
    (<term> inp
      (lambda (inp2 cont2)
         (next-sym inp2
            (lambda (inp3 sym)
              (cond ((equal? sym 'TIME)
                       (<mult> inp3 (lambda (inp4 expr) (cont inp4 (list 'MUL cont2 expr)))))
                 ((equal? sym 'DIVD)
                      (<mult> inp3 (lambda (inp4 expr) (cont inp4 (list 'DIV cont2 expr)))))
                 ((equal? sym 'MODO)
                      (<mult> inp3 (lambda (inp4 expr) (cont inp4 (list 'MOD cont2 expr)))))
                 (else
                      (cont inp2 cont2)))))))))

(define <term>
  (lambda (inp cont)
    (next-sym inp ;; verifier le premier symbole du <term>
              (lambda (inp2 sym)
                (cond ((string? sym) ;; identificateur?
                       (cont inp2 (list 'VAR sym)))
                      ((number? sym) ;; entier?
                       (cont inp2 (list 'INT sym)))
                      (else
                       (<paren_expr> inp cont)))))))

;; La fonction execute prend en parametre l'ASA du programme a
;; interpreter et retourne une chaine de caracteres qui contient
;; l'accumulation de tout ce qui est affiche par les enonces "print"
;; executes par le programme interprete.

(define execute
  (lambda (ast)
    (exec-stat '() ;; etat des variables globales
               ""  ;; sortie jusqu'a date
               ast ;; ASA du programme
               (lambda (env output)
                 output)))) ;; retourner l'output pour qu'il soit affiche

;; La fonction exec-stat fait l'interpretation d'un enonce du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'enonce a interpreter et une continuation.  La continuation sera
;; appelee avec deux parametres : une liste d'association donnant la
;; valeur de chaque variable du programme apres l'interpretation de
;; l'enonce et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'enonce.


(define exec-stat
  (lambda (env output ast cont)
    (case (car ast)

      ((PRINT)
       (exec-expr env ;; evaluer l'expression du print
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env ;; ajouter le resultat a la sortie
                          (string-append output
                                         (cond ((equal? val #t)
                                                     "true")
                                               ((equal? val #f)
                                                     "false")
                                               (else
                                                     (number->string val)))
                                         "\n")))))
      ((EXPR)
       (exec-expr env ;; evaluer l'expression
                  output
                  (cadr ast)
                  (lambda (env output val)
                    (cont env output)))) ;; continuer en ignorant le resultat

      ((SEQ) ;; suite de 2 statements
         (exec-stat env
                    output
                    (cadr ast)
                    (lambda (env2 output2) ;; env and output are updated with their value after execution
                      (exec-stat env2         ;; donner l'env d'output du premier au deuxième
                                 output2      ;; concat les sorties
                                 (caddr ast)
                                 (lambda (env3 output3)
                                   (cont env3 output3))))))


      ((IF)
         (exec-expr env ;; execute the condition of the if
                    output
                    (cadr ast)
                    (lambda (env output val)
                            (if val
                                (exec-stat env  ;; execute the body of the if
                                           output
                                           (caddr ast)
                                           (lambda (env output)
                                                   (cont env output)))
                                (cont env output)))))   ;; skip the body of the if
      (else
       "internal error (unknown statement AST)\n"))))

                      
;; La fonction exec-expr fait l'interpretation d'une expression du
;; programme.  Elle prend quatre parametres : une liste d'association
;; qui contient la valeur de chaque variable du programme, une chaine
;; de caracteres qui contient la sortie accumulee a date, l'ASA de
;; l'expression a interpreter et une continuation.  La continuation
;; sera appelee avec deux parametres : une liste d'association donnant
;; la valeur de chaque variable du programme apres l'interpretation de
;; l'expression et une chaine de caracteres qui contient la sortie
;; accumulee apres l'interpretation de l'expression.

(define value3 (lambda (x y val) val))

(define exec-expr
  (lambda (env output ast cont)
    (case (car ast)

      ((ASSIGN)
       (cont (cons (cons (cadr ast)
                         (exec-expr env output (caddr ast) value3))
                    env) ;; environement où on a ajouté la variable
             output
             '())) ;; pas de sous-arbre où continuer le travail
      ((INT)
         (cont env ;; retourner la valeur de la constante
               output
               (cadr ast)))
      ((ADD)
       (cont env
             output
             (+ (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((SUB)
       (cont env
             output
             (- (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((MUL)
       (cont env
             output
             (* (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((DIV)
       (cont env
             output
             (if (equal? 0 (strip->number (exec-expr env output (caddr ast) value3)))
                (arithmetic-err "division by zero.")
                (quotient (strip->number (exec-expr env output (cadr ast) value3))
                          (strip->number (exec-expr env output (caddr ast) value3))))))
      ((MOD)
       (cont env
             output
             (remainder (strip->number (exec-expr env output (cadr ast) value3))
                        (strip->number (exec-expr env output (caddr ast) value3)))))
      ((LT)
       (cont env
             output
             (< (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((GT)
       (cont env
             output
             (> (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((LE)
       (cont env
             output
             (<= (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((GE)
       (cont env
             output
             (>= (strip->number (exec-expr env output (cadr ast) value3))
                (strip->number (exec-expr env output (caddr ast) value3)))))
      ((EQ)
       (cont env
             output
             (equal? (strip->number (exec-expr env output (cadr ast) value3))
                     (strip->number (exec-expr env output (caddr ast) value3)))))
      ((NE)
       (cont env
             output
             (not (equal? (strip->number (exec-expr env output (cadr ast) value3))
                  (strip->number (exec-expr env output (caddr ast) value3))))))
      (else
       "internal error (unknown expression AST)\n"))))



;;;----------------------------------------------------------------------------

;;; *** NE MODIFIEZ PAS CETTE SECTION ***

(define main
  (lambda ()
    (print (parse-and-execute (read-all (current-input-port) read-char)))))

;;;----------------------------------------------------------------------------


;;(trace main parse-and-execute execute <program> <expr> <while_stat> <if_stat> <bracket_stat>)

;;(trace <sum> <mult> <term>  exec-stat exec-expr <stat> pp) 
