#lang racket

(require "ppt.rkt")

(provide (all-defined-out))

;; Pornind de la numerele e și f din cvartetele GH, putem
;; obține trei soluții (a1,b1,c1), (a2,b2,c2), (a3,b3,c3)
;; care respectă relația a^2 + b^2 = c^2.
;; Aceste triplete nu reprezintă TPP, și în anumite cazuri
;; conțin coeficienți negativi, însă acest lucru nu ne
;; împiedică să le folosim pentru a crea cheia de
;; criptare/decriptare pentru un criptosistem simplu.

;; Cele trei soluții se obțin conform formulelor de mai jos:
(define (a1 e f) (+ (* e e) (* 2 f e)))
(define (b1 e f) (+ (* 2 f f) (* 2 f e)))
(define (c1 e f) (+ (* 2 f f) (* e e) (* 2 f e)))

(define (a2 e f) (- (* e e) (* 2 f e)))
(define (b2 e f) (- (* 2 f f) (* 2 f e)))
(define (c2 e f) (+ (* 2 f f) (* e e) (* -2 f e)))

(define (a3 e f) (- (* f f) (* e e)))
(define (b3 e f) (* 2 f e))
(define (c3 e f) (+ (* f f) (* e e)))

;; Funcția check nu are utilitate pentru temă, este doar
;; un mod de a:
;; - vizualiza tripletele generate pentru 2 numere e și f
;; - testa că ele respectă relația a^2 + b^2 = c^2
(define (check e f)
  (let ((a1 (a1 e f)) (b1 (b1 e f)) (c1 (c1 e f))
                      (a2 (a2 e f)) (b2 (b2 e f)) (c2 (c2 e f))
                      (a3 (a3 e f)) (b3 (b3 e f)) (c3 (c3 e f)))
    (display (list (list a1 b1 c1) (list a2 b2 c2) (list a3 b3 c3)))
    (newline)
    (and
     (= (+ (sqr a1) (sqr b1)) (sqr c1))
     (= (+ (sqr a2) (sqr b2)) (sqr c2))
     (= (+ (sqr a3) (sqr b3)) (sqr c3)))))

;; Exemplu de utilizare
;; (cu primele 3 generații din arborele de cvartete):
 (map check
     '(1 1 2 2 1 4 4 2 5 5 2 3 3)
     '(2 4 5 3 6 9 7 9 12 8 7 8 4))

;; Vom trimite mesaje folosind cele 26 de caractere din
;; alfabetul englez (doar în varianta cu literă mică),
;; plus caracterul "spațiu". În consecință, avem nevoie să
;; codificăm 27 de caractere distincte:
;;  - asociem codul 0 caracterului "spațiu"
;;  - asociem codurile 1-26 caracterelor 'a', 'b' ... 'z'
;;
;; Cele două părți angrenate în comunicare își vor trimite un
;; număr n, pe baza căruia fiecare va determina cheia de
;; criptare/decriptare astfel:
;;  - determină al n-lea cvartet din arborele TPP
;;  - extrag valorile e și f din acest cvartet
;;  - cheia va fi (a1,b1,c1,a2,b2,c2,a3,b3,c3) mod 27
;;    (fiecare valoare din tuplu devine un cod între 0 și 26)
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

(define (apply-functional-transformations Fs tuple)
   (car (foldl (λ (func list) (map func list)) (list tuple) Fs)))
(define get-nth-tuple
  (lambda (start Fs1 Fs2 Fs3) ; start - tuplu   Fs - lista functii ce trebuiesc aplicate
      (lambda (n)
        (apply-functional-transformations (map (lambda(index) (if (= index 1) Fs1 (if (= index 2) Fs2 Fs3))) (get-transformations n)) start))))
(define get-nth-quadruple (get-nth-tuple '(1 1 2 3) (apply-q Q1) (apply-q Q2) (apply-q Q3)))

; TODO
; Implementați o funcție care primește un număr n și
; obține cheia de criptare/decriptare conform algoritmului
; de mai sus.
; Folosiți o formă adecvată de let pentru a extrage e și f.
; Utilizați o funcțională astfel încât să nu scrieți 9 bucăți 
; de cod foarte similare (de exemplu, numele operației mod ar
; trebui să apară o singură dată).

(define (key n)
  (let ((e (cadr (get-nth-quadruple n))) (f (caddr (get-nth-quadruple n))))
      (map (lambda (elem) (modulo elem 27)) (list (a1 e f) (b1 e f) (c1 e f) (a2 e f) (b2 e f) (c2 e f) (a3 e f) (b3 e f) (c3 e f)))))
 ; din cvartet - al 2 lea si al 3 lea numar sunt e si f


; TODO
; Implementați o funcție care primește un mesaj (un șir de
; caractere care conține doar litere mici și spații) și
; întoarce o listă de coduri asociate mesajului respectiv
; (spațiu devine 0, 'a' devine 1 ... 'z' devine 26).
; Funcții utile: string->list, char->integer
(define (message->codes message)
  (foldr (lambda (elem acc) (if (= elem -64) (cons 0 acc) (cons elem acc)))
           empty
           (map (lambda (elem) (- elem 96)) (map (lambda (elem) (char->integer elem)) (string->list message)))))
  
;pasi: "Lista mea x"
; string->list("Lista mea x") => '(#\L #\i #\s #\t #\a #\ #\m #\e #\a #\ #\x) - cu tot cu space
; (char->integer #\litera-mica) - 96 - ca sa obtin ce trebuie, 0 e cu " "


; TODO
; Implementați o funcție care primește o listă de coduri
; (numere între 0 și 26) și întoarce mesajul asociat
; (procesul invers celui de la funcția message->codes).
; Funcții utile: list->string, integer->char
(define (codes->message codes)
       (list->string (map (lambda (elem) (integer->char elem))
            (foldr (lambda (elem acc) (if (= elem 128) (cons 32 acc) (cons elem acc)))
                   empty
                   (map (lambda (elem) (+ 96 elem)) (foldr (lambda (elem acc) (if (= elem 0) (cons 32 acc) (cons elem acc)))
                                                           empty
                                                           codes))))))

;; Pentru operațiile de criptare și decriptare, lucrăm
;; cu coduri între 0 și 26.
;; Folosim următoarele notații:
;;    m = un cod din reprezentarea mesajului original
;;    c = un cod din reprezentarea mesajului criptat
;;    k = un cod din reprezentarea cheii
;;
;; Pentru a putea efectua operații de criptare/decriptare,
;; cheia trebuie extinsă/trunchiată la dimensiunea
;; mesajului care trebuie criptat/decriptat.
;;
;; Mai departe, criptarea se realizează conform formulei:
;;   c = (m + k) mod 27   (pentru fiecare index)          
;;
;; Similar, formula pentru decriptare este:
;;   m = (c - k) mod 27   (pentru fiecare index)


; TODO
; Implementați o funcție care primește o cheie key și o
; dimensiune size și extinde/trunchiază cheia key la
; dimensiunea size.
; Folosiți cel puțin o formă de let.

(define (helper l)
  (append (cdr l) (list (car l)))) ; (2 3 4 5) => (3 4 5 2) - ca sa se continuie adaugarea

(define (extend-key key size)
  ; extinderea se face prin prelungire - daca am a b c d si size = 10 trebuie : a b c d a b c d a b

  (letrec
      ((length-equal?
        (lambda (l acc)
          (if (= (length l) size)
              l
              (modify-list l acc))))
       (modify-list
        (lambda (L listadd)
          (if (< (length L) size)
              (length-equal? (append L (list (car listadd))) (helper listadd))
              (take L size))))) ; daca lungimea e mai mare decat size vreau doar primele size elemente
    (length-equal? key key)))
                 
       
; TODO
; Observați că algoritmii de criptare/decriptare se
; aseamănă foarte mult. Abstractizați procesul într-o
; funcție mai generală (încercați să îi dați un nume
; sugestiv) din care derivați apoi, cu minim de efort,
; funcțiile:
;    (encrypt-codes message key)
;    (decrypt-codes crypted key)
; Nu folosiți recursivitate explicită (ci funcționale).

; Ambele funcții primesc două liste de coduri (reprezentând
; mesajul clar/criptat, respectiv cheia) și întorc o listă
; de coduri (mesajul criptat/decriptat, după caz).

;;    m = un cod din reprezentarea mesajului original
;;    c = un cod din reprezentarea mesajului criptat
;;    k = un cod din reprezentarea cheii
;;
;;
;; Mai departe, criptarea se realizează conform formulei:
;;   c = (m + k) mod 27   (pentru fiecare index)


;SABLONUL COMUN - FUNCTIA COMUNA DIN CARE DERIVA MAI USOR CELELALTE

(define codes-cripted-decripted
  (lambda (func)
    (λ (L key)
      (map (lambda (elemmsg elemkey) (modulo (func elemmsg elemkey) 27)) L (extend-key key (length L))))))


(define encrypt-codes (codes-cripted-decripted +))

;; Similar, formula pentru decriptare este:
;;   m = (c - k) mod 27   (pentru fiecare index)

(define decrypt-codes (codes-cripted-decripted -))

; TODO
; Analog funcțiilor encrypt-codes și decrypt-codes care
; operează pe coduri, implementați funcțiile encrypt-message
; și decrypt-message care operează pe șiruri de caractere.
; În acest caz, ambele funcții primesc un șir de caractere
; (mesajul clar/criptat) și o listă de coduri (cheia) și
; întorc un șir de caractere (mesajul criptat/decriptat).
(define codes-cripted-decripted-2
  (lambda (func)
    (λ (L key)
      (codes->message (map (lambda (elemmsg elemkey) (modulo (func elemmsg elemkey) 27)) (message->codes L) (extend-key key (length (message->codes L))))))))

(define encrypt-message (codes-cripted-decripted-2 +))

(define decrypt-message (codes-cripted-decripted-2 -))
           