#lang racket
#|
- Fecha:  16/04/2024
- Hora de Publicacion: 10:55am
- Version del código: 3.0
- Autor: Ing(c) Burbano Rodriguez Angel Gabriel
- Lenguaje utilizado: Racket
- Versión del lenguaje: 8.8
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Descripcion del programa: Este programa consiste en un juego llamado memorama o concentrese el cual,
será de dos jugadores, digitarán su nombre, el orden de las cartas a voltear cambiaran por cada nuevo juego,
las cartas se voltearan en caso en que el jugador haya perdido durante su turno y se le pasara el turno al siguiente
jugador. En caso de que el jugador acierte una, se le sumara un punto y seguirá con su turno. Gana el que más
aciertos obtenga. El programa cerrará 10 segundos despues de que el juego haya finalizado.
- SALVEDAD: para nombres mayores a 12 caracteres y nombres que contengan espacios, no garantizamos los resultados.
|#

(require graphics/graphics)

(open-graphics)

(printf "POR FAVOR ESCRIBA EL NOMBRE DEL PRIMER JUGADOR: ")

;Identificador player1 que guardará el nombre del jugador 1 todo en mayuscula
(define player1 (string-upcase(~a(read))))

(printf "POR FAVOR ESCRIBA EL NOMBRE DEL SEGUNDO JUGADOR: ")

;Identificador player2 que guardará el nombre del jugador 2 todo en mayuscula
(define player2 (string-upcase(~a(read))))

;Ventana del juego definido como memorama
(define memorama (open-viewport "Memorama" 800 650))

;Lo pinta de color rojo oscuro (Dark Red)
((draw-viewport memorama) "DarkRed")

;Grafica en la ventana memorama el titulo del juego, el nombre de los jugadores y el logo.
(((draw-pixmap-posn "logo.png") memorama )(make-posn 500 425))
((draw-string memorama) (make-posn 475  50) "JUEGO DEL CONCENTRESE" "Yellow")
((draw-string memorama) (make-posn 700  50) "SCORE" "Yellow")
((draw-string memorama) (make-posn 475  100) "JUGADOR 1:" "Yellow")
((draw-string memorama) (make-posn 575  100)  player1 "Yellow")
((draw-string memorama) (make-posn 475 120) "JUGADOR 2:" "Yellow")
((draw-string memorama) (make-posn 575 120)   player2 "Yellow")

#|--------------------------------------------------
Funcion ChangeChar que sirve para cambiar un caracter en una cadena ya dada en una posicion que el
usuario necesite y el char, lo pone el usuario
- Identificador local str: es la cadena de string a cambiar
- Identificador local char: es el caracter por el cual reemplazaremos
- Identificador local pos: es el valor de la posicion
|#
(define (ChangeChar str char pos)
  (string-append (substring str 0 pos)
                 char
                 (substring str (+ pos 1))
  )
)

#|--------------------------------------------------
Funcion RandomString que genera el string con las letras en distinta posicion
- Identificador local str: es el string en el cual, tendra las letras en posiciones aleatorias
- Identificador local char: es el caracter a poner dentro de la cadena, de acuerdo al contador y el random contador
se colocara una letra que puede ser desde la A hasta la H
- Identificador local counter: es el contador para que la funcion acabe despues de realizarse 16 veces
- Identificador local randomCounter: es el contador para saber si una letra fue puesta dos veces
|#
(define (RandomString str char counter randomCounter)
  ;Identificador numRandom que genera un numero aleatorio entre 0 y 15
  (define numRandom (random 16))
  ( if (= counter 17)
       str ;devuelve el string
  ;De lo contrario
       ( if (> 2 randomCounter)
            (if (char-alphabetic? (string-ref str numRandom))
              (RandomString str char counter randomCounter)
            ;De lo contrario
              (RandomString (ChangeChar str (~a(integer->char char)) numRandom) char (+ counter 1) (+ 1 randomCounter)) 
            );Fin if (char-alphabetic? (string-ref str numRandom))
       ;De lo contrario
            (RandomString str ( + 1 char ) counter 0)
       );Fin if (> 2 randomCounter) 
  );Fin if (= counter 17)
);Fin funcion RandomString

;------------------------------------------------------------------------------------
;Identificador stringRandom que tendra el string con las letras en desorden aleatorio
(define stringRandom (RandomString (make-string 16 #\space) 65 1 0))

#|-----------------------------------------------------------------------------------------------
Funcion ImprimirReverso que graficara en la ventana memorama las cartas en reverso al inicio del juego
- Identificador local x: guarda la posicion en x para graficar la carta
- Identificador local y: guarda la posicion en y para graficar la carta
- Identificador local counter: cuenta si la carta ya fue graficada 4 veces en x para bajar en y y volver a imprimir otras 4
- Identificador local final: cuenta la cantidad de filas que a graficado cartas y detiene el programa en 4 filas 
|#
(define (ImprimirReverso x y counter final)
   (if (= final 4)
       (void)
       ;De lo contrario
   (if (< counter 4)
       (if (= (remainder y 150) 50)   
         (if (= (remainder counter 2) 1)
            [begin
             (((draw-pixmap-posn "reverso1.JPG") memorama )(make-posn x y))
             (ImprimirReverso (+ x 110) y (+ counter 1) final )
            ];Fin begin
         ;De lo contrario
            [begin
             (((draw-pixmap-posn "reverso1.JPG") memorama )(make-posn x y))
             (ImprimirReverso (+ x 110) y (+ counter 1) final)
            ];Fin begin
         );Fin if (= (remainder counter 2) 1)
        ;De lo contrario
        (if (= (remainder counter 2) 1)
           [begin
            (((draw-pixmap-posn "reverso1.JPG") memorama )(make-posn x y))
            (ImprimirReverso (+ x 110) y (+ counter 1)final)
          ];Fin begin
        ;De lo contrario   
          [begin
           (((draw-pixmap-posn "reverso1.JPG") memorama )(make-posn x y))
           (ImprimirReverso (+ x 110) y (+ counter 1) final)
          ];Fin begin
       );Fin if (= (remainder counter 2) 1)
      );Fin if (= (remainder y 150) 50) 
       (ImprimirReverso 30 (+ y 160) 0 (+ final 1))
    );Fin if (< counter 4)
  );Fin if (= final 4)
);Fin funcion ImprimirReverso
;Llamamos la funcion para que grafique las cartas en reverso
(ImprimirReverso 30 10 0 0)

#|-----------------------------------------------------------------------------------------------
Funcion ImprimirImagen que graficara en la ventana memorama las cartas de acuerdo al lugar donde se dio click
- Identificador local str: guarda el string del juego
- Identificador local respuesta1: guarda el valor de la carta seleccionada (de 1 a 16)
- Identificador local x1: Guarda el valor de x de acuerdo a la posicion clickeada por el jugador
- Identificador local y1: Guarda el valor de y de acuerdo a la posicion clickeada por el jugador 
|#
(define (ImprimirImagen str respuesta1 x1 y1)
(if (char=? (string-ref str (- respuesta1 1)) #\A)
      (((draw-pixmap-posn "img1.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
    ;De lo contrario
      (if (char=? (string-ref str (- respuesta1 1)) #\B)
         (((draw-pixmap-posn "img2.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
      ;De lo contrario   
         (if (char=? (string-ref str (- respuesta1 1)) #\C)
             (((draw-pixmap-posn "img3.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
         ;De lo contrario
             (if (char=? (string-ref str (- respuesta1 1)) #\D)
               (((draw-pixmap-posn "img4.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
             ;De lo contrario
               (if (char=? (string-ref str (- respuesta1 1)) #\E)
                 (((draw-pixmap-posn "img5.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
               ;De lo contrario  
                 (if (char=? (string-ref str (- respuesta1 1)) #\F)
                  (((draw-pixmap-posn "img6.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
               ;De lo contrario   
                  (if (char=? (string-ref str (- respuesta1 1)) #\G)
                   (((draw-pixmap-posn "img7.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
                  ;De lo contrario
                   (if (char=? (string-ref str (- respuesta1 1)) #\H)
                    (((draw-pixmap-posn "img8.JPG") memorama ) (make-posn (+ (* 100 x1 ) (* 10 x1 ) 30) (+ (* 150 y1) (* 10 y1) 10)))
                    ;De lo contrario
                    (void)
                   );Fin if (char=? (string-ref str (- respuesta1 1)) #\H)
                  );Fin if (char=? (string-ref str (- respuesta1 1)) #\G)
                 );Fin if (char=? (string-ref str (- respuesta1 1)) #\F)
               );Fin if (char=? (string-ref str (- respuesta1 1)) #\E)
             );Fin if (char=? (string-ref str (- respuesta1 1)) #\D)
         );Fin if (char=? (string-ref str (- respuesta1 1)) #\C)
      );Fin if (char=? (string-ref str (- respuesta1 1)) #\B)
  );Fin if (char=? (string-ref str (- respuesta1 1)) #\A)
);fin funcion ImprimirImagen

#|-----------------------------------------------------------------------------------------------
Funcion Game que realizará las funciones del juego
- Identificador local str: guarda el string del juego
- Identificador local newStr: guarda el nuevo string del juego de acuerdo a la carta ya adivinadas
- Identificador local turn: Guarda el valor del turno para saber si le toca al jugador 1 o el jugador 2
- Identificador local player1: Guarda el nombre del jugador 1
- Identificador local player2: Guarda el nombre del jugador 2
- Identificador local score1: Guarda los puntos del jugador 1
- Identificador local score2: Guarda los puntos del jugador 2
- Identificador local movimiento1: Guarda el valor de la carta seleccionada previamente (en caso de que sea la segunda a escoger)
- Identificador local xAnt: Guarda el valor de x de la jugada anterior
- Identificador local yAnt: Guarda el valor de y de la jugada anterior
- Identificador local counter: va contando los movimientos del juego e identificar si es el movimiento 1 o el 2
|#
(define (Game str newStr turn player1 player2 score1 score2 movimiento1 xAnt yAnt counter )
;Grafica en la ventana memorama los puntos y el turno del jugador
((draw-string memorama) (make-posn 710  100) (number->string (- score1 1)) "DarkRed")
((draw-string memorama) (make-posn 710  100) (number->string score1) "Yellow")
((draw-string memorama) (make-posn 710  120) (number->string (- score2 1)) "DarkRed")
((draw-string memorama) (make-posn 710  120) (number->string score2) "Yellow")
((draw-string memorama) (make-posn 475  160) "JUEGA: " "Yellow")
((draw-string memorama) (make-posn 550  160) (if (= turn 1) player2 player1) "DarkRed")
((draw-string memorama) (make-posn 550  160) (if (= turn 1) player1 player2) "Yellow")
;---------------------------------------------------------------------------------------  
;Evalua si ya hubo un ganador
;Si lo hubo, quita el texto de que juega el jugador 1 o 2 y lo reemplaza por el resultado del juego
(if (= (+ score1 score2) 8)
  (if (= score1 score2)
    [begin
     ((draw-solid-rectangle memorama) (make-posn 465  145) 200 30 "DarkRed")
     ((draw-string memorama) (make-posn 475  160) "EMPATE" "Yellow")
     (sleep 10)
     (exit)
    ];Fin begin
    ;De lo contrario
    (if (> score1 score2)
        [begin
         ((draw-solid-rectangle memorama) (make-posn 465  145) 200 30 "DarkRed")
         ((draw-string memorama) (make-posn 475  160) "GANA: " "Yellow")
         ((draw-string memorama) (make-posn 550  160) player1 "Yellow")
         (sleep 10)
         (exit)
       ];Fin begin
      [begin
        ((draw-solid-rectangle memorama) (make-posn 465  145) 200 30 "DarkRed")
        ((draw-string memorama) (make-posn 475  160) "GANA: " "Yellow")
        ((draw-string memorama) (make-posn 550  160) player2 "Yellow")
        (sleep 10)
        (exit)
      ];Fin begin
    );Fin if (> score1 score2)
  );Fin if (= score1 score2)
  (void)
);Fin if (= (+ score1 score2) 8)
  
;----------------------------------------------------------------------------
;Identificador ubication1 que guarda la posicion del click en la ventana memorama
(define ubication1 (get-mouse-click memorama))
;Identificador x1 que guarda el valor de x del click en la ventana memorama
(define x1 (posn-x (mouse-click-posn ubication1)))
;Identificador y1 que guarda el valor de y del click en la ventana memorama  
(define y1 (posn-y (mouse-click-posn ubication1)))
;Identificador respuesta1 que guarda el número de la carta de acuerdo a su posicion en x y en y
(define respuesta1 (+ (+ 1 (quotient (- x1 30) 110)) (* (quotient (- y1 10) 160) 4)) )
  
;---------------------------------------------------------------------------------------------------------------------
;Evalua si la posicion clickeada es correcta en y  
 (if (or (and (> y1 9) (< y1 161) ) (and (> y1 169) (< y1 321) ) (and (> y1 329) (< y1 481) ) (and (> y1 489) (< y1 641) ))
  ;Evalua si la posicion clickeada es correcta en x  
  (if (or (and (> x1 29) (< x1 131) ) (and (> x1 139) (< x1 241) ) (and (> x1 249) (< x1 351) ) (and (> x1 359) (< x1 461) ))
      ;Evalua si la carta seleccionada ya había sido ganadora
      (if (char-alphabetic? (string-ref newStr (- respuesta1 1)))
          ;Evalua si la carta seleccionada no es la misma que la que selecciono la jugada pasada
          (if (not ( = movimiento1 respuesta1))
              [begin
               (ImprimirImagen str respuesta1  (quotient (- x1 30) 110) (quotient (- y1 10) 160))
               ;Evalua si es la primer carta que selecciona o la segunda
               (if (= (remainder counter 2) 0)
                   ;Evalua si las cartas seleccionadas son iguales
                   (if (char=? (string-ref str (- respuesta1 1)) (string-ref str (- movimiento1 1)))
                       (Game (ChangeChar newStr (string #\1) (- respuesta1 1)) (ChangeChar newStr (string #\1) (- respuesta1 1)) turn player1 player2 (if (= turn 1) (+ score1 1) score1) (if (= turn 2) (+ score2 1) score2) 0 0 0 (+ 1 counter) )
                   ;De lo contrario
                       [begin
                         (sleep 1)
                         (((draw-pixmap-posn "reverso1.JPG") memorama ) (make-posn (+ (* 100 (quotient (- x1 30) 110) ) (* 10 (quotient (- x1 30) 110) ) 30) (+ (* 150 (quotient (- y1 10) 160)) (* 10 (quotient (- y1 10) 160)) 10)))
                         (((draw-pixmap-posn "reverso1.JPG") memorama ) (make-posn (+ (* 100 (quotient (- xAnt 30) 110) ) (* 10 (quotient (- xAnt 30) 110) ) 30) (+ (* 150 (quotient (- yAnt 10) 160)) (* 10 (quotient (- yAnt 10) 160)) 10)))
                         (Game str str (if (= turn 1) 2 1) player1 player2 score1 score2 0 0 0 (+ 1 counter) )  
                         ];Fin begin
                  );Fin if (char=? (string-ref str (- respuesta1 1)) (string-ref str (- movimiento1 1)))
              ;De lo contrario 
                  (Game str (ChangeChar newStr (string #\1) (- respuesta1 1)) turn player1 player2 score1 score2 respuesta1 x1 y1 ( + counter 1) ) 
              );Fin if (= (remainder counter 2) 0) 

            ];Fin begin
          ;De lo contrario    
              (Game str newStr turn player1 player2 score1 score2 movimiento1 xAnt yAnt counter )
         );Fin if (not ( = movimiento1 respuesta1))
      ;De lo contrario
         (Game str newStr turn player1 player2 score1 score2 movimiento1 xAnt yAnt counter )
      );Fin if (char-alphabetic? (string-ref newStr (- respuesta1 1)))
  ;De lo contrario    
     (Game str newStr turn player1 player2 score1 score2 movimiento1 xAnt yAnt counter )
  );Fin if (or (and (> x1 29) (< x1 131) ) (and (> x1 139) (< x1 241) ) (and (> x1 249) (< x1 351) ) (and (> x1 359) (< x1 461) ))
 ;De lo contrario
 (Game str newStr turn player1 player2 score1 score2 movimiento1 xAnt yAnt counter )
 );Fin if (or (and (> y1 9) (< y1 161) ) (and (> y1 169) (< y1 321) ) (and (> y1 329) (< y1 481) ) (and (> y1 489) (< y1 641) ))
);Fin funcion Game
;----------------------------------------------------------------------------------------------------------

;Llamamos la funcion Game
(Game stringRandom stringRandom 1 player1 player2 0 0 0 0 0 1)