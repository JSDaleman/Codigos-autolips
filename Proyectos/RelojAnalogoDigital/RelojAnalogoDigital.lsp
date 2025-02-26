;Hacer un reloj analogo-digital que funciones y simule un reloj real.
;Documentado con un doc en word o PDF que tenga el titulo del programa
;con una portada, la descripción, descripción de calculos matematicos,
;una copia del programa documentado, cargado y se corre el programa,
;como queda el reloj despues de correr el programa, conclusiones
;Bibliografia de como se hizo el reloj
;Se entrega un lsp donde esta el programa 
;Se envia por correo en un winrar de todos los archivos necesarios

;Funcion que elimina todos los objetos del dibujo actual
(defun c:Borrartodo ()
  (vl-cmdf "_.ERASE" "_All" "") 
)

;Funcion que inicializa todo el proceso de hacer el reloj y activarlo
(defun c:Reloj ()
  
  ;Se limpia el dibujo de otros elementos no deseados
  (c:Borrartodo)
  ;Se le pide al usuario cual sera 
  ;el tiempo de ejecucuión del reloj
  ;el centro del reloj
  ;y la escala teniendo como base un radio de 100 
  ;Se crea una variable de control para el tiempo de ejecución   
  (setq Parar   (getreal "\¿Cuantos segundos desea que funcione el reloj?\n")
        Home    (getpoint "\nDe el punto central del reloj\n")
        Escala  (getreal "\n¿De que escala desea que sea el reloj?\n ")
        TiempoCorriendo 0
  )

  
  ;Se apaga todo el osnap para evitar errores de dibujo 
  (vl-cmdf "_.-osnap" "NONE" )
  ;Se coloca el custom visual style en realistic
  (vl-cmdf "_.vscurrent" "R")
  
  ;Se dibujan todos los elemtentos del reloj analogo
  (RelojAnalogo)
   ;Se dibujan todos los elemtentos del reloj digital
  (RelojDigital)
  
  ; El bucle se ejecutará infinitamente 
  (while (> Parar TiempoCorriendo) ;T
    ;delay para esperar el tiempo de 1000 ms 
    (vl-cmdf "_.delay" 1000)
    ;Se suma 1 en todas las variables de segundos y la variable de control de tiempo de ejecución
    (setq Seg (1+ Seg)
          SegTotalMin (1+ SegTotalMin)
          SegTotalHor (1+ SegTotalHor)
          TextoSegViejo (assoc 1 DatosCuadroTextoSeg)
          TiempoCorriendo (1+ TiempoCorriendo)
    )
      
   ;Se verifica que cuando se cumplen los 60 segundos
   ;se vulve a 0 segundos y se suma 1 a minutos
  
    (if (= Seg 60)
      (progn 
        (setq Minu (1+ Minu) Seg  0)
        ;Se verifica que cuando se cumplen los 60 minutos
        (if (= Minu 60)
          ;se vulve a 0 minutos y se suma 1 a hora
          (progn  (setq Hora (1+ Hora) Minu 0)
                  ;Se verifica que cuando se cumplen las 24 horas
                  (if (= Hora 24)
                    ;se vulve a 0 hora y se le suma 1 al día
                    (progn  (setq dia (1+ dia) Hora 0)
                            ;Se verifica si se llego a alguna condición de dia para cambio de mes
                            (if (> dia 27) (progn
                              (COND
                                ;Se verifica si el mes es del grupo de 31 días
                                ((member mes Meses31Dias)
                                 ;Si el dia indica que ya paso el 31 se busca actualizar las variables
                                 (if (> dia 31)
                                     (if (= mes 12)
                                        ;Si el mes es diciembre se hace el cambio de año 
                                        ;Se actualizan las variables de la fecha y el año
                                        ;Se actualiza la entidad que muestra el año en el reloj  
                                        (progn (setq mes 1 dia 1 año (1+ año))
                                               (ActualizarAñoDig)
                                        )
                                        ;De no ser diciembre se actualizan solo las varibales de mes y dia
                                        (setq mes (1+ mes) dia 1)
                                     )
                                 )
                                )
                                ;Se verifica si el mes es del grupo de 31 días
                                ((member mes Meses30Dias)
                                 ;Si el dia indica que ya paso el 30 se busca actualizar las variables                                 
                                 (if (> dia 30)
                                     (setq mes (1+ mes) dia 1)
                                 )
                                )
                                ;Al no estar en los dos grupos anteriores el mes es febrero
                                ;Se verifica con el limite de febrero al ser bisiesto o no si este se revaso
                                ((if (> dia FebreroLim)
                                     (setq mes (1+ mes) dia 1)
                                     )
                                )
                              )
                              ;Se actualiza la entidad que muestra el mes en el reloj
                              (ActualizarMesDig)
                              )
                            )
                            ;Se actualiza la entidad que muestra el dia y la que muestra que dia de la semana es en el reloj
                            (ActualizarDiaDig)
                            (ActualizarDiaSemanDig)
                    )
                  )
                  ;Se actualiza la entidad que muestra la hora en el reloj
                  (ActualizarHoraDig)            
          )
        )
        ;Se actualiza la entidad que muestra los minutos en el reloj       
        (ActualizarMinDig) 
      )
    )
    ;Se actualiza la entidad que muestra los segundos en el reloj
    (ActualizarSegDig)
    ;Se actualizan las posiciones de las agujas 
    (c:ActualizarAgujas)
    (vl-cmdf "_.regenall")
  )
)

;Funcion para abstraer del sistema datos de fecha y hora

(defun c:ObtenerFechayHora ()
  
  ;Se obtiene la fecha y hora del sistema de la variable CDATE
  ;de la variable se extraen cada uno de los datos
  ;se definen tambien las velocidades angulares de cada manecilla en rad/seg
  ;se calculan las horas y minutos en segundos
  (setq TimeData      (getvar "CDATE")
        TextTimeData  (rtos TimeData 2 10)
        TextAño       (substr TextTimeData 1 4)
        TextMes       (substr TextTimeData 5 2)
        TextDia       (substr TextTimeData 7 2)
        TextHora      (substr TextTimeData 10 2)
        TextMin       (substr TextTimeData 12 2)
        TextSeg       (substr TextTimeData 14 2)
        año           (atoi TextAño)
        mes           (atoi TextMes)
        dia           (atoi TextDia)
        Hora          (atoi TextHora)
        Minu          (atoi TextMin)
        Seg           (atoi TextSeg)
        DelSeg        (* 2 (/ pi 60))
        DelMin        (* 2 (/ pi 3600))
        DelHor        (* 2 (/ pi 43200))
        SegTotalMin   (+ Seg (* Minu 60))
        SegTotalHor   (+ Seg (* Minu 60) (* Hora 3600))
        Meses31Dias  '(1 3 5 7 8 10 12)
        Meses30Dias  '(4 6 9 11)
  )
  ;Se verifica si el año es bisiesto o no
  ;y se pone el limite de los dias de febrero según
  ;sea el caso
  (if (= (rem año 4) 0)
    (setq FebreroLim 29)
    (setq FebreroLim 28)
  )
   
)


 
;Funciones para calcular que dia de la semana es apartir de los datos de fecha

(defun c:QueDiaSemanaEs ()
  (setq DiaSemana       (CalcularDiaSemana año mes dia)
        NombreDiaSemana (ObtenerNombreDiaSemana DiaSemana)
  )
)

;Para el calculo de que dia de la semana se esta se usa la congetura de zeller
;Al ver con pruebas de calculos que la congruencia poseia problemas
;al calcularse con el algoritmo computacional recomendado
;se corrigio generando dos listas para el calculo en vez
;En vez de las midificaciones recomendadas por la ISO en mes y año
(defun CalcularDiaSemana (año mes dia)
  ;(setq mes (if (< mes 3) (+ mes 12) mes)
  ;      año (if (< mes 3) (- año 1) año))
  (setq q dia
        m mes
        K (rem año 100.0)
        J (fix (/ año 100.0))
        h (fix (rem (+ q (fix (/ (* 13 (+ m 1)) 5.0)) K (fix (/ K 4.0)) (fix (/ J 4.0)) (* 5.0 J)) 7))
  )
)

(defun ObtenerNombreDiaSemana (DiaSemana)
  (if (< mes 3)
  (setq NombresDias '("LU" "MA" "MI" "JU" "VI" "SA" "DO" ))
  (setq NombresDias '("SA" "DO" "LU" "MA" "MI" "JU" "VI" ))  
  )
  ;(setq NombresDias '("Lunes" "Martes" "Miércoles" "Jueves" "Viernes" "Sábado" "Domingo" ))
  (nth DiaSemana NombresDias)
)



;Dibuja la parte digital del reloj y pone los datos correspondientes
(defun RelojDigital ()
  (DibCajas)
  (PonerDatosDigitales)
)

;Dibuja las cajas donde estan los números del reloj digital 
;donde se indicara que dia de la semana es
;y la fecha del dia
(defun DibCajas ( /  p1a p2a p1b p2b p1c p2c)
  (setq p1a (polar Home  (* -131 (/ pi 180))  (* Escala 20))
        p2a (polar p1a   (* -132 (/ pi 180))  (* Escala 60))
        p1b (polar Home  (* -49 (/ pi 180))   (* Escala 20))
        p2b (polar p1b   (* -48 (/ pi 180))   (* Escala 60))
        p1c (polar Home  (* 24 (/ pi 180))    (* Escala 49))
        p2c (polar p1c   (* 167 (/ pi 180))   (* Escala 92))  
  )
  
  (vl-cmdf "_.rectang"  p1a p2a ""  "_.rectang"  p1b p2b "" "_.rectang"  p1c p2c "")
)

;Escribe los textos para el reloj digital
(defun PonerDatosDigitales (/ pSeg pMin pHora pDia pFecha)
  (setq pSeg        (polar Home    (* -70 (/ pi 180))  (* Escala 60.5))
        pMin        (polar Home    (* -129 (/ pi 180)) (* Escala 72))
        pHora       (polar pMin    (* 90 (/ pi 180))   (* Escala 20))
        pDiaSemana  (polar pSeg    (* 101 (/ pi 180))   (* Escala 20.5))
        pDia        (polar Home    (* 153 (/ pi 180))  (* Escala 48.8))
        pRaya1      (polar pDia    (* 0 (/ pi 180))    (* Escala 20))
        pMes        (polar pRaya1  (* 0 (/ pi 180))    (* Escala 5))
        pRaya2      (polar pMes    (* 0 (/ pi 180))    (* Escala 20))
        pAño        (polar pRaya2  (* 0 (/ pi 180))    (* Escala 5))
        Altura1     (* Escala 15)
        Altura2     (* Escala 12)
        Rotacion    0.0
  )
  
  ;Se crea el cuadro de texto para segundos
  (if (< (strlen TextSeg) 2)
    (setq TextSeg (strcat "0" TextSeg))
  )
  (vl-cmdf "_.text" pSeg Altura1 Rotacion TextSeg "" "") 
  (setq CuadroTextoSeg      (entlast)
        DatosCuadroTextoSeg (entget CuadroTextoSeg)
  )
  
  ;Se crea el cuadro de texto para minutos
  (vl-cmdf "_.text" pMin Altura1 Rotacion TextMin "" "") 
  (setq CuadroTextoMin      (entlast)
        DatosCuadroTextoMin (entget CuadroTextoMin)
  )
  
  ;Se crea el cuadro de texto para hora
  (vl-cmdf "_.text" pHora Altura1 Rotacion TextHora "" "") 
  (setq CuadroTextoHora      (entlast)
        DatosCuadroTextoHora (entget CuadroTextoHora)
  )
  
  ;Se crea el cuadro de texto para dia de la semana
  (vl-cmdf "_.text" pDiaSemana Altura1 Rotacion NombreDiaSemana "" "") 
  (setq CuadroTextoDiaSemana      (entlast)
        DatosCuadroTextoDiaSemana (entget CuadroTextoDiaSemana)
  )
  
  ;Se crea el cuadro de texto para dia
  (vl-cmdf "_.text" pDia Altura2 Rotacion TextDia "" "") 
  (setq CuadroTextoDia      (entlast)
        DatosCuadroTextoDia (entget CuadroTextoDia)
  )
  
  ;Se crea el cuadro de texto para la / que divide dia y mes
  (vl-cmdf "_.text" pRaya1 Altura2 Rotacion "/" "" "") 
  
  ;Se crea el cuadro de texto para mes
  (vl-cmdf "_.text" pMes Altura2 Rotacion TextMes "" "") 
  (setq CuadroTextoMes      (entlast)
        DatosCuadroTextoMes (entget CuadroTextoMes)
  )
  
  ;Se crea el cuadro de texto para la / que divide mes y año
  (vl-cmdf "_.text" pRaya2 Altura2 Rotacion "/" "" "") 
  
  ;Se crea el cuadro de texto para año
  (vl-cmdf "_.text" pAño Altura2 Rotacion TextAño "" "") 
  (setq CuadroTextoAño      (entlast)
        DatosCuadroTextoAño (entget CuadroTextoAño))
  
  
)

;Funcion que actualiza los segundos en el reloj digital
(defun ActualizarSegDig ()
  (setq  TextSeg  (itoa Seg))
  (if (< (strlen TextSeg) 2)
    (setq TextSeg (strcat "0" TextSeg))
  )
  (setq  TextSegNuevo             (cons 1 TextSeg)
         TextoSegViejo            (assoc 1 DatosCuadroTextoSeg)
         DatosCuadroTextoSegNuevo (subst TextSegNuevo TextoSegViejo DatosCuadroTextoSeg)
   )
  (entmod DatosCuadroTextoSegNuevo)
)

;Funcion que actualiza los minutos el reloj digital
(defun ActualizarMinDig ()
  (setq TextMin (itoa Minu))
  (if (< (strlen TextMin) 2)
    (setq TextMin (strcat "0" TextMin))
  )
  (setq TextMinNuevo              (cons 1 TextMin)
        TextoMinViejo             (assoc 1 DatosCuadroTextoMin)
        DatosCuadroTextoMinNuevo  (subst TextMinNuevo  TextoMinViejo DatosCuadroTextoMin)
  )
  (entmod DatosCuadroTextoMinNuevo)
)

;Funcion que actualiza la hora en el reloj digital
(defun ActualizarHoraDig ()
  (setq TextHora (itoa Hora))
  (if (< (strlen TextHora) 2)
    (setq TextHora (strcat "0" TextHora))
  )
  (setq TextHoraNuevo             (cons 1 TextHora)
        TextoHoraViejo            (assoc 1 DatosCuadroTextoHora )
        DatosCuadroTextoHoraNuevo (subst TextHoraNuevo TextoHoraViejo DatosCuadroTextoHora)
  )
  (entmod DatosCuadroTextoHoraNuevo)
)

;Funcion que actualiza el dia en el reloj digital
(defun ActualizarDiaDig ()
  (setq TextDia (itoa dia))
  (if (< (strlen TextDia) 2)
    (setq TextDia (strcat "0" TextDia))
  )
  (setq TextDiaNuevo             (cons 1 TextDia)
        TextoDiaViejo            (assoc 1 DatosCuadroTextoDia)
        DatosCuadroTextoDiaNuevo (subst TextDiaNuevo TextoDiaViejo DatosCuadroTextoDia)
  )
  (entmod DatosCuadroTextoDiaNuevo)
)

;Funcion que actualiza el dia de la semana en el reloj digital
(defun ActualizarDiaSemanDig ()
  (c:QueDiaSemanaEs)
  (setq TextDiaSemanaNuevo       (cons 1 NombreDiaSemana)
        TextoDiaSemanaViejo      (assoc 1 DatosCuadroTextoDiaSemana)
        DatosCuadroTextoDiaSemanaNuevo (subst TextDiaSemanaNuevo TextoDiaSemanaViejo DatosCuadroTextoDiaSemana)
  )
  (entmod DatosCuadroTextoDiaSemanaNuevo)
)

;Funcion que actualiza el mes en el reloj digital
(defun ActualizarMesDig ()
  (setq TextMes (itoa mes))
  (if (< (strlen TextMes) 2)
    (setq TextMes (strcat "0" TextMes))
  )
  (setq TextMesNuevo             (cons 1 TextMes)
        TextoMesViejo            (assoc 1 DatosCuadroTextoMes)
        DatosCuadroTextoMesNuevo (subst TextMesNuevo TextoMesViejo DatosCuadroTextoMes)
  )
  (entmod DatosCuadroTextoMesNuevo)
)

;Funcion que actualiza el año en el reloj digital
(defun ActualizarAñoDig ()
  (setq TextAño                  (itoa año)
        TextAñoNuevo             (cons 1 TextAño)
        TextoAñoViejo            (assoc 1 DatosCuadroTextoAño)
        DatosCuadroTextoAñoNuevo (subst TextAñoNuevo TextoAñoViejo  DatosCuadroTextoAño)
  )
  (entmod DatosCuadroTextoAñoNuevo)
)


;Dibuja los elementos del reloj analogo
(defun RelojAnalogo ( / p1a p2a p3a)
  
  (setq p1a (polar Home (* 153 (/ pi 180))  (* Escala 88.9))
        p2a (polar Home (* 123 (/ pi 180))  (* Escala 86.5))
        p3a (polar Home (* 91 (/ pi 180))   (* Escala 85)))
  
  ;Dibuja el marco del reloj
  (DibujarReloj)
  ;Dibuja los numeros del 1-9
  (c:Dib1 Home 58 85)
  (c:Dib2 Home 26 90)
  (c:Dib3 Home -3 92)
  (c:Dib4 Home -33 93)
  (c:Dib5 Home -63 93.5)
  (c:Dib6 Home -91 95)
  (c:Dib7 Home -118 94)
  (c:Dib8 Home -148 94.3)
  (c:Dib9 Home -177 92)
  ;Dibuja el 10
  (c:Dib1 Home 153 88.9)
  (c:Dib0 p1a 0 3)
  ;Dibuja el 11
  (c:Dib1 Home 123 86.5)
  (c:Dib1 p2a 0 4)
  ;Dibuja el 12
  (c:Dib1 Home 91 85)
  (c:Dib2 p3a 0 7)
  ;Dibuja las agujas del reloj
  (DibAgujas)
  ;Se hace zoom al reloj para que el usuario lo visualice correctamente
  ;Se regenera los elementos creados para evitar errores
  (vl-cmdf "zoom" "extents")
  (vl-cmdf "regen")
  ;Se optiene la hora y fecha del sistema
  ;,se calcula el dia de la semana y 
  ;se actualizan las posiciones de las aguajas
  (c:ObtenerFechayHora)
  (c:QueDiaSemanaEs)
  (c:ActualizarAgujas)
)

;Dibuja el marco del reloj y el circulo del centro con sus decoraciones
(defun DibujarReloj ()
  (setq radio (* Escala 100)
        p1dec (polar Home (* 90 (/ pi 180))  (* Escala 100))
        p2dec (polar Home (* 90 (/ pi 180))  (* Escala 95))
        p3dec (polar Home (* 90 (/ pi 180))  (* Escala 97.5))
  )
  
  (vl-cmdf "_.circle" Home (+ radio (* Escala 10)) ""  "_.circle" Home radio "" "_.circle" Home (* Escala 5) "")
  (vl-cmdf "_.line" p1dec p2dec "")
  (setq Lin1Dec (entlast))
  (setq DatosLin1Dec  (entget Lin1Dec ))
  (vl-cmdf "_.ARRAYPOLAR" Lin1Dec "" Home 12 360 "X")
  (vl-cmdf "_.line" p1dec p3dec "")
  (setq Lin2Dec (entlast))
  (setq DatosLin2Dec  (entget Lin2Dec ))
  (vl-cmdf "_.ARRAYPOLAR" Lin2Dec "" Home 60 360 "X")
)


;Dibuja todas las manceillas mirando a las 12
(defun DibAgujas ()
  ;Define los puntos iniciales de cada uno de los puntos de las mencillas
  (setq p1Seg  (polar Home    (* 90 (/ pi 180))  (* Escala 75))
        p1Min  (polar Home    (* 98 (/ pi 180))  (* Escala 40.5))
        p2Min  (polar p1Min   (* 82 (/ pi 180))  (* Escala 40))
        p3Min  (polar p2Min   (* -82 (/ pi 180)) (* Escala 40))
        p1Hora (polar Home    (* 108 (/ pi 180)) (* Escala 23.5))
        p2Hora (polar p1Hora  (* 72 (/ pi 180))  (* Escala 23.7))
        p3Hora (polar p2Hora  (* -72 (/ pi 180)) (* Escala 23.7))
        ;Define la distancia desde el centro a cada uno de los puntos
        r1Seg  (distance Home p1Seg)
        r1Min  (distance Home p1Min)
        r2Min  (distance Home p2Min)
        r3Min  (distance Home p3Min)
        r1Hora (distance Home p1Hora)
        r2Hora (distance Home p2Hora)
        r3Hora (distance Home p3Hora)
        ;Define el angulo de la linea que se forma del centro y cada uno de los puntos
        Ang1Seg  (angle Home p1Seg)
        Ang1Min  (angle Home p1Min)
        Ang2Min  (angle Home p2Min)
        Ang3Min  (angle Home p3Min)
        Ang1Hora (angle Home p1Hora)
        Ang2Hora (angle Home p2Hora)
        Ang3Hora (angle Home p3Hora)
        
  )
  
  ;Dibuja el segundero
  (vl-cmdf "_.line" Home p1Seg "")
  (setq Segundero      (entlast)
        DatosSegundero (entget segundero)
        Mov1Seg        (assoc 11 DatosSegundero)
  )
  (setq DatosSegundero (CambiarColorEntidad DatosSegundero 1))
  (entmod DatosSegundero)
  
  
  ;Dibuja el minutero
  (vl-cmdf "_.pline" Home p1Min p2Min p3Min Home "")
  (setq Minutero       (entlast)
        DatosMinutero  (entget Minutero)
        PuntosMinutero (ObtenerPuntosPolilinea DatosMinutero)
  )
  (setq DatosMinutero (CambiarColorEntidad DatosMinutero 2))
  (entmod DatosMinutero)
  
  ;Dibuja el horario
  (vl-cmdf "_.pline" Home p1Hora p2Hora p3Hora Home "")
  (setq Horario       (entlast)
        DatosHorario  (entget Horario)
        PuntosHorario (ObtenerPuntosPolilinea DatosHorario)
  )
  (setq DatosHorario  (CambiarColorEntidad DatosHorario  120))
  (entmod DatosHorario)
  
)


;Actualiza la posicion de los puntos que describen las manecillas
;del reloj con respecto a la hora actual que se tiene de CDATE

(defun c:ActualizarAgujas ()
  ;Encuentra cual debe ser el angulo que debe tener cada manecilla
  (setq AngSeg   (* DelSeg Seg)
        AngMin   (* DelMin SegTotalMin)
        AngHora  (* DelHor SegTotalHor)
        
        ElemFalt   (list 10)
        ElemFalt1  (list 11)
        ModHome   (Append ElemFalt Home)
        ;Se Encuentra las nuevas posiciones de cada punto
        ;y se crean las listas que se remplazaran en la entidad
        p1Seg (polar Home (- Ang1Seg AngSeg)  r1Seg)
        p1Seg (Append ElemFalt1 p1Seg)
        
        p1Min (polar Home (- Ang1Min AngMin)  r1Min)
        p2Min (polar Home  (- Ang2Min AngMin)  r2Min)
        p3Min (polar Home  (- Ang3Min AngMin)  r3Min)
        p1Min   (Append ElemFalt p1Min)
        p2Min   (Append ElemFalt p2Min)
        p3Min   (Append ElemFalt p3Min)
        NewPuntosMinutero (list ModHome p1Min p2Min p3Min ModHome)
        
        p1Hora (polar Home (- Ang1Hora AngHora)  r1Hora)
        p2Hora (polar Home  (- Ang2Hora AngHora)  r2Hora)
        p3Hora (polar Home  (- Ang3Hora AngHora)  r3Hora)
        p1Hora (Append ElemFalt p1Hora)
        p2Hora (Append ElemFalt p2Hora)
        p3Hora (Append ElemFalt p3Hora)
        NewPuntosHorario (list ModHome p1Hora p2Hora p3Hora ModHome)
        
        ;Sustituyen los valores de cada entidad
        DatosSegundero2 (subst p1Seg Mov1Seg DatosSegundero)
        DatosMinutero2  (ReemplazarPuntos PuntosMinutero NewPuntosMinutero DatosMinutero)
        DatosHorario2   (ReemplazarPuntos PuntosHorario NewPuntosHorario DatosHorario)
  )
  
  ;Se actualiza como se presenta en el dibujo las manecillas
  (entmod DatosSegundero2)
  (entmod DatosMinutero2)
  (entmod DatosHorario2)
  
)


(defun ObtenerPuntosPolilinea (plineData)
  
  (setq puntos '()) ; Inicializar una lista para almacenar los puntos
  
  ; Iterar sobre las propiedades para encontrar los puntos
  (foreach prop plineData
    (if (= (car prop) 10) ; Verificar si el código de grupo es 10 (representa un punto)
        (setq puntos (cons prop puntos)) ; Agregar las coordenadas del punto a la lista
    )

  )
  
  ;Retorno de los puntos de la polilinea
  puntos
)

;Remplaza los punto de una polilinea por los nuevos puntos en la entidad ingresada
(defun ReemplazarPuntos (Puntos NewPuntos DatosEntidad)
  (setq NewDatosEntidad DatosEntidad) 
  (foreach Punto Puntos 
    (setq NewPunto        (car NewPuntos)
          NewDatosEntidad (subst NewPunto Punto NewDatosEntidad)
          NewPuntos       (cdr NewPuntos)
    ) 
  )
  ; Retornamos el resultado final
  NewDatosEntidad 
)

;Cambia los datos de color de una entidad por un color definido
(defun CambiarColorEntidad (DatosEntidad Color)
  (setq NewColor (list (cons 62 Color)))
  (setq OldColor (assoc 62 DatosEntidad))
  (if  OldColor
    (setq NewDatosLinea (subst NewColor OldColor DatosEntidad))
    (setq NewDatosLinea (append DatosEntidad NewColor))
  )
  NewDatosLinea
)

;Funciones para dibujar todos los digitos de 0-9 dando un punto inical y un offset polar a ese punto

(defun c:Dib0 (Home Ang Dis / p1a p2a p3a p4a)
  (setq p1a (polar Home  (* Ang (/ pi 180))  (* Escala Dis))
        p2a (polar p1a   (* 0 (/ pi 180))    (* Escala 4))
        p3a (polar p1a   (* 90 (/ pi 180))   (* Escala 10))
        p4a (polar p2a   (* 90 (/ pi 180))   (* Escala 10))
  )
  
  (vl-cmdf "_.pline" p1a p2a p4a p3a p1a "")
)

(defun c:Dib1 (Home Ang Dis / p1a p2a p3a)
  (setq p1a (polar Home (* Ang (/ pi 180))  (* Escala Dis))
        p2a (polar p1a  (* 90 (/ pi 180))   (* Escala 10))
        p3a (polar p2a  (* 240 (/ pi 180))  (* Escala 4))
  )
  
  (vl-cmdf "_.pline" p1a p2a p3a "")
)

(defun c:Dib2 (Home Ang Dis / p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180)) (* Escala Dis))
        p2a (polar p1a  (* 180 (/ pi 180)) (* Escala 4))
        p3a (polar p2a  (* 90 (/ pi 180))  (* Escala 5))
        p4a (polar p3a  (* 0 (/ pi 180))   (* Escala 4))
        p5a (polar p4a  (* 90 (/ pi 180))  (* Escala 5))
        p6a (polar p5a  (* 180 (/ pi 180)) (* Escala 4))
        
  )
  
  (vl-cmdf "_.pline" p1a p2a p3a p4a p5a p6a "")
)

(defun c:Dib3 (Home Ang Dis / p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180)) (* Escala Dis))
        p2a (polar p1a  (* 180 (/ pi 180)) (* Escala 4))
        p3a (polar p2a  (* 90 (/ pi 180))  (* Escala 5))
        p4a (polar p3a  (* 0 (/ pi 180))   (* Escala 4))
        p5a (polar p3a  (* 90 (/ pi 180))  (* Escala 5))
        p6a (polar p5a  (* 0 (/ pi 180))   (* Escala 4))
        
  )
  
  (vl-cmdf "_.pline" p5a p6a  p4a p3a p4a p1a p2a "")
)

(defun c:Dib4 (Home Ang Dis / p1a p2a p3a p4a)
  (setq p1a (polar Home (* Ang (/ pi 180))  (* Escala Dis))
        p2a (polar p1a  (* 90 (/ pi 180))   (* Escala 10))
        p3a (polar p2a  (* -129 (/ pi 180)) (* Escala 6.4))
        p4a (polar p3a  (* 0 (/ pi 180))    (* Escala 5.5))
        
  )
  
  (vl-cmdf "_.pline" p1a p2a p3a p4a "")
)

(defun c:Dib5 (Home Ang Dis / p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180)) (* Escala Dis))
        p2a (polar p1a  (* 0 (/ pi 180))   (* Escala 4))
        p3a (polar p2a  (* 90 (/ pi 180))  (* Escala 5))
        p4a (polar p3a  (* 180 (/ pi 180)) (* Escala 4))
        p5a (polar p4a  (* 90 (/ pi 180))  (* Escala 5))
        p6a (polar p5a  (* 0 (/ pi 180))   (* Escala 4))
        
  )
  
  (vl-cmdf "_.pline" p1a p2a p3a p4a p5a p6a "")
)

(defun c:Dib6 (Home Ang Dis / p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180)) (* Escala Dis))
        p2a (polar p1a  (* 0 (/ pi 180))   (* Escala 4))
        p3a (polar p1a  (* 90 (/ pi 180))  (* Escala 5))
        p4a (polar p3a  (* 0 (/ pi 180))   (* Escala 4))
        p5a (polar p3a  (* 90 (/ pi 180))  (* Escala 5))
        p6a (polar p5a  (* 0 (/ pi 180))   (* Escala 4))
        
  )
  
  (vl-cmdf "_.pline" p6a p5a p1a p2a p4a p3a "")
)

(defun c:Dib7 (Home Ang Dis / p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180))  (* Escala Dis))
        p2a (polar p1a  (* 70 (/ pi 180))   (* Escala 10.5))
        p3a (polar p2a  (* 180 (/ pi 180))  (* Escala 4))
        p4a (polar p1a  (* 70 (/ pi 180))   (* Escala 5.25))
        p5a (polar p3a  (* -90 (/ pi 180))  (* Escala 4.95))
        p6a (polar p5a  (* 0 (/ pi 180))    (* Escala 4))
        
  )
  
  (vl-cmdf "_.pline" p1a p2a p3a p2a p4a p5a p6a "")
)

(defun c:Dib8 (Home Ang Dis / p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180)) (* Escala Dis))
        p2a (polar p1a  (* 0 (/ pi 180))   (* Escala 4))
        p3a (polar p1a  (* 90 (/ pi 180))  (* Escala 5))
        p4a (polar p3a  (* 0 (/ pi 180))   (* Escala 4))
        p5a (polar p3a  (* 90 (/ pi 180))  (* Escala 5))
        p6a (polar p5a  (* 0 (/ pi 180))   (* Escala 4))
  )
  
  (vl-cmdf "_.pline" p5a p1a p2a p6a p5a p3a p4a "")
)

(defun c:Dib9 (Home Ang Dis /  p1a p2a p3a p4a p5a p6a)
  (setq p1a (polar Home (* Ang (/ pi 180)) (* Escala Dis))
        p2a (polar p1a  (* 0 (/ pi 180))   (* Escala 4))
        p3a (polar p1a  (* 90 (/ pi 180))  (* Escala 5))
        p4a (polar p3a  (* 0 (/ pi 180))   (* Escala 4))
        p5a (polar p3a  (* 90 (/ pi 180))  (* Escala 5))
        p6a (polar p5a  (* 0 (/ pi 180))   (* Escala 4))
  )
  
  (vl-cmdf "_.pline" p1a p2a p6a p5a p3a p4a "")
)










