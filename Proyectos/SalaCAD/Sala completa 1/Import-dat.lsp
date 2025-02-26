
(alert "Los bloques a importar, junto con los datos para unirlos se deben encontrar en la misma carpeta")

; Inicialización y selección de archivo fuente:
(defun c:atrimp ()

	(setq dir (getstring 0 "Ingrese el directorio donde se encuentra el archivo con los datos a importar"))
	(setq nom (getstring 0 "Ingrese el nombre del archivo con los datos a importar: \n"))

	(if (and (/= "/" (substr dir (strlen dir)))(/= "\\" (substr dir (strlen dir))))
		(setq dir (strcat dir "\\"))			
	)

	(cond
		((eq (substr nom (-(strlen nom) 5) (strlen nom)) ".xlsx")
		 (setq nom (substr nom 1 (-(strlen nom) 5)))
		 )
		((eq (substr nom (-(strlen nom) 4) (strlen nom)) ".xls")
		 (setq nom (substr nom 1(-(strlen nom) 4)))
		 )
		)
	(c:extdat) ; Handler de todo el programa de importación
)

(vl-load-com) ; Carga de las herramientas de VL

; Función para viculación con excel y el archivo seleccionado:
(defun opexcel ()
	(setq exdir (strcat dir nom)
				nhoja 1

				excelapp (vlax-get-or-create-object "excel.application") ; Conexión con excel
				libcol (vlax-get-property excelapp "workbooks") ; Conexión con libros de excel
				lib (vlax-invoke-method libcol "open" exdir) ; Invocación del libro específico
				colhojas (vlax-get-property lib "sheets") ; Conexión con las hojas del archivo
				hojaN (vlax-get-property colhojas "item" nhoja) ; Conexión con la hoja
				colcel (vlax-get-property hojaN "cells") ; Acceso a las celdas de la hoja
 	 )
	 (vla-put-visible excelapp :vlax-false) ; No se muestra el archivo de excel al importar
)


(defun getDat (fila)
	(while 

		(/= (setq name (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 1) vlax-vbstring))) "") ; Ejecutar para celdas "no vacías"
	
		(setq 
					objeto (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 2) vlax-vbstring))

				  material (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 3) vlax-vbstring))

				  referencia (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 4) vlax-vbstring))

					COORDX (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 5) vlax-vbstring))

					COORDY (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 6) vlax-vbstring))

					XSCALE (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 7) vlax-vbstring))

					YSCALE (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 8) vlax-vbstring))

					ORIENT (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 9) vlax-vbstring))

					cantidad (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 10) vlax-vbstring))

				  precio (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 11) vlax-vbstring))
		)
		(princ (strcat "(" (itoa fila) ") " objeto "  " referencia "  " material "  "  COORDX "  "  COORDY "  "  XSCALE "  " YSCALE  "  " ORIENT "  " cantidad "  " precio"\n"))
	
		(corresc) ; Ajuste de escala para cada bloque
		(insbloc) ; Insersicón de cada bloque

		(setq fila (+ fila 1)) 
	)
)

; Función para la desconexión completa del archivo fuente y de excel
(defun desc-excel()

	(vlax-release-object colcel)
	(vlax-release-object hojaN)
	(vlax-release-object colhojas)
	(vlax-release-object lib)
	(vlax-release-object libcol)

	(vlax-invoke-method excelapp 'QUIT) 
	(vlax-release-object excelapp)
	(gc)
)

(defun c:extdat ()

	(setvar "ATTREQ" 0) ; Se desactivan las ventanas emergentes al importar bloques
	; Se homogeneizan las unidades del archivo actual y del archivo fuente
	(setvar "INSUNITS" 0)
	(setvar "INSUNITSDEFSOURCE" 0)
	(setvar "INSUNITSDEFTARGET" 0)
	; Se desactiva la fijación a puntos específicos de la pantalla
	(setq autosORI (getvar "AUTOSNAP"))
	(setq osmodeORI (getvar "OSMODE"))
	(setvar "AUTOSNAP" 47)
	(setvar "OSMODE" 0)
	(opexcel) ; Vinculación con excel y archivo fuente
	(setq NombreHoja (vlax-get-property hojaN "name"))
	(alert(strcat "Se lee la hoja llamada " NombreHoja))
	(setq in1 "S")
	(setq in1 (strcase in1))
	(getDat 2) ;Función de extracción de valores definiendo la fila de inicio.
	(desc-excel) ; Desvinculación con excel y archivo fuente
	(print)
	; Se retornan las configuraciones predeterminadas
	(setvar "ATTREQ" 1)
	(setvar "INSUNITS" 6)
	(setvar "AUTOSNAP" autosORI)
	(setvar "OSMODE" osmodeORI)
	(command "_zoom" "_o" "ALL" "") ; Zoom a todos los objetos dentro del dibujo
)

; Función para ajustar las unidades del archivo origen al archivo destino
(defun corresc ()
	(setq COORDX (rtos (/ (distof COORDX) 1000.0))
				COORDY (rtos (/ (distof COORDY) 1000.0))
				XSCALE (rtos (/ (distof XSCALE) 1000.0))
				YSCALE (rtos (/ (distof YSCALE) 1000.0))
	)
)

; Función para insertar un bloque con los datos de inserción asignados
(defun insbloc () 
	(if (= (substr name 1 1) "'")
			(setq name (substr name 2 (- (strlen name) 2)))
	)
	(setq name (strcat dir name))
	(command "_-insert" name (strcat COORDX "," COORDY) XSCALE YSCALE ORIENT)
)

