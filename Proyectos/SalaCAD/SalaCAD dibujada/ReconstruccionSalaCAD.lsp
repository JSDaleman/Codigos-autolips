;Autores: Juan Sebastian Daleman y Nicolas Prieto


;Función para extraer bloques con sus atributos personalizados
(defun c:ExtraerAtributos (/ dir nom form)

	(setq dir (getvar "dwgprefix"))
	(setq nom (getvar "dwgname"))
	(setq nom (substr nom 1 (- (strlen nom) 4)))
	(initget 
		"Cdf Sdf eXit"
		)
	(setq form (getkword "\n Formato para guardar los atributos [Cdf/Sdf] <eXit>: ")
	)

	; En la variable "dir" se almacena el directorio del archivo de dibujo actual,
  ; de forma que se pueda utilizar después par conocer la ruta del archivo con los atributos (.txt)

	(cond
	; Condicional de formato CDF.
		((eq form "Cdf")
		 (progn
			(command "_-ATTEXT" "C" (strcat dir "Sala_Referencia.txt") (strcat dir nom "_CDF.txt"))

			(alert (strcat "Los atributos de este dibujo se guardaron en el archivo de texto: " nom "_CDF.txt con formato CDF."))
		 )
		)
   
	; Condicional de formato SDF.
		((eq form "Sdf")
		 (progn
			(command "_-ATTEXT" "S" (strcat dir "Sala_Referencia.txt") (strcat dir nom "_SDF.txt"))
			(alert (strcat "Los atributos de este dibujo se guardaron en el archivo de texto: " nom "_SDF.txt con formato SDF."))
		 )
		)

	)
)




; Inicialización y selección de archivo fuente:
(defun c:ReconstruirDibujo ()
  ; Obtener el directorio actual del dibujo abierto
  (setq dir (getvar "DWGPREFIX"))
  
  ; Mostrar ventana emergente para seleccionar el archivo de Excel
  (setq nom (getfiled "Selecciona el archivo de Excel con los datos a importar" dir "xlsx;xls" 0))
  
  ; Extraer solo el nombre del archivo (sin el directorio) para evitar duplicación
  (setq nom (vl-filename-base nom))
  
  ; Llamar a la función de importación principal
  (c:extdat)
)

; Función para vinculación con Excel y el archivo seleccionado:
(defun opexcel ()
  (setq exdir (strcat dir nom ".xlsx")) ; Construir la ruta correcta del archivo Excel
  
  (setq nhoja 1

        excelapp (vlax-get-or-create-object "excel.application") ; Conexión con Excel
        libcol (vlax-get-property excelapp "workbooks") ; Conexión con libros de Excel
        lib (vlax-invoke-method libcol "open" exdir) ; Invocación del libro específico
        colhojas (vlax-get-property lib "sheets") ; Conexión con las hojas del archivo
        hojaN (vlax-get-property colhojas "item" nhoja) ; Conexión con la hoja
        colcel (vlax-get-property hojaN "cells") ; Acceso a las celdas de la hoja
  )
  (vla-put-visible excelapp :vlax-false) ; No se muestra el archivo de Excel al importar
)

; Función para extraer los datos desde Excel en el orden especificado:
(defun getDat (fila)
  (while 
    (/= (setq name (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 1) vlax-vbstring))) "") ; Extraer NAME
  
    (setq 
          COORDX (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 2) vlax-vbstring)) ; Extraer coordenada X
          COORDY (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 3) vlax-vbstring)) ; Extraer coordenada Y
          XSCALE (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 4) vlax-vbstring)) ; Extraer escala X
          YSCALE (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 5) vlax-vbstring)) ; Extraer escala Y
          ORIENT (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 6) vlax-vbstring)) ; Extraer orientación
          ID (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 7) vlax-vbstring)) ; Extraer ID
          MARCA (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 8) vlax-vbstring)) ; Extraer MARCA
          PUESTO (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 9) vlax-vbstring)) ; Extraer PUESTO
          POSICION (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 10) vlax-vbstring)) ; Extraer POSICION
          PC_ACTUALES (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 11) vlax-vbstring)) ; Extraer PC_ACTUALES
          TIEMPO_DE_USO (vlax-variant-value (vlax-variant-change-type (vlax-get-property colcel "item" fila 12) vlax-vbstring)) ; Extraer TIEMPO_DE_USO
    )
    
    ; Imprimir los datos extraídos para verificar
    (princ (strcat "(" (itoa fila) ") " name "  " COORDX "  " COORDY "  " XSCALE "  " YSCALE  "  " ORIENT "  " ID "  " MARCA "  " PUESTO "  " POSICION "  " PC_ACTUALES "\n"))
    
    (corresc) ; Ajuste de escala para cada bloque
    (insbloc) ; Inserción de cada bloque

    (setq fila (+ fila 1)) ; Avanzar a la siguiente fila
  )
)

; Función para la desconexión completa del archivo fuente y de Excel
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
  (opexcel) ; Vinculación con Excel y archivo fuente
  (setq NombreHoja (vlax-get-property hojaN "name"))
  (alert(strcat "Se lee la hoja llamada " NombreHoja))
  (setq in1 "S")
  (setq in1 (strcase in1))
  (getDat 2) ;Función de extracción de valores definiendo la fila de inicio.
  (desc-excel) ; Desvinculación con Excel y archivo fuente
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


