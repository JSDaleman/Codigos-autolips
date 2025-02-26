(defun c:atrexp (/ dir nom form)

	(setq dir (getvar "dwgprefix"))
	(setq nom (getvar "dwgname"))
	(setq nom (substr nom 1 (- (strlen nom) 4)))
	(initget 
		"Cdf Sdf eXit"
		)
	(setq form (getkword "\n Formato para guardar los atributos [Cdf/Sdf] <eXit>: ")
	)

	; ---> En la variable "dir" se almacena el directorio del archivo de dibujo actual, de forma que se pueda utilizar después para guardar aqu+i el archivo con los atributos.

	(cond
	; ---> Selección del formato CDF.
		((eq form "Cdf")
		 (progn
			(command "_-ATTEXT" "C" (strcat dir "RefSalaCAD.txt") (strcat dir nom "_CDF.txt"))

			(alert (strcat "Los atributos de este dibujo se guardaron en el archivo de texto: " nom "_CDF.txt con formato CDF."))
		 )
		)
	; ---> Selección del formato SDF.
		((eq form "Sdf")
		 (progn
			(command "_-ATTEXT" "S" (strcat dir "RefSalaCAD.txt") (strcat dir nom "_SDF.txt"))
			(alert (strcat "Los atributos de este dibujo se guardaron en el archivo de texto: " nom "_SDF.txt con formato SDF."))
		 )
		)

	)
)
