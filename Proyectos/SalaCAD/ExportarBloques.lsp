(defun C:InsertarYExportarBloquesDesdeArchivos ()
  (setq rutaCarpeta (getvar "DWGPREFIX")) ; Obtiene la ruta de la carpeta donde está el dibujo actual
  (if (not rutaCarpeta)
    (progn
      (princ "\nNo se pudo obtener la ruta de la carpeta.")
      (exit)
    )
  )

  ;; Listar archivos DWG en la carpeta
  (setq archivosDWG (vl-directory-files rutaCarpeta "*.dwg" 1)) ; Busca archivos .dwg en la carpeta
  (if archivosDWG
    (progn
      ;; Obtener los bloques existentes en el dibujo actual
      (setq bloquesExistentes '())
      (setq tbl (tblnext "BLOCK" T))
      (while tbl
        (setq bloquesExistentes (cons (cdr (assoc 2 tbl)) bloquesExistentes))
        (setq tbl (tblnext "BLOCK"))
      )

      ;; Verificar si hay bloques en el dibujo actual
      (if (null bloquesExistentes)
        (progn
          ;; Si no hay bloques, exportar todos los bloques de los archivos DWG
          (foreach archivo archivosDWG
            (setq nombreBloque (vl-filename-base archivo))
            (princ (strcat "\nProcesando archivo: " archivo))
            
            ;; Intentar insertar el archivo DWG como bloque
            (command "._-INSERT" (strcat rutaCarpeta archivo) "0,0" "1" "1" "0" "0" "0" "0" "0")
            (if (tblsearch "BLOCK" nombreBloque)
              (progn
                ;; Exportar el bloque insertado
                (if (not (findfile (strcat rutaCarpeta nombreBloque ".dwg"))) ; Si el bloque no ha sido exportado aún
                  (progn
                    (command "WBLOCK" (strcat rutaCarpeta nombreBloque ".dwg") nombreBloque)
                    (princ (strcat "\nBloque exportado: " nombreBloque))
                  )
                  (princ (strcat "\nEl bloque ya ha sido exportado: " nombreBloque))
                )
              )
              (princ (strcat "\nEl archivo no contiene bloques válidos o no se insertó correctamente: " archivo))
            )
          )
        )
        (princ "\nEl dibujo actual ya contiene bloques. Solo se exportarán los bloques no existentes previamente.")
      )
    )
    (princ "\nNo se encontraron archivos DWG en la carpeta.")
  )
  (princ) ; Termina el programa
)




(defun C:ListarBloques ()
  (setq bloques (tblnext "BLOCK" T)) ; Obtiene la primera definición de bloque
  (setq listaBloques '()) ; Inicializa una lista vacía para almacenar los nombres de bloques
  (if bloques
    (progn
      (while bloques
        (setq bloqueNombre (cdr (assoc 2 bloques))) ; Obtiene el nombre del bloque
        ;; Verifica que no sea un bloque anónimo (como "*Model_Space" o "*Paper_Space")
        (if (not (wcmatch bloqueNombre "*`*"))
          (progn
            (princ (strcat "\nBloque encontrado: " bloqueNombre)) ; Imprime en la consola
            (setq listaBloques (cons bloqueNombre listaBloques)) ; Agrega el bloque a la lista
          )
        )
        (setq bloques (tblnext "BLOCK")) ; Obtiene la siguiente definición de bloque
      )
      ;; Si se encontraron bloques, los muestra en una ventana emergente
      (if listaBloques
        (alert (strcat "Bloques en el dibujo:\n" (apply 'strcat (mapcar '(lambda (x) (strcat x "\n")) listaBloques))))
        (alert "No se encontraron bloques en el dibujo.")
      )
    )
    (princ "\nNo se encontraron definiciones de bloques en el dibujo.")
  )
  (princ) ; Termina el programa
)
