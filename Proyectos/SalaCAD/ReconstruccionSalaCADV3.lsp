(defun c:ReconstruirBloquesConDatosExtendidos (/ archivo linea datos bloque x y rotacion tag value lista-atributos)
  (setq archivo (getfiled "Seleccione el archivo de datos" "" "txt" 4)) ; Abrir el buscador de archivos
  (if archivo
    (progn
      (setq archivo (open archivo "r")) ; Abrir el archivo en modo lectura
      (while (setq linea (read-line archivo)) ; Leer línea por línea
        (cond
          ;; Detectar si es una línea de bloque
          ((vl-string-search "Block:" linea)
           (setq lista-atributos nil) ; Inicializar los atributos para el nuevo bloque
           (setq datos (parse-datos linea)) ; Extraer datos del bloque
           (setq bloque (cadr datos)) ; Nombre del bloque
           (setq x (atof (vl-string-subst "" "X:" (cadr (member "X:" datos))))) ; Coordenada X
           (setq y (atof (vl-string-subst "" "Y:" (cadr (member "Y:" datos))))) ; Coordenada Y
           (setq rotacion (atof (vl-string-subst "" "Rotation:" (cadr (member "Rotation:" datos))))) ; Rotación
          )
          ;; Detectar si es una línea de atributo
          ((vl-string-search "Tag:" linea)
           (setq datos (parse-datos-atributos linea)) ; Extraer datos de atributos
           (setq tag (cadr datos)) ; Nombre del atributo
           (setq value (cadr (member "Value:" datos))) ; Valor del atributo
           (setq lista-atributos (append lista-atributos (list (list tag value)))) ; Agregar atributo a la lista
          )
        )
        ;; Si se llega a otra línea de bloque o al final del archivo, insertar el bloque anterior
        (if (or (not (read-line archivo)) (vl-string-search "Block:" linea))
          (progn
            ;; Insertar el bloque en AutoCAD
            (command "_.-insert" bloque (list x y) 1 1 rotacion)
            ;; Asignar los atributos al bloque
            (foreach atributo lista-atributos
              (command "_.-attedit" "" "" (car atributo) (cadr atributo))
            )
            (setq lista-atributos nil) ; Limpiar la lista para el siguiente bloque
          )
        )
      )
      (close archivo) ; Cerrar el archivo al finalizar
    )
  )
  (princ)
)

;; Función auxiliar para extraer los datos del bloque, ya sea por espacios o comas
(defun parse-datos (linea)
  (setq delim (if (vl-string-search "," linea) "," " ")) ; Detectar delimitador
  (setq datos (parse-list linea delim)) ; Separar datos por el delimitador adecuado
  datos
)

;; Función auxiliar para extraer los datos de los atributos, ya sea por espacios o comas
(defun parse-datos-atributos (linea)
  (setq delim (if (vl-string-search "," linea) "," " ")) ; Detectar delimitador
  (setq datos (parse-list linea delim)) ; Separar datos por el delimitador adecuado
  datos
)

;; Función auxiliar para parsear una lista de datos por un delimitador
(defun parse-list (str delim)
  ;; Divide la cadena manualmente eliminando el delimitador y separando
  (setq lista '())
  (while (setq pos (vl-string-search delim str))
    (setq lista (append lista (list (vl-string-trim " " (substr str 1 pos)))))
    (setq str (substr str (+ pos (strlen delim))))) ; Actualiza la cadena eliminando lo procesado
  (setq lista (append lista (list (vl-string-trim " " str)))) ; Agregar la última parte
  lista
)
