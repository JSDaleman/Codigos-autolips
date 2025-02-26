(defun readFile (filename)
  ;; Lee el archivo línea por línea y devuelve una lista de las líneas
  (setq file (open filename "r"))
  (setq lines '())
  (while (setq line (read-line file))
    (setq lines (cons line lines))
  )
  (close file)
  (reverse lines)
)

(defun detectDelimiter (line)
  ;; Detecta si el delimitador es una coma o un espacio
  (cond
    ((vl-string-search "," line) ",")
    ((vl-string-search " " line) " ")
    (t ""))
)

(defun parseBlockData (lines)
  ;; Agrupa líneas de bloques y atributos en listas separadas
  (setq blocks '())
  (setq currentBlock '())
  (foreach line lines
    (if (vl-string-search "Block:" line)
      (progn
        (if currentBlock
          (setq blocks (cons (reverse currentBlock) blocks)))
        (setq currentBlock (list line)))
      (setq currentBlock (cons line currentBlock)))
  )
  (if currentBlock (setq blocks (cons (reverse currentBlock) blocks)))
  (reverse blocks)
)

(defun parseProperties (block)
  (setq blockProperties '())
  (setq blockLine (car block))
  (setq delimiter (detectDelimiter blockLine))
  
  ;; Parseamos la línea del bloque
  (setq blockNamePos (vl-string-search (strcat delimiter "X:") blockLine))
  (if blockNamePos
    (setq blockName (substr blockLine 7 (- blockNamePos 7)))  ;; Si se encuentra "X:", extrae el nombre del bloque
    (setq blockName (substr blockLine 7))  ;; Si no se encuentra, extrae desde el inicio hasta el final
  )

  (setq xPos (atof (substr blockLine (+ 3 (vl-string-search (strcat delimiter "X:") blockLine)))))
  (setq yPos (atof (substr blockLine (+ 3 (vl-string-search (strcat delimiter "Y:") blockLine)))))
  (setq rotation (atof (substr blockLine (+ 9 (vl-string-search "Rotation:" blockLine)))))
  
  ;; Parseamos las líneas de atributos
  (foreach line (cdr block)
    (if (vl-string-search "Tag:" line)
      (progn
        (setq tagPos (+ 5 (vl-string-search "Tag:" line)))
        (setq delimiterPos (vl-string-search delimiter (substr line tagPos)))
        (if delimiterPos
          (setq tag (substr line tagPos (- delimiterPos tagPos)))
          (setq tag (substr line tagPos)))  ;; Si no se encuentra el delimitador, tomamos el resto de la línea
        (setq valuePos (+ 7 (vl-string-search "Value:" line)))
        (setq value (substr line valuePos))
        (setq blockProperties (cons (cons tag value) blockProperties)))))
  
  (list blockName xPos yPos rotation (reverse blockProperties))
)

(defun blockExists (blockName)
  ;; Verifica si el bloque ya está en el dibujo actual
  (tblsearch "BLOCK" blockName)
)

(defun insertBlock (blockName xPos yPos rotation attributes)
  (if (not (blockExists blockName))
    (progn
      ;; Si el bloque no existe, intentamos insertarlo desde un archivo DWG
      (setq currentDir (getvar "DWGPREFIX")) ;; Obtiene el directorio del archivo DWG actual
      (setq blockFile (strcat currentDir blockName ".dwg")) ;; Construye el path completo del bloque
      (if (findfile blockFile)
        (command "_.-INSERT" blockFile) ;; Inserta el bloque desde el archivo DWG
        (prompt (strcat "\nNo se encontró el archivo de bloque: " blockFile)) ;; Mensaje de error si no se encuentra
      )
    )
  )

  ;; Una vez que el bloque está cargado o ya existía, se inserta en la posición y con la rotación dada
  (command "_.-INSERT" blockName (list xPos yPos) "1" "1" rotation)

  ;; Asignación de valores a los atributos del bloque
  (foreach attr attributes
    (setq tag (car attr))  ;; etiqueta del atributo
    (setq value (cdr attr))  ;; valor del atributo
    ;; Seleccionamos el bloque insertado y modificamos sus atributos
    (command "_.-ATTEDIT" "_N" "_TAG" tag "_VALOR" value "_ALL" (list xPos yPos)))
)

(defun processFile (filename)
  ;; Procesa el archivo y reconstruye los bloques
  (setq lines (readFile filename))
  (setq blocks (parseBlockData lines))
  (foreach block blocks
    (setq properties (parseProperties block))
    (apply 'insertBlock properties))
)

(defun c:reconstructBlocksFromTxt ()
  ;; Abrir el explorador de archivos para seleccionar el archivo .txt
  (setq filename (getfiled "Seleccione el archivo .txt" "" "txt" 4))
  (if filename
    (processFile filename))
)