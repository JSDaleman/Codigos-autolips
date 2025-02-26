
;Lee el documento .txt donde estan los datos
(defun readFile (filename)
  (setq file (open filename "r"))
  (setq lines '())
  (while (setq line (read-line file))
    (setq lines (cons line lines))
  )
  (close file)
  (reverse lines)
)

;Detector de tipo de delimitador si es , o es un espacio, en caso por defecto se deja un tabulador
(defun detectDelimiter (line)
  (cond
    ((vl-string-search "," line) ",")
    ((vl-string-search " " line) " ")
    (t "\t") ; por defecto, tabulador
  )
)


(defun parseBlockData (lines)
  (setq blockData '())
  (setq currentBlock '())
  (foreach line lines
    (cond
      ((vl-string-search "Block:" line)
       (if currentBlock
         (setq blockData (cons currentBlock blockData)))
       (setq currentBlock (list line)))
      ((vl-string-search "Tag:" line)
       (setq currentBlock (cons line currentBlock)))
      (t
       (setq currentBlock (cons line currentBlock))))
  )
  (if currentBlock
    (setq blockData (cons currentBlock blockData)))
  (reverse blockData)
)

(defun parseProperties (block)
  (setq blockProperties '())
  (setq blockLine (car block))
  (setq delimiter (detectDelimiter blockLine))
  
  (foreach line block
    (cond
      ((vl-string-search "Block:" line)
       (setq blockName (substr line (+ 6 (vl-string-search "Block:" line))
                            (- (vl-string-search (strcat delimiter "X:") line)
                               (+ 6 (vl-string-search "Block:" line)))))
       (setq xPos (atof (substr line (+ 3 (vl-string-search (strcat delimiter "X:") line))
                          (- (vl-string-search (strcat delimiter "Y:") line)
                             (+ 3 (vl-string-search (strcat delimiter "X:") line)))))
       (setq yPos (atof (substr line (+ 3 (vl-string-search (strcat delimiter "Y:") line))
                          (- (vl-string-search (strcat delimiter "Rotation:") line)
                             (+ 3 (vl-string-search (strcat delimiter "Y:") line)))))
       (setq rotation (atof (substr line (+ 9 (vl-string-search (strcat delimiter "Rotation:") line))))))
      
      ((vl-string-search "Tag:" line)
       (setq tagPos (+ 5 (vl-string-search "Tag:" line)))
       (setq valuePos (+ 7 (vl-string-search "Value:" line)))
       (setq delimiterPos (vl-string-search delimiter (substr line tagPos)))
       (setq tag (if delimiterPos
                    (substr line tagPos (- delimiterPos tagPos))
                    (substr line tagPos)))
       (setq value (if valuePos
                     (substr line valuePos)
                     "")) ; Valor predeterminado si no se encuentra "Value:"
       (setq blockProperties (cons (cons tag value) blockProperties)))))
  (list blockName xPos yPos rotation (reverse blockProperties))
)
)
)

;Incerta los atibutos y datos
(defun insertBlock (blockName xPos yPos rotation attributes)
  (command "_.-INSERT" blockName (list xPos yPos) "1" "1" rotation)
  (foreach attr attributes
    (setq tag (car attr))
    (setq value (cdr attr))
    (command "_.-ATTEDIT" "_N" tag "" value (list xPos yPos)))
)

;Funcion de procesamiento de los datos de cada bloque
(defun processFile (filename)
  (setq lines (readFile filename))
  (setq blocks (parseBlockData lines))
  (foreach block blocks
    (setq properties (parseProperties block))
    (apply 'insertBlock properties))
)

;Funcion creada para ser llamada desde autocad por el usuario
(defun c:reconstruirTXT ()
  (setq filename (getfiled "Select the properties file" "" "txt" 4))
  (if filename
    (processFile filename))
)
