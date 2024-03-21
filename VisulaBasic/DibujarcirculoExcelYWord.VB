Sub DibujarCirculoDesdeExcel()
    Dim objExcel As Object
    Dim objWorkbook As Object
    Dim objAutoCAD As Object
    Dim radio As Double
    Dim filePath As String
    
    ' Crear una instancia de Excel
    Set objExcel = CreateObject("Excel.Application")
    
    ' Hacer visible la aplicación de Excel (opcional)
    objExcel.Visible = True
    
    ' Mostrar el cuadro de diálogo de búsqueda de archivos para que el usuario seleccione el archivo de Excel
    filePath = objExcel.GetOpenFilename("Archivos de Excel (*.xls; *.xlsx), *.xls; *.xlsx", , "Seleccione el archivo de Excel")
    
    ' Si el usuario cancela la selección del archivo, salir del subproceso
    If filePath = "Falso" Or filePath = "" Then
        MsgBox "No se seleccionó ningún archivo. La operación ha sido cancelada."
        Exit Sub
    End If
    
    ' Abrir el libro de Excel seleccionado
    Set objWorkbook = objExcel.Workbooks.Open(filePath)
    
    ' Especifica la hoja y la celda donde se va a escribir el valor (B2)
    objWorkbook.Sheets("Hoja1").Range("B2").Value = 100
    
    ' Obtener el valor de la celda B2
    radio = objWorkbook.Sheets("Hoja1").Range("B2").Value
    
    ' Crear una instancia de AutoCAD
    Set objAutoCAD = CreateObject("AutoCAD.Application")
    
    ' Hacer visible la aplicación de AutoCAD (opcional)
    objAutoCAD.Visible = True
    
    ' Obtener la referencia al espacio modelo de AutoCAD
    Dim modelSpace As Object
    Set modelSpace = objAutoCAD.ActiveDocument.modelSpace
    
    ' Dibujar el círculo en AutoCAD
    Dim center(0 To 2) As Double
    center(0) = 1000
    center(1) = 1000
    center(2) = 0
    modelSpace.AddCircle center, radio
    
    ' Liberar recursos de AutoCAD
    Set modelSpace = Nothing
    Set objAutoCAD = Nothing
    
    ' Guardar los cambios en el libro de Excel
    objWorkbook.Save
    
    ' Cerrar el libro de Excel
    objWorkbook.Close
    
    ' Liberar recursos de Excel
    objExcel.Quit
    Set objWorkbook = Nothing
    Set objExcel = Nothing
End Sub