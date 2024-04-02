Sub GenerateLettersFromExcel()
    Dim excelApp As Object
    Dim excelWorkbook As Object
    Dim excelWorksheet As Object
    Dim wordApp As Object
    Dim doc As Object
    Dim i As Integer
    
    ' Open Excel application and workbook
    Set excelApp = CreateObject("Excel.Application")
    excelApp.Visible = False ' You can set this to True for debugging purposes
    
    ' Change the path to your Excel workbook
    Set excelWorkbook = excelApp.Workbooks.Open("C:\Users\ESTUDIANTE\Documents\mierda.xlsx")
    
    ' Assuming the data is in the first worksheet
    Set excelWorksheet = excelWorkbook.Worksheets(1)
    
    ' Open Word application
    Set wordApp = CreateObject("Word.Application")
    wordApp.Visible = True
    
    ' Loop through each row in Excel
    For i = 2 To excelWorksheet.Cells(excelWorksheet.Rows.Count, 1).End(-4162).Row ' -4162 represents xlUp
        ' Create a new document
        Set doc = wordApp.Documents.Add
        
        ' Assuming the data is in columns A, B, and C (change as needed)
        With doc.Content
            ' Replace placeholders with actual data
            .InsertAfter "Dear " & excelWorksheet.Cells(i, 1).Value & " " & excelWorksheet.Cells(i, 2).Value & "," & vbCrLf
            .InsertAfter "Nos complace anuncias que ha sido aceptado en el programa" & excelWorksheet.Cells(i, 3).Value
            .InsertAfter "Lo felicitamos en estos momentos" & vbCrLf
            ' Add other content as needed
        End With
        
        ' Save the document
        doc.SaveAs "C:\Users\ESTUDIANTE\Documents\Cartas\Letter_" & i - 1 & ".docx" ' Adjust the save path
        
        ' Close the document
        doc.Close
        
        ' Optionally, you can pause or add some delay here
        ' Sleep 1000 ' 1000 milliseconds (1 second)
    Next i
    
    ' Close Excel workbook and quit Excel
    excelWorkbook.Close False
    excelApp.Quit
    
    ' Clean up
    Set excelWorksheet = Nothing
    Set excelWorkbook = Nothing
    Set excelApp = Nothing
    
    ' Quit Word
    wordApp.Quit
    Set wordApp = Nothing
End Sub
