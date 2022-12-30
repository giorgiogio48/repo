Imports System
Imports iTextSharp.text.pdf
Imports System.Security.AccessControl
Imports iTextSharp.text.pdf.parser
Imports System.IO
Imports System.Runtime.InteropServices
Imports System.Text
Imports iTextSharp.text
Imports System.Net.WebRequestMethods
Imports iTextSharp.xmp.impl

Namespace SHGetFileInfoDemo

    Partial Public Class Form1
        Inherits Form
        Dim counter As Integer = 0
        Dim varstr As String
        Private sysIcons As New SystemIconsImageList()
        Private pages As Object

        Public Sub New()
            InitializeComponent()
        End Sub

        Private Sub Form1_Load(ByVal sender As Object, ByVal e As EventArgs) Handles Me.Load

            Dim folderName As String = Nothing

            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If

            Me.Text = folderName
            Me.WindowState = FormWindowState.Normal

            TextBox1.Select()
            RadioButton1.Checked = True
            Label2.Visible = False
            Label1.Visible = False
            listView1.ShowItemToolTips = True
            listView1.View = View.Details
            listView1.HeaderStyle = ColumnHeaderStyle.None
            listView1.FullRowSelect = True
            listView1.Columns.Add("", -2)
        End Sub

        Private Sub AddItem(ByVal name As String, ByVal path As String)
            Dim item As New ListViewItem(name)
            item.ImageIndex = sysIcons.GetIconIndex(path)
            item.Tag = path
            listView1.Items.Add(item)
        End Sub

        '
        ' Recursive function which keeps moving down the directory tree until a given depth.
        Public Sub GetFiles(ByVal strFileFilter As String, ByVal strDirectory As String, ByVal intDepthLimit As Integer, ByVal intCurrentDepth As Integer)
            Try
                Dim folderInfo As New DirectoryInfo(strDirectory)

                ' Is the current depth on this recursion less than our limit?
                ' If so, find any directories and get into them by calling GetFiles recursively (incrementing depth count)
                If intCurrentDepth < intDepthLimit Then
                    Dim directories() As DirectoryInfo
                    directories = folderInfo.GetDirectories()

                    For Each fDirectory In directories
                        ' Recursively call ourselves incrementing the depth using the given folder path.
                        GetFiles(strFileFilter, fDirectory.FullName, intDepthLimit, intCurrentDepth + 1)
                    Next
                End If

                ' After we can't go further down, add any files which match our filter to listbox (in this case lstFiles)
                Dim files() As FileInfo
                files = folderInfo.GetFiles(strFileFilter)
                Dim sh As New NativeMethods.SHFILEINFO

                Dim exeIcon As System.Drawing.Icon
                listView1.BeginUpdate()
                For Each fFile In files

                    Dim reader = My.Computer.FileSystem.ReadAllText(fFile.FullName,
   System.Text.Encoding.ASCII)
                    If reader.Contains(TextBox1.Text) Then
                        If Not fFile.FullName.EndsWith("dll") And Not fFile.FullName.EndsWith("exe") And Not fFile.FullName.EndsWith("res") And Not fFile.FullName.EndsWith("tx") _
And Not fFile.FullName.EndsWith("pak") And Not fFile.FullName.EndsWith("xls") And Not fFile.FullName.EndsWith("pdf") And Not fFile.FullName.EndsWith("pcv") Then
                            Application.DoEvents()
                            listView1.SmallImageList = ImageList1
                            listView1.LargeImageList = ImageList1
                            ImageList1.ColorDepth = ColorDepth.Depth32Bit
                            sh = NativeMethods.GetInfoFromShell(fFile.FullName)
                            exeIcon = CType(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, Icon)
                            ImageList1.ColorDepth = ColorDepth.Depth32Bit
                            sh = NativeMethods.GetInfoFromShell(fFile.FullName)


                            exeIcon = CType(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, Icon)

                            If (ImageList1.Images.ContainsKey(fFile.FullName)) Then
                                listView1.Items.Add(fFile.FullName, fFile.FullName)
                            ElseIf (Not exeIcon Is Nothing) Then
                                ImageList1.Images.Add(fFile.FullName, exeIcon)
                                listView1.Items.Add(fFile.FullName, fFile.FullName)
                            Else
                                listView1.Items.Add(fFile.FullName)
                            End If


                        End If
                    End If
                Next
                listView1.EndUpdate()

                remov()
            Catch ex As Exception

            End Try
        End Sub
        Public Sub GetFiles1(ByVal strFileFilter As String, ByVal strDirectory As String, ByVal intDepthLimit As Integer, ByVal intCurrentDepth As Integer)
            Try
                Dim folderInfo As New DirectoryInfo(strDirectory)

                ' Is the current depth on this recursion less than our limit?
                ' If so, find any directories and get into them by calling GetFiles recursively (incrementing depth count)
                If intCurrentDepth < intDepthLimit Then
                    Dim directories() As DirectoryInfo
                    directories = folderInfo.GetDirectories()

                    For Each fDirectory In directories
                        ' Recursively call ourselves incrementing the depth using the given folder path.
                        GetFiles1(strFileFilter, fDirectory.FullName, intDepthLimit, intCurrentDepth + 1)
                    Next
                End If

                ' After we can't go further down, add any files which match our filter to listbox (in this case lstFiles)

                Dim files() As FileInfo
                files = folderInfo.GetFiles(strFileFilter)


                For Each namo In files

                    counter = counter + 1

                    Dim text As StringBuilder = New StringBuilder

                    Dim pdfReader As PdfReader = New PdfReader(namo.FullName)
                    Dim page As Integer = 1
                    Do While (page <= pdfReader.NumberOfPages)
                        Dim strategy As ITextExtractionStrategy = New SimpleTextExtractionStrategy
                        Dim currentText As String = PdfTextExtractor.GetTextFromPage(pdfReader, page, strategy)
                        currentText = Encoding.UTF8.GetString(ASCIIEncoding.Convert(Encoding.Default, Encoding.UTF8, Encoding.Default.GetBytes(currentText)))
                        text.Append(currentText)
                        page = (page + 1)
                        If currentText.Contains(TextBox1.Text) Then

                            ImageList1.ColorDepth = ColorDepth.Depth32Bit
                            Dim sh As New NativeMethods.SHFILEINFO

                            sh = NativeMethods.GetInfoFromShell(namo.FullName)
                            Dim theIcon As Icon
                            If sh.hIcon <> IntPtr.Zero Then



                                listView1.LargeImageList = ImageList1
                                listView1.SmallImageList = ImageList1
                                listView1.View = View.SmallIcon
                                theIcon = DirectCast(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, System.Drawing.Icon)


                                ImageList1.Images.Add(namo.FullName, theIcon.Clone)
                                listView1.Items.Add(namo.FullName, namo.FullName)
                            End If
                        End If
                    Loop

                    pdfReader.Close()


                Next
                remov()
                listView1.View = View.Details
                listView1.HeaderStyle = ColumnHeaderStyle.None
                listView1.FullRowSelect = True
                listView1.Columns.Add("", -2)

            Catch ex As Exception

            Finally

            End Try
        End Sub
        Public Sub Searpdftext(ByVal strFileFilter As String, ByVal strDirectory As String)
            Try
                Dim folderInfo As New DirectoryInfo(strDirectory)

                ' Is the current depth on this recursion less than our limit?
                ' If so, find any directories and get into them by calling GetFiles recursively (incrementing depth count)


                ' After we can't go further down, add any files which match our filter to listbox (in this case lstFiles)

                Dim files() As FileInfo
                files = folderInfo.GetFiles(strFileFilter)


                For Each namo In files

                    counter = counter + 1

                    Dim text As StringBuilder = New StringBuilder

                    Dim pdfReader As PdfReader = New PdfReader(namo.FullName)
                    Dim page As Integer = 1
                    Do While (page <= pdfReader.NumberOfPages)
                        Dim strategy As ITextExtractionStrategy = New SimpleTextExtractionStrategy
                        Dim currentText As String = PdfTextExtractor.GetTextFromPage(pdfReader, page, strategy)
                        currentText = Encoding.UTF8.GetString(ASCIIEncoding.Convert(Encoding.Default, Encoding.UTF8, Encoding.Default.GetBytes(currentText)))
                        text.Append(currentText)
                        page = (page + 1)
                        If currentText.ToLower.Contains(TextBox1.Text.ToLower) Then

                            ImageList1.ColorDepth = ColorDepth.Depth32Bit
                            Dim sh As New NativeMethods.SHFILEINFO

                            sh = NativeMethods.GetInfoFromShell(namo.FullName)
                            Dim theIcon As Icon
                            If sh.hIcon <> IntPtr.Zero Then



                                listView1.LargeImageList = ImageList1
                                listView1.SmallImageList = ImageList1
                                listView1.View = View.SmallIcon
                                theIcon = DirectCast(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, System.Drawing.Icon)


                                ImageList1.Images.Add(namo.FullName, theIcon.Clone)
                                listView1.Items.Add(namo.FullName, namo.FullName)
                            End If
                        End If
                    Loop

                    pdfReader.Close()


                Next
                remov()
                listView1.View = View.Details
                listView1.HeaderStyle = ColumnHeaderStyle.None
                listView1.FullRowSelect = True
                listView1.Columns.Add("", -2)

            Catch ex As Exception

            Finally

            End Try
        End Sub


        Private Sub Searchfiles(ByVal path As String, ByVal boolFile As Boolean)
            listView1.Items.Clear()
            Dim folderName As String = Nothing

            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If

            Try

                Label2.Visible = True
                Label2.Refresh()
                Dim diTop As New DirectoryInfo(folderName)
                Try
                    For Each fi In diTop.EnumerateFiles()
                        Try
                            If fi.ToString.ToLower.Contains(TextBox1.Text.ToLower) Then
                                If fi.ToString.EndsWith("txt") Or fi.ToString.EndsWith("jpg") Or fi.ToString.EndsWith("doc") Or fi.ToString.EndsWith("pdf") _
                                     Or fi.ToString.EndsWith("xls") Or fi.ToString.EndsWith("calc") Or fi.ToString.EndsWith("odt") _
                                     Or fi.ToString.EndsWith("exe") Or fi.ToString.EndsWith("iss") Then

                                    ImageList1.ColorDepth = ColorDepth.Depth32Bit
                                    Dim sh As New NativeMethods.SHFILEINFO

                                    sh = NativeMethods.GetInfoFromShell(fi.FullName)
                                    Dim theIcon As Icon
                                    If sh.hIcon <> IntPtr.Zero Then



                                        listView1.LargeImageList = ImageList1
                                        listView1.SmallImageList = ImageList1
                                        listView1.View = View.SmallIcon
                                        theIcon = DirectCast(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, System.Drawing.Icon)


                                        ImageList1.Images.Add(fi.FullName, theIcon.Clone)
                                        listView1.Items.Add(fi.FullName, fi.FullName)
                                    End If
                                End If
                            End If
                        Catch UnAuthTop As UnauthorizedAccessException
                            Console.WriteLine("{0}", UnAuthTop.Message)
                        End Try
                    Next
                    If boolFile = True Then
                        For Each di In diTop.EnumerateDirectories("*")
                            Try
                                For Each fi In di.EnumerateFiles("*", SearchOption.AllDirectories)
                                    If fi.ToString.ToLower.Contains(TextBox1.Text.ToLower) Then
                                        If fi.ToString.EndsWith("txt") Or fi.ToString.EndsWith("jpg") Or fi.ToString.EndsWith("doc") Or fi.ToString.EndsWith("pdf") _
                                     Or fi.ToString.EndsWith("xls") Or fi.ToString.EndsWith("calc") Or fi.ToString.EndsWith("odt") _
                                     Or fi.ToString.EndsWith("exe") Or fi.ToString.EndsWith("iss") Then
                                            ImageList1.ColorDepth = ColorDepth.Depth32Bit
                                            Dim sh As New NativeMethods.SHFILEINFO

                                            sh = NativeMethods.GetInfoFromShell(fi.FullName)
                                            Dim theIcon As Icon
                                            If sh.hIcon <> IntPtr.Zero Then



                                                listView1.LargeImageList = ImageList1
                                                listView1.SmallImageList = ImageList1
                                                listView1.View = View.SmallIcon
                                                theIcon = DirectCast(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, System.Drawing.Icon)


                                                ImageList1.Images.Add(fi.FullName, theIcon.Clone)
                                                listView1.Items.Add(fi.FullName, fi.FullName)
                                            End If
                                        End If
                                    End If
                                Next
                            Catch UnAuthSubDir As UnauthorizedAccessException
                                Console.WriteLine("UnAuthSubDir: {0}", UnAuthSubDir.Message)
                            End Try
                        Next
                    End If
                Catch LongPath As Exception
                    Console.WriteLine("{0}", LongPath.Message)
                End Try


                remov()

                listView1.View = View.Details
                listView1.HeaderStyle = ColumnHeaderStyle.None
                listView1.FullRowSelect = True
                listView1.Columns.Add("", -2)


            Catch s As Exception
                MsgBox(s.Message)
            End Try
            Label2.Visible = False
        End Sub
        Private Sub listView1_MouseUp(sender As Object, e As MouseEventArgs) Handles listView1.MouseUp
            Try
                If e.Button = MouseButtons.Right Then
                    Dim folderName As String = Nothing

                    If (Environment.GetCommandLineArgs.Length > 1) Then
                        folderName = Environment.GetCommandLineArgs(1)

                    End If
                    Dim selectedFile As String = listView1.SelectedItems(0).Text

                    Dim ctxMnu As New ShellTestApp.ShellContextMenu
                    Dim arrFI() As FileInfo = New FileInfo((1) - 1) {}
                    arrFI(0) = New FileInfo(IO.Path.Combine(folderName, selectedFile))
                    ctxMnu.ShowContextMenu(arrFI, PointToScreen(New Point))

                End If
            Catch
            End Try
        End Sub


        Private Function IsValidPdf(ByVal filepath As String) As Boolean

        End Function
        Private Sub remov()
            Try
                Dim i, j As Integer
                With Me.listView1
                    Do Until i > .Items.Count - 2
                        Dim text As String = .Items(i).Text
                        j = i + 1
                        Do Until j > .Items.Count - 1
                            If .Items(j).Text = text Then
                                .Items.RemoveAt(j)
                            Else
                                j += 1
                            End If
                        Loop
                        i += 1
                    Loop
                End With
            Catch
            End Try
        End Sub



        Public Function FileMatches(folderPath As String, filePattern As String, phrase As String) As Boolean
            For Each fileName As String In Directory.GetFiles(folderPath, filePattern)
                If fileName.ToLower().Contains(phrase.ToLower()) Then
                    Return True
                End If
            Next
            Return False
        End Function




        Private Sub listview1_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles listView1.MouseDoubleClick
            Try
                Dim folderName As String = Nothing

                If (Environment.GetCommandLineArgs.Length > 1) Then
                    folderName = Environment.GetCommandLineArgs(1)

                End If
                Dim selectedFile As String = listView1.SelectedItems(0).Text
                System.Diagnostics.Process.Start(IO.Path.Combine(folderName, selectedFile))


            Catch ex As Exception

            End Try
        End Sub




        Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
            Try
                Dim folderName As String = Nothing



                If (Environment.GetCommandLineArgs.Length > 1) Then
                    folderName = Environment.GetCommandLineArgs(1)

                End If
                If RadioButton1.Checked Then
                    Searchfiles(folderName, SearchOption.TopDirectoryOnly)
                Else
                    Searchfiles(folderName, True)

                End If
            Catch ex As Exception
                MsgBox(ex.Message & " Delete this folder by hand")
            End Try
        End Sub

        Private Sub giorgio(ByVal fol As String)
            Dim folderName As String = Nothing



            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If
            Dim sh As New NativeMethods.SHFILEINFO
            Dim SourceDir As FileInfo = New FileInfo(fol)
            Dim exeIcon As System.Drawing.Icon
            Dim list As System.Collections.ObjectModel.
   ReadOnlyCollection(Of String)
            list = My.Computer.FileSystem.FindInFiles(folderName,
     TextBox1.Text, True, FileIO.SearchOption.SearchTopLevelOnly)
            For Each fname As String In list
                If Not fname.EndsWith("dll") And Not fname.EndsWith("exe") And Not fname.EndsWith("res") And Not fname.EndsWith("tx") _
                                            And Not fname.EndsWith("pak") And Not fname.EndsWith("xls") And Not fname.EndsWith("pdf") Then
                    Application.DoEvents()
                    listView1.SmallImageList = ImageList1
                    listView1.LargeImageList = ImageList1
                    ImageList1.ColorDepth = ColorDepth.Depth32Bit
                    sh = NativeMethods.GetInfoFromShell(fname.ToString)


                    exeIcon = CType(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, Icon)


                    ImageList1.ColorDepth = ColorDepth.Depth32Bit
                    sh = NativeMethods.GetInfoFromShell(fname.ToString)


                    exeIcon = CType(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, Icon)

                    If (ImageList1.Images.ContainsKey(fname.ToString)) Then
                        listView1.Items.Add(fname.ToString, fname.ToString)
                    ElseIf (Not exeIcon Is Nothing) Then
                        ImageList1.Images.Add(fname.ToString, exeIcon)
                        listView1.Items.Add(fname.ToString, fname.ToString)
                    Else
                        listView1.Items.Add(fname.ToString)
                    End If


                End If
            Next

            Application.DoEvents()

            remov()
            listView1.View = View.Details
            listView1.HeaderStyle = ColumnHeaderStyle.None
            listView1.FullRowSelect = True
            listView1.Columns.Add("", -2)

        End Sub
        Private Sub giorgio1()
            Dim folderName As String = Nothing



            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If

            GetFiles("*.*", folderName, 1, 0)


            Application.DoEvents()

            remov()
            listView1.View = View.Details
            listView1.HeaderStyle = ColumnHeaderStyle.None
            listView1.FullRowSelect = True
            listView1.Columns.Add("", -2)
        End Sub

        Private Sub giorgio()
            Dim folderName As String = Nothing



            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If
            Dim sh As New NativeMethods.SHFILEINFO

            Dim exeIcon As System.Drawing.Icon
            Dim list As System.Collections.ObjectModel.
   ReadOnlyCollection(Of String)
            list = My.Computer.FileSystem.FindInFiles(folderName,
     TextBox1.Text, True, FileIO.SearchOption.SearchTopLevelOnly)
            For Each fname As String In list
                If Not fname.EndsWith("dll") And Not fname.EndsWith("exe") And Not fname.EndsWith("res") And Not fname.EndsWith("tx") _
                                            And Not fname.EndsWith("pak") And Not fname.EndsWith("xls") And Not fname.EndsWith("pdf") Then
                    Application.DoEvents()
                    listView1.SmallImageList = ImageList1
                    listView1.LargeImageList = ImageList1
                    ImageList1.ColorDepth = ColorDepth.Depth32Bit
                    sh = NativeMethods.GetInfoFromShell(fname.ToString)


                    exeIcon = CType(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, Icon)


                    ImageList1.ColorDepth = ColorDepth.Depth32Bit
                    sh = NativeMethods.GetInfoFromShell(fname.ToString)


                    exeIcon = CType(System.Drawing.Icon.FromHandle(sh.hIcon).Clone, Icon)

                    If (ImageList1.Images.ContainsKey(fname.ToString)) Then
                        listView1.Items.Add(fname.ToString, fname.ToString)
                    ElseIf (Not exeIcon Is Nothing) Then
                        ImageList1.Images.Add(fname.ToString, exeIcon)
                        listView1.Items.Add(fname.ToString, fname.ToString)
                    Else
                        listView1.Items.Add(fname.ToString)
                    End If


                End If
            Next

            Application.DoEvents()

            remov()
            listView1.View = View.Details
            listView1.HeaderStyle = ColumnHeaderStyle.None
            listView1.FullRowSelect = True
            listView1.Columns.Add("", -2)
        End Sub

        Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
            Dim folderName As String = Nothing



            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If
            If RadioButton1.Checked Then
                listView1.Items.Clear()
                listView1.BeginUpdate()
                Label2.Visible = True
                Label2.Refresh()
                Dim results As String = Nothing
                Dim stopwatch1 As New System.Diagnostics.Stopwatch()
                stopwatch1.Start()
                giorgio()
                Searpdftext("*.pdf", folderName)
                Dim ts As TimeSpan = stopwatch1.Elapsed
                Label1.Visible = True
                Label1.Text = "Search elapsed time: " & String.Format("{0} seconds", ts.Seconds)
                stopwatch1.Stop()


                remov()
                Label2.Visible = False
                listView1.EndUpdate()
            ElseIf RadioButton2.Checked Then
                Try

                    listView1.Items.Clear()

                    Label2.Visible = True
                    Label2.Refresh()
                    Dim results As String = Nothing
                    Dim stopwatch1 As New System.Diagnostics.Stopwatch()
                    stopwatch1.Start()
                    listView1.BeginUpdate()
                    giorgio1()
                    ReadPdfFile()
                    listView1.EndUpdate()
                    remov()
                    Label1.Visible = True
                    Dim ts As TimeSpan = stopwatch1.Elapsed

                    Label1.Text = "Search elapsed time: " & String.Format("{0} seconds", ts.Seconds)
                    stopwatch1.Stop()
                    Label2.Visible = False
                Catch ex As Exception
                    MsgBox(ex.Message & " Delete this folder by hand")
                End Try
            End If
        End Sub





        Public Function ReadPdfFile()
            Dim folderName As String = Nothing

            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If

            GetFiles1("*.pdf", folderName, 1, 0)



        End Function

        Private Sub Button3_Click(sender As Object, e As EventArgs)
            Dim folderName As String = Nothing

            If (Environment.GetCommandLineArgs.Length > 1) Then
                folderName = Environment.GetCommandLineArgs(1)

            End If
            listView1.BeginUpdate()
            GetFiles("*.*", folderName, 1, 0)

            GetFiles1("*.pdf", folderName, 1, 0)
            listView1.EndUpdate()
        End Sub

    End Class
    Friend Class NativeMethods

        <DllImport("shell32.dll", CharSet:=CharSet.Auto)>
        Private Shared Function SHGetFileInfo(ByVal pszPath As String,
        ByVal dwFileAttributes As UInteger, ByRef psfi As SHFILEINFO,
        ByVal cbFileInfo As Integer, ByVal uFlags As UInteger) As IntPtr
        End Function

        <DllImport("user32.dll")>
        Private Shared Function DestroyIcon(ByVal hIcon As IntPtr) As IntPtr
        End Function

        <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Auto, Pack:=1)>
        Public Structure SHFILEINFO
            Implements IDisposable

            Public hIcon As IntPtr
            Public iIcon As Integer
            Public dwAttributes As UInteger
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=260)>
            Public szDisplayName As String
            <MarshalAs(UnmanagedType.ByValTStr, SizeConst:=80)>
            Public szTypeName As String

            Public Sub Dispose() Implements System.IDisposable.Dispose
                DestroyIcon(Me.hIcon)
            End Sub
        End Structure

        Private Const SHGFI_ICON As UInt32 = &H100
        Private Const SHGFI_DISPLAYNAME As UInt32 = &H200
        Private Const SHGFI_TYPENAME As UInt32 = &H400
        Private Const SHGFI_LARGEICON As UInt32 = 0
        Private Const SHGFI_SMALLICON As UInt32 = 1

        Public Shared Function GetInfoFromShell(ByVal FileName As String) As SHFILEINFO
            Dim shfi As SHFILEINFO = New SHFILEINFO
            shfi.hIcon = IntPtr.Zero
            SHGetFileInfo(FileName, 0, shfi, Marshal.SizeOf(shfi), SHGFI_TYPENAME Or SHGFI_ICON Or SHGFI_SMALLICON)
            Return shfi
        End Function

    End Class
End Namespace
