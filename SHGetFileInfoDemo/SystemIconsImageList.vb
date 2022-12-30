Imports System
Imports System.Runtime.InteropServices
Imports System.IO
Imports System.Drawing

Namespace System.Windows.Forms
	Public Class SystemIconsImageList
		Implements IDisposable

		#Region "Win32 declarations"
		Private Const SHGFI_ICON As UInteger = &H100
		Private Const SHGFI_LARGEICON As UInteger = &H0
		Private Const SHGFI_SMALLICON As UInteger = &H1

		<StructLayout(LayoutKind.Sequential)>
		Public Structure SHFILEINFO
			Public hIcon As IntPtr
			Public iIcon As IntPtr
			Public dwAttributes As UInteger
			<MarshalAs(UnmanagedType.ByValTStr, SizeConst := 260)>
			Public szDisplayName As String
			<MarshalAs(UnmanagedType.ByValTStr, SizeConst := 80)>
			Public szTypeName As String
		End Structure

		<DllImport("shell32.dll")>
		Public Shared Function SHGetFileInfo(ByVal pszPath As String, ByVal dwFileAttributes As UInteger, ByRef psfi As SHFILEINFO, ByVal cbSizeFileInfo As UInteger, ByVal uFlags As UInteger) As IntPtr
		End Function
		#End Region

		#Region "Fields"
		Private _smallImageList As New ImageList()
		Private _largeImageList As New ImageList()
		Private _disposed As Boolean = False
		#End Region

		#Region "Properties"
		''' <summary>
		''' Gets System.Windows.Forms.ImageList with small icons in. Assign this property to SmallImageList of ListView, TreeView etc.
		''' </summary>
		Public ReadOnly Property SmallIconsImageList() As ImageList
			Get
				Return _smallImageList
			End Get
		End Property

		''' <summary>
		''' Gets System.Windows.Forms.ImageList with large icons in. Assign this property to LargeImageList of ListView, TreeView etc.
		''' </summary>
		Public ReadOnly Property LargeIconsImageList() As ImageList
			Get
				Return _largeImageList
			End Get
		End Property

		''' <summary>
		''' Gets number of icons were loaded
		''' </summary>
		Public ReadOnly Property Count() As Integer
			Get
				Return _smallImageList.Images.Count
			End Get
		End Property
		#End Region

		#Region "Constructor/Destructor"
		''' <summary>
		''' Default constructor
		''' </summary>
		Public Sub New()
			MyBase.New()
			_smallImageList.ColorDepth = ColorDepth.Depth32Bit
			_smallImageList.ImageSize = SystemInformation.SmallIconSize

			_largeImageList.ColorDepth = ColorDepth.Depth32Bit
			_largeImageList.ImageSize = SystemInformation.IconSize
		End Sub

		Private Sub CleanUp(ByVal disposing As Boolean)
			If Not Me._disposed Then
				If disposing Then
					_smallImageList.Dispose()
					_largeImageList.Dispose()
				End If
			End If
			_disposed = True
		End Sub

		''' <summary>
		''' Performs resource cleaning
		''' </summary>
		Public Sub Dispose() Implements IDisposable.Dispose
			CleanUp(True)
			GC.SuppressFinalize(Me)
		End Sub

		Protected Overrides Sub Finalize()
			CleanUp(False)
		End Sub
		#End Region

		#Region "Public Methods"
		''' <summary>
		''' Returns index of an icon based on FileName. Note: File should exists!
		''' </summary>
		''' <param name="FileName">Name of an existing File or Directory</param>
		''' <returns>Index of an Icon</returns>
		Public Function GetIconIndex(ByVal FileName As String) As Integer
			Dim shinfo As New SHFILEINFO()

			Dim info As New FileInfo(FileName)

			Dim ext As String = info.Extension
			If String.IsNullOrEmpty(ext) Then
				If (info.Attributes And FileAttributes.Directory) <> 0 Then
					ext = "5EEB255733234c4dBECF9A128E896A1E" ' for directories
				Else
					ext = "F9EB930C78D2477c80A51945D505E9C4" ' for files without extension
				End If
			Else
				If ext.Equals(".exe", StringComparison.InvariantCultureIgnoreCase) OrElse ext.Equals(".lnk", StringComparison.InvariantCultureIgnoreCase) Then
					ext = info.Name
				End If
			End If

			If _smallImageList.Images.ContainsKey(ext) Then
				Return _smallImageList.Images.IndexOfKey(ext)
			Else
				SHGetFileInfo(FileName, 0, shinfo, CUInt(Marshal.SizeOf(shinfo)), SHGFI_ICON Or SHGFI_SMALLICON)
				Dim smallIcon As Icon
				Try
					smallIcon = Icon.FromHandle(shinfo.hIcon)
				Catch ex As ArgumentException
					Throw New ArgumentException(String.Format("File ""{0}"" doesn not exist!", FileName), ex)
				End Try
				_smallImageList.Images.Add(ext, smallIcon)

				SHGetFileInfo(FileName, 0, shinfo, CUInt(Marshal.SizeOf(shinfo)), SHGFI_ICON Or SHGFI_LARGEICON)
				Dim largeIcon As Icon = Icon.FromHandle(shinfo.hIcon)
				_largeImageList.Images.Add(ext, largeIcon)

				Return _smallImageList.Images.Count - 1
			End If
		End Function
		#End Region
	End Class
End Namespace
