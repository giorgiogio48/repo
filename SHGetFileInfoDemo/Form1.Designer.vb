Namespace SHGetFileInfoDemo
    Partial Public Class Form1
        Inherits Form

        ''' <summary>
        ''' Required designer variable.
        ''' </summary>
        Private components As System.ComponentModel.IContainer = Nothing

        ''' <summary>
        ''' Clean up any resources being used.
        ''' </summary>
        ''' <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        Protected Overrides Sub Dispose(ByVal disposing As Boolean)
            If disposing AndAlso (components IsNot Nothing) Then
                components.Dispose()
            End If
            MyBase.Dispose(disposing)
        End Sub

#Region "Windows Form Designer generated code"

        ''' <summary>
        ''' Required method for Designer support - do not modify
        ''' the contents of this method with the code editor.
        ''' </summary>
        Private Sub InitializeComponent()
            Me.components = New System.ComponentModel.Container()
            Me.listView1 = New System.Windows.Forms.ListView()
            Me.Button1 = New System.Windows.Forms.Button()
            Me.TextBox1 = New System.Windows.Forms.TextBox()
            Me.RadioButton1 = New System.Windows.Forms.RadioButton()
            Me.RadioButton2 = New System.Windows.Forms.RadioButton()
            Me.Button2 = New System.Windows.Forms.Button()
            Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
            Me.Label2 = New System.Windows.Forms.Label()
            Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
            Me.Label1 = New System.Windows.Forms.Label()
            Me.SuspendLayout()
            '
            'listView1
            '
            Me.listView1.BackColor = System.Drawing.Color.Honeydew
            Me.listView1.Font = New System.Drawing.Font("Microsoft Sans Serif", 15.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.listView1.HideSelection = False
            Me.listView1.Location = New System.Drawing.Point(12, 12)
            Me.listView1.Name = "listView1"
            Me.listView1.Size = New System.Drawing.Size(975, 539)
            Me.listView1.TabIndex = 3
            Me.listView1.UseCompatibleStateImageBehavior = False
            '
            'Button1
            '
            Me.Button1.Font = New System.Drawing.Font("Microsoft Sans Serif", 13.87156!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.Button1.Location = New System.Drawing.Point(1017, 74)
            Me.Button1.Name = "Button1"
            Me.Button1.Size = New System.Drawing.Size(161, 41)
            Me.Button1.TabIndex = 4
            Me.Button1.Text = "Search files"
            Me.Button1.UseVisualStyleBackColor = True
            '
            'TextBox1
            '
            Me.TextBox1.Font = New System.Drawing.Font("Microsoft Sans Serif", 15.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.TextBox1.Location = New System.Drawing.Point(1000, 12)
            Me.TextBox1.Name = "TextBox1"
            Me.TextBox1.Size = New System.Drawing.Size(279, 30)
            Me.TextBox1.TabIndex = 5
            '
            'RadioButton1
            '
            Me.RadioButton1.AutoSize = True
            Me.RadioButton1.Font = New System.Drawing.Font("Microsoft Sans Serif", 13.87156!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.RadioButton1.Location = New System.Drawing.Point(1017, 178)
            Me.RadioButton1.Name = "RadioButton1"
            Me.RadioButton1.Size = New System.Drawing.Size(102, 28)
            Me.RadioButton1.TabIndex = 6
            Me.RadioButton1.TabStop = True
            Me.RadioButton1.Text = "Directory"
            Me.RadioButton1.UseVisualStyleBackColor = True
            '
            'RadioButton2
            '
            Me.RadioButton2.AutoSize = True
            Me.RadioButton2.Font = New System.Drawing.Font("Microsoft Sans Serif", 13.87156!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.RadioButton2.Location = New System.Drawing.Point(1017, 213)
            Me.RadioButton2.Name = "RadioButton2"
            Me.RadioButton2.Size = New System.Drawing.Size(262, 28)
            Me.RadioButton2.TabIndex = 7
            Me.RadioButton2.TabStop = True
            Me.RadioButton2.Text = "Directory and subdirectories"
            Me.RadioButton2.UseVisualStyleBackColor = True
            '
            'Button2
            '
            Me.Button2.Font = New System.Drawing.Font("Microsoft Sans Serif", 13.87156!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.Button2.Location = New System.Drawing.Point(1017, 122)
            Me.Button2.Name = "Button2"
            Me.Button2.Size = New System.Drawing.Size(150, 31)
            Me.Button2.TabIndex = 8
            Me.Button2.Text = "Search text"
            Me.Button2.UseVisualStyleBackColor = True
            '
            'ImageList1
            '
            Me.ImageList1.ColorDepth = System.Windows.Forms.ColorDepth.Depth8Bit
            Me.ImageList1.ImageSize = New System.Drawing.Size(16, 16)
            Me.ImageList1.TransparentColor = System.Drawing.Color.Transparent
            '
            'Label2
            '
            Me.Label2.AutoSize = True
            Me.Label2.BackColor = System.Drawing.Color.Honeydew
            Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 21.79817!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.Label2.Location = New System.Drawing.Point(359, 266)
            Me.Label2.Name = "Label2"
            Me.Label2.Size = New System.Drawing.Size(174, 36)
            Me.Label2.TabIndex = 9
            Me.Label2.Text = "Searching..."
            '
            'Label1
            '
            Me.Label1.AutoSize = True
            Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 14.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
            Me.Label1.Location = New System.Drawing.Point(1015, 345)
            Me.Label1.Name = "Label1"
            Me.Label1.Size = New System.Drawing.Size(66, 24)
            Me.Label1.TabIndex = 11
            Me.Label1.Text = "Label1"
            '
            'Form1
            '
            Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
            Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
            Me.ClientSize = New System.Drawing.Size(1357, 588)
            Me.Controls.Add(Me.Label1)
            Me.Controls.Add(Me.Label2)
            Me.Controls.Add(Me.Button2)
            Me.Controls.Add(Me.RadioButton2)
            Me.Controls.Add(Me.RadioButton1)
            Me.Controls.Add(Me.TextBox1)
            Me.Controls.Add(Me.Button1)
            Me.Controls.Add(Me.listView1)
            Me.Name = "Form1"
            Me.Text = "SHGetFileInfo Demo"
            Me.ResumeLayout(False)
            Me.PerformLayout()

        End Sub

#End Region
        Private WithEvents listView1 As System.Windows.Forms.ListView
        Friend WithEvents Button1 As Button
        Friend WithEvents TextBox1 As TextBox
        Friend WithEvents RadioButton1 As RadioButton
        Friend WithEvents RadioButton2 As RadioButton
        Friend WithEvents Button2 As Button
        Friend WithEvents ImageList1 As ImageList
        Friend WithEvents Label2 As Label
        Friend WithEvents Timer1 As Timer
        Friend WithEvents Label1 As Label
    End Class
End Namespace

