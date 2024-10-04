object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 603
  ClientWidth = 885
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 23
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 33
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 47
    Width = 869
    Height = 548
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      
        'Database=C:\repositorio\ac-refactoring-live\db\SistemaFinanceiro' +
        '.db'
      'LockingMode=Normal'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 688
    Top = 16
  end
end
