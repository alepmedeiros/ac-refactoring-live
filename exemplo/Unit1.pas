unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  MonolitoFinanceiro.EntityManager;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TUsuario = class
  private
    FId: String;
    FNome: String;
    FNascimento: String;
  public
    property Id: String read FId write FId;
    property Nome: String read FNome write FNome;
    property Nascimento: String read FNascimento write FNascimento;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  var LOldusuario := TUsuario.Create;
  LOldusuario.Id := 'ABC123';
  LOldusuario.Nome := 'Alessandro';
  LOldusuario.Nascimento := FormatDateTime('dd/mm/yyyy', now);

  var LNewdusuario := TUsuario.Create;
  LNewdusuario.Id := 'ABC123';
  LNewdusuario.Nome := 'Alessandro Medeiros';
  LNewdusuario.Nascimento := '01/06/1992';

  Memo1.Lines.Add('INSERT');
  Memo1.Lines.Add(TEntityManager.GenerateInsert(LNewdusuario, 'usuarios'));
  Memo1.Lines.Add(EmptyStr);

  Memo1.Lines.Add('UPDATE');
  Memo1.Lines.Add(TEntityManager.GenerateUpdate(LOldusuario, LNewdusuario, 'usuarios', ['id']));
  Memo1.Lines.Add(EmptyStr);

  Memo1.Lines.Add('SELECTALL');
  Memo1.Lines.Add(TEntityManager.GenerateSelectAll('usuarios'));
  Memo1.Lines.Add(EmptyStr);

  Memo1.Lines.Add('SELECTWHERE');
  Memo1.Lines.Add(TEntityManager.GenerateSelectWhere('usuarios',LNewdusuario,['Nascimento']));
  Memo1.Lines.Add(EmptyStr);
end;

end.
