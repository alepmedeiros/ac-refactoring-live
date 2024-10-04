unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls, MonolitoFinanceiro.EntityManager, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    FDConnection1: TFDConnection;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCliente = class
  private
    FId: Integer;
    FNome: String;
    FEmail: String;
  public
    [TColumnAttribute('ID', True)]
    property Id: Integer read FId write FId;
    [TColumnAttribute('NOME_PRINCIPAL')]
    property Nome: String read FNome write FNome;
    property Email: String read FEmail write FEmail;
  end;

  TClienteFactory = class(TInterfacedObject, IEntityFactory<TCliente>)
  public
    function CreateEntity: TCliente;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  var LFactory := TClienteFactory.Create;
  var LEntityManager := TEntityDatasetMapper<TCliente>.Create(FDConnection1, dtSQLite, LFactory);
  var LId: Integer;

  Memo1.Clear;
  Memo1.Lines.Add('Inserindo Cliente');
  var LClienteInsert := TCliente.Create;
  try
    LClienteInsert.Nome := 'Alessandro';
    LClienteInsert.Email := 'email@email.com';
    LClienteInsert := LEntityManager.Insert(LClienteInsert,'Clientes');
    Memo1.Lines.Add('Id do Cliente Inserido: ' + LClienteInsert.Id.ToString);
    var LCliente := TCliente.Create;
    try
      LCliente.Nome := 'Joao';
      LCliente.Email := 'joao@email.com';
      LCliente := LEntityManager.Insert(LCliente,'Clientes');
      LID := LCliente.Id;
      Memo1.Lines.Add('Id do Cliente Inserido: ' + LCliente.Id.ToString);
    finally
      LCliente.Free;
    end;
  finally
    LClienteInsert.Free;
  end;

  Memo1.Lines.Add('Atualizando Cliente');
  var LClienteUpdate := LEntityManager.SelectOneRow('Clientes', ['NOME_PRINCIPAL'],['Alessandro']);;
  try
    LClienteUpdate.Nome := 'Alessandro Medeiros';
    LClienteUpdate := LEntityManager.Update(LClienteUpdate,'Clientes', ['ID']);
    Memo1.Lines.Add('Dados do Cliente Atualizado: ');
    Memo1.Lines.Add('    Id: '+LClienteUpdate.Id.ToString);
    Memo1.Lines.Add('    Nome: '+LClienteUpdate.Nome);
    Memo1.Lines.Add('    Email: '+LClienteUpdate.Email);
  finally
    LClienteUpdate.Free;
  end;

  Memo1.Lines.Add('Todos os Clientes');
  var LClientes := LEntityManager.Select('Clientes');
  try
    for var Cliente in LClientes do
    begin
      Memo1.Lines.Add('Id: '+Cliente.Id.ToString);
      Memo1.Lines.Add('Nome: '+Cliente.Nome);
      Memo1.Lines.Add('Email: '+Cliente.Email);
      Memo1.Lines.Add(EmptyStr);
    end;
  finally
    LClientes.Free;
  end;

  Memo1.Lines.Add('Todos os Clientes por email');
  var LClientesFiltro := LEntityManager.SelectSingle('Clientes',['EMAIL'],['%email.com']);
  try
    for var Cliente in LClientesFiltro do
    begin
      Memo1.Lines.Add('Id: '+Cliente.Id.ToString);
      Memo1.Lines.Add('Nome: '+Cliente.Nome);
      Memo1.Lines.Add('Email: '+Cliente.Email);
      Memo1.Lines.Add(EmptyStr);
    end;
  finally
    LClientesFiltro.Free;
  end;

  Memo1.Lines.Add('Todos os Clientes por Entidade');
  var LFiltroCliente := TCliente.Create;
  LFiltroCliente.FNome := 'Alessandro';
  var LFiltros := LEntityManager.SelectSingle('Clientes', LFiltroCliente);
  try
    for var Cliente in LFiltros do
    begin
      Memo1.Lines.Add('Id: '+Cliente.Id.ToString);
      Memo1.Lines.Add('Nome: '+Cliente.Nome);
      Memo1.Lines.Add('Email: '+Cliente.Email);
      Memo1.Lines.Add(EmptyStr);
    end;
  finally
    LFiltros.Free;
    LFiltroCliente.Free;
  end;
  Memo1.Lines.Add('Cliente:');
  var LCliente := LEntityManager.SelectOneRow('Clientes', ['NOME_PRINCIPAL'],['Alessandro Medeiros']);
  try
    Memo1.Lines.Add('Id: '+LCliente.Id.ToString);
    Memo1.Lines.Add('Nome: '+LCliente.Nome);
    Memo1.Lines.Add('Email: '+LCliente.Email);
  finally
    LCliente.Free;
  end;

  var lExcluir := LEntityManager.SelectOneRow('Clientes',['ID'],[LId]);
  Memo1.Lines.Add('Excluindo Cliente');
  Memo1.Lines.Add(Format('Cliente do ID %d excluido',[LId]));
  LEntityManager.Delete(lExcluir,'Clientes', ['ID']);
end;

{ TClienteFactory }

function TClienteFactory.CreateEntity: TCliente;
begin
  Result := TCliente.Create;
end;

end.
