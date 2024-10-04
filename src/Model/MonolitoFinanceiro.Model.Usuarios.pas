unit MonolitoFinanceiro.Model.Usuarios;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Datasnap.Provider,
  Datasnap.DBClient, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  MonolitoFinanceiro.Model.Conexao, MonolitoFinanceiro.Model.Entidades.Usuario,
  MonolitoFinanceiro.Model.resources.interfaces;

type
  TdmUsuarios = class(TDataModule)
    sqlUsuarios: TFDQuery;
    cdsUsuarios: TClientDataSet;
    dspUsuarios: TDataSetProvider;
    cdsUsuariosid: TStringField;
    cdsUsuariosnome: TStringField;
    cdsUsuarioslogin: TStringField;
    cdsUsuariossenha: TStringField;
    cdsUsuariosstatus: TStringField;
    cdsUsuariosdata_cadastro: TDateField;
    cdsUsuariossenha_temporaria: TStringField;
    cdsUsuariosadministrador: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FEntidadeUsuario : TModelEntidadeUsuario;
    FQuery: IQuery;
  public
    { Public declarations }
    const TEMP_PASSWORD = '123456';
    function TemLoginCadastrado(Login : string; ID : String) : Boolean;
    procedure EfetuarLogin(Login : String; Senha : String);
    function GetUsuarioLogado : TModelEntidadeUsuario;
    procedure LimparSenha(IDUsuario : String);
    procedure RedefinirSenha(Usuario : TModelEntidadeUsuario);
    procedure CadastrarUsuario(Usuario : TModelEntidadeUsuario);
    function TabelaUsuariosVazia : boolean;
  end;

var
  dmUsuarios: TdmUsuarios;

implementation
uses
  BCrypt, MonolitoFinanceiro.Utilitarios,
  MonolitoFinanceiro.Model.resources.query;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TdmUsuarios }

procedure TdmUsuarios.CadastrarUsuario(Usuario: TModelEntidadeUsuario);
begin
  FQuery.SQL('INSERT INTO USUARIOS ' +
      '(ID, NOME, LOGIN, SENHA, DATA_CADASTRO, SENHA_TEMPORARIA, STATUS, ADMINISTRADOR) ' +
      'VALUES (:ID, :NOME, :LOGIN, :SENHA, :DATA_CADASTRO, :SENHA_TEMPORARIA, :STATUS, :ADMINISTRADOR)')
  .Params('ID',TUtilitarios.GetID)
  .Params('NOME',Usuario.Nome)
  .Params('LOGIN',Usuario.Login)
  .Params('SENHA',TBCrypt.GenerateHash(Usuario.Senha))
  .Params('DATA_CADASTRO',Now)
  .Params('SENHA_TEMPORARIA','N')
  .Params('STATUS','A')
  .Params('ADMINISTRADOR','N')
  .ExecSQL;
end;

procedure TdmUsuarios.DataModuleCreate(Sender: TObject);
begin
  FEntidadeUsuario := TModelEntidadeUsuario.Create;
  FQuery := TQuery.New;
end;

procedure TdmUsuarios.DataModuleDestroy(Sender: TObject);
begin
  FEntidadeUsuario.Free;
end;

procedure TdmUsuarios.EfetuarLogin(Login, Senha: String);
begin
  var LConsulta := FQuery.SQL('SELECT * FROM USUARIOS WHERE LOGIN = :LOGIN')
    .Params('LOGIN', Login).ExecSQL;

  if (LConsulta.IsEmpty or (not TBCrypt.CompareHash(Senha, LConsulta.FieldByName('SENHA').AsString))) then
    raise Exception.Create('Usuário e/ou senha inválidos');

  if not LConsulta.FieldByName('STATUS').AsString.Equals('A') then
    raise Exception.Create('Usuário bloqueado, favor entrar em contato com o administrador');

  FEntidadeUsuario.ID := LConsulta.FieldByName('ID').AsString;
  FEntidadeUsuario.Nome := LConsulta.FieldByName('NOME').AsString;
  FEntidadeUsuario.Login := LConsulta.FieldByName('LOGIN').AsString;
  FEntidadeUsuario.Senha := LConsulta.FieldByName('SENHA').AsString;
  FEntidadeUsuario.SenhaTemporaria := LConsulta.FieldByName('SENHA_TEMPORARIA').AsString = 'S';
  FEntidadeUsuario.Administrador := LConsulta.FieldByName('ADMINISTRADOR').AsString = 'S';
end;

function TdmUsuarios.GetUsuarioLogado: TModelEntidadeUsuario;
begin
  Result := FEntidadeUsuario;
end;

procedure TdmUsuarios.LimparSenha(IDUsuario: String);
begin
  FQuery.SQL('UPDATE USUARIOS SET SENHA_TEMPORARIA = :SENHA_TEMPORARIA, SENHA = :SENHA WHERE ID = :ID')
  .Params('SENHA_TEMPORARIA','S')
  .Params('SENHA', TBCrypt.GenerateHash(TEMP_PASSWORD))
  .Params('ID', IDUsuario)
  .ExecSQL;
end;

procedure TdmUsuarios.RedefinirSenha(Usuario: TModelEntidadeUsuario);
begin
  FQuery.SQL('UPDATE USUARIOS SET SENHA_TEMPORARIA = :SENHA_TEMPORARIA, SENHA = :SENHA WHERE ID = :ID')
    .Params('SENHA_TEMPORARIA','N')
    .Params('SENHA', TBCrypt.GenerateHash(Usuario.Senha))
    .Params('ID', Usuario.ID)
    .ExecSQL;
end;

function TdmUsuarios.TabelaUsuariosVazia: boolean;
begin
  Result := FQuery.SQL('SELECT * FROM USUARIOS')
    .ExecSQL.IsEmpty;
end;

function TdmUsuarios.TemLoginCadastrado(Login, ID: String): Boolean;
begin
  var LConsulta := FQuery.SQL('SELECT ID FROM USUARIOS WHERE LOGIN = :LOGIN')
    .Params('LOGIN', Login).ExecSQL;

  Result := ((not LConsulta.IsEmpty) and (not LConsulta.FieldByName('ID').AsString.Equals(ID)));
end;

end.
