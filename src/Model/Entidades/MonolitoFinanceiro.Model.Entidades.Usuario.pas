unit MonolitoFinanceiro.Model.Entidades.Usuario;

interface

uses MonolitoFinanceiro.EntityManager;

type
  TModelEntidadeUsuario = class
  private
    FLogin: string;
    FNome: string;
    FID: String;
    FSenha: String;
    FSenhaTemporaria: Boolean;
    FAdministrador: Boolean;
    FDataCadastro: TDate;
    FStatus: String;
    procedure SetID(const Value: String);
    procedure SetLogin(const Value: string);
    procedure SetNome(const Value: string);
    procedure SetSenha(const Value: String);
    procedure SetSenhaTemporaria(const Value: Boolean);
    procedure SetAdministrador(const Value: Boolean);
    procedure SetDataCadastro(const Value: TDate);
  public
    property Nome : string read FNome write SetNome;
    property Login : string read FLogin write SetLogin;
    property ID : String read FID write SetID;
    property Senha : String read FSenha write SetSenha;
    [TColumnAttribute('SENHA_TEMPORARIA')]
    property SenhaTemporaria : Boolean read FSenhaTemporaria write SetSenhaTemporaria;
    property Administrador : Boolean read FAdministrador write SetAdministrador;
    [TColumnAttribute('DATA_CADASTRO')]
    property DataCadastro: TDate read FDataCadastro write SetDataCadastro;
    property Status: String read FStatus write FStatus;
  end;

implementation

uses
  System.SysUtils;

procedure TModelEntidadeUsuario.SetAdministrador(const Value: Boolean);
begin
  FAdministrador := Value;
end;

procedure TModelEntidadeUsuario.SetDataCadastro(const Value: TDate);
begin
  if DateToStr(Value).IsEmpty then
  begin
    FDataCadastro := now;
    Exit;
  end;
    FDataCadastro := Value;
end;

procedure TModelEntidadeUsuario.SetID(const Value: String);
begin
  FID := Value;
end;

procedure TModelEntidadeUsuario.SetLogin(const Value: string);
begin
  FLogin := Value;
end;

procedure TModelEntidadeUsuario.SetNome(const Value: string);
begin
  FNome := Value;
end;

procedure TModelEntidadeUsuario.SetSenha(const Value: String);
begin
  FSenha := Value;
end;

procedure TModelEntidadeUsuario.SetSenhaTemporaria(const Value: Boolean);
begin
  FSenhaTemporaria := Value;
end;

end.
