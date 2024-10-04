unit MonolitoFinanceiro.Model.resources.configuracao;

interface

uses
  MonolitoFinanceiro.Model.resources.interfaces;

type
  TConfiguracao = class(TInterfacedObject, IConfiguracao)
  private
    const ARQUIVOCONFIGURACAO = 'MonolitoFinanceiro.cfg';

    function GetConfiguracao(Secao, Parametro, valorPadrao : String) : String;
    procedure SetConfiguracao(Secao, Parametro, Valor : String);
  public
    class function New: IConfiguracao;

    function DataUltimoAcesso : String; overload;
    function DataUltimoAcesso(aValue : TDateTime): IConfiguracao; overload;
    function UsuarioUltimoAcesso : String; overload;
    function UsuarioUltimoAcesso(aValue : String): IConfiguracao; overload;
    function DriverID : String; overload;
    function DriverID(aValue : String): IConfiguracao; overload;
    function Database : String; overload;
    function Database(aValue : String): IConfiguracao; overload;
    function LockingMode : String; overload;
    function LockingMode(aValue : String): IConfiguracao; overload;
  end;

implementation

uses
  System.sysutils,
  System.inifiles, Vcl.Forms;

function TConfiguracao.Database(aValue: String): IConfiguracao;
begin
  Result := Self;
  SetConfiguracao('Banco de dados', 'Database', aValue);
end;

function TConfiguracao.Database: String;
begin
  Result := GetConfiguracao('Banco de dados', 'Database', 'db\SistemaFinanceiro.db');
end;

function TConfiguracao.DataUltimoAcesso(aValue: TDateTime): IConfiguracao;
begin
  SetConfiguracao('ACESSO', 'Data', DateTimeToStr(aValue));
end;

function TConfiguracao.DataUltimoAcesso: String;
begin
  Result := GetConfiguracao('ACESSO', 'Data', '');
end;

function TConfiguracao.DriverID: String;
begin
  Result := GetConfiguracao('Banco de dados', 'DriverID', 'SQLite');
end;

function TConfiguracao.DriverID(aValue: String): IConfiguracao;
begin
  SetConfiguracao('Banco de dados', 'DriverID', aValue);
end;

function TConfiguracao.GetConfiguracao(Secao, Parametro,
  valorPadrao: String): String;
var
  LArquivoConfig : TIniFile;
begin
  LArquivoConfig := TIniFile.Create(ExtractFilePath(Application.ExeName) + ARQUIVOCONFIGURACAO);
  try
    Result := LArquivoConfig.ReadString(Secao, Parametro, ValorPadrao);
  finally
    LArquivoConfig.Free;
  end;
end;

function TConfiguracao.LockingMode: String;
begin
  Result := GetConfiguracao('Banco de dados', 'LockingMode', 'Normal');
end;

function TConfiguracao.LockingMode(aValue: String): IConfiguracao;
begin
  SetConfiguracao('Banco de dados', 'LockingMode', aValue);
end;

class function TConfiguracao.New: IConfiguracao;
begin
  Result := Self.create;
end;

procedure TConfiguracao.SetConfiguracao(Secao, Parametro, Valor: String);
var
  LArquivoConfig : TIniFile;
begin
  LArquivoConfig := TIniFile.Create(ExtractFilePath(Application.ExeName) + ARQUIVOCONFIGURACAO);
  try
    LArquivoConfig.WriteString(Secao, Parametro, Valor);
  finally
    LArquivoConfig.Free;
  end;
end;

function TConfiguracao.UsuarioUltimoAcesso: String;
begin
  Result := GetConfiguracao('ACESSO', 'Usuario', '');
end;

function TConfiguracao.UsuarioUltimoAcesso(aValue: String): IConfiguracao;
begin
  SetConfiguracao('ACESSO', 'Usuario', aValue);
end;

end.
