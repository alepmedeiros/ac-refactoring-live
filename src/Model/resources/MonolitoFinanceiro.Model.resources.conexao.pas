unit MonolitoFinanceiro.Model.resources.conexao;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.VCLUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.DatS,
  FireDAC.DApt.Intf,
  FireDAC.DApt,
  FireDAC.Comp.DataSet,
  MonolitoFinanceiro.Model.resources.interfaces;

type
  TConexao = class(TInterfacedObject, IConexao)
  private
    FConexao: TFDConnection;
    FConf: IConfiguracao;

    procedure CarregarConfiguracoes;

    constructor Create;
    destructor Destroy; override;
  public
    class function New: IConexao;

    function Conexao: TCustomConnection;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  end;

implementation

uses
  MonolitoFinanceiro.Model.resources.configuracao;

{ TConexao }

procedure TConexao.CarregarConfiguracoes;
begin
  FConexao.Params.Clear;
  FConexao.Params.DriverID := FConf.DriverID;
  FConexao.Params.Database := FConf.Database;
  FConexao.Params.Add('LockingMode=' + FConf.LockingMode);
  FConexao.Connected;
end;

procedure TConexao.Commit;
begin
  FConexao.Commit;
end;

function TConexao.Conexao: TCustomConnection;
begin
  Result := FConexao;
end;

constructor TConexao.Create;
begin
  FConexao := TFDConnection.Create(nil);
  FConf := TConfiguracao.NEw;
  CarregarConfiguracoes;
end;

destructor TConexao.Destroy;
begin
  FConexao.Free;
  inherited;
end;

class function TConexao.New: IConexao;
begin
  Result := Self.Create;
end;

procedure TConexao.Rollback;
begin
  FConexao.Rollback;
end;

procedure TConexao.StartTransaction;
begin
  FConexao.StartTransaction;
end;

end.
