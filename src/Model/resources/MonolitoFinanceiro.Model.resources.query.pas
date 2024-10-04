unit MonolitoFinanceiro.Model.resources.query;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  Datasnap.Provider,
  Datasnap.DBClient,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.Client,
  MonolitoFinanceiro.Model.resources.interfaces;

type
  TQuery = class(TInterfacedObject, IQuery)
  private
    FQuery: TFDQuery;
    FConn: IConexao;
    FSQL: String;

    constructor Create;
    destructor Destroy; override;
  public
    class function New: IQuery;

    function SQL(Value: String): IQuery;
    function Params(Key: String; Value: Variant): IQuery;
    function ExecSQL: TDataSet;
  end;

implementation

uses
  MonolitoFinanceiro.Model.resources.conexao;

{ TQuery }

constructor TQuery.Create;
begin
  FQuery := TFDQuery.Create(nil);
  FConn := TConexao.NEw;
  FQuery.Connection := TFDConnection(FConn.Conexao);
end;

destructor TQuery.Destroy;
begin
  FQuery.free;
  inherited;
end;

function TQuery.ExecSQL: TDataSet;
begin
  if not (AnsiPos('SELECT', UpperCase(FSQL)) > 0) then
  begin
    FConn.StartTransaction;
    try
      FQuery.Prepare;
      FQuery.ExecSQL;
      FConn.Commit;
      Exit;
    except on E: Exception do
      begin
        FConn.Rollback;
        raise Exception.Create('Erro ao realizar a persistencia dos dados');
      end;
    end;
  end;

  FQuery.Open;

  Result := FQuery;
end;

class function TQuery.New: IQuery;
begin
  Result := Self.Create;
end;

function TQuery.Params(Key: String; Value: Variant): IQuery;
begin
  REsult := Self;
  FQuery.Params.Add;
  FQuery.Params.ParamByName(Key).Value := Value;
end;

function TQuery.SQL(Value: String): IQuery;
begin
  Result := SElf;
  FSQL := EmptyStr;
  FSQL := Value;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(FSQL);
end;

end.
