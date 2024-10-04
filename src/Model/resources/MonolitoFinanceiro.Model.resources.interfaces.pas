unit MonolitoFinanceiro.Model.resources.interfaces;

interface

uses
  Data.DB;

type
  IConexao = interface
    ['{5EDAA742-366F-446A-9DA7-1348D9ACC6EA}']
    function Conexao: TCustomConnection;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
  end;

  IConfiguracao = interface
    ['{1F11CCF9-201B-4AF1-9960-50B517F83919}']
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

  IQuery = interface
    ['{51D7E658-9774-4500-80C2-19C471446676}']
    function SQL(Value: String): IQuery;
    function Params(Key: String; Value: Variant): IQuery;
    function ExecSQL: TDataSet;
  end;

implementation

end.
