unit MonolitoFinanceiro.Model.dao.base;

interface

uses
  System.Contnrs,
  MonolitoFinanceiro.Model.resources.interfaces,
  System.Rtti;

type
  TDAOBase = class
  protected
    FConn: IConexao;
  public
    constructor Create; virtual;

    function Update(Value: TObject): TObject; virtual; abstract;
    function Insert(Value: TObject): TObject; virtual; abstract;
    function FindAll: TObjectList; virtual; abstract;
    function FindById(aId: Variant): TObject; virtual; abstract;
    function FindWhere(aKey: String; aValue: Variant): TObjectList; overload; virtual; abstract;
  end;

implementation

uses
  MonolitoFinanceiro.Model.resources.conexao;

constructor TDAOBase.Create;
begin
  FConn := TConexao.New;
end;

end.
