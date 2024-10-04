unit MonolitoFinanceiro.Model.dao.usuario;

interface

uses
  System.Contnrs,
  System.Generics.Collections,
  Data.DB,
  MonolitoFinanceiro.Model.dao.base,
  MonolitoFinanceiro.Model.Entidades.Usuario,
  MonolitoFinanceiro.EntityManager, System.Rtti,
  MonolitoFinanceiro.Model.Entidades.Factory;

type
  TDAOUsuario = class(TDAOBase)
  private
    FMapper: TEntityDatasetMapper<TModelEntidadeUsuario>;
  public
    constructor Create; override;

    function Update(Value: TObject): TObject; override;
    function Insert(Value: TObject): TObject; override;
    function FindAll: TObjectList; override;
    function FindById(aId: Variant): TObject; override;
    function FindWhere(aKey: String; aValue: Variant): TObjectList; override;

    function FindOneWhere(aKey: String; aValue: Variant): TObject;
  end;

implementation

uses
  System.SysUtils,
  System.Math,
  System.StrUtils,
  System.TypInfo,
  BCrypt,
  System.Variants,
  MonolitoFinanceiro.Utilitarios,
  FireDAC.Comp.Client;

constructor TDAOUsuario.Create;
begin
  inherited;
  FMapper := TEntityDatasetMapper<TModelEntidadeUsuario>.Create(TFDConnection(FConn.Conexao),dtSQLite,TUsuarioFactory.Create);
end;

function TDAOUsuario.FindAll: TObjectList;
begin
  var LLista := FMapper.Select('USUARIOS');

  Result := TObjectList.Create;
  for var I in LLista do
    Result.Add(I);
end;

function TDAOUsuario.FindById(aId: Variant): TObject;
begin
  REsult := FMapper.SelectOneRow('USUARIOS',['ID'],[VarToStr(aId)]);
end;

function TDAOUsuario.FindOneWhere(aKey: String; aValue: Variant): TObject;
begin
  Result := FMapper.SelectOneRow('USUARIOS',[aKey], [TValue.FromVariant(aValue)]);
end;

function TDAOUsuario.FindWhere(aKey: String; aValue: Variant): TObjectList;
begin
  var LLista := FMapper.SelectSingle('USUARIOS',[aKey], [TValue.FromVariant(aValue)]);

  Result := TObjectList.Create;
  for var I in LLista do
    Result.Add(I);
end;

function TDAOUsuario.Insert(Value: TObject): TObject;
begin
  Result := FMapper.Insert(TModelEntidadeUsuario(Value), 'USUARIOS');
end;

function TDAOUsuario.Update(Value: TObject): TObject;
begin
  Result := FMapper.Update(TModelEntidadeUsuario(Value),'USUARIOS', ['ID']);
end;

end.
