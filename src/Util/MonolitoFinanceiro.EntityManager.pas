unit MonolitoFinanceiro.EntityManager;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.SysUtils,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.Param,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Comp.Client,
  FireDAC.DApt;

type
  TColumnAttribute = class(TCustomAttribute)
  private
    FColumnName: string;
    FIsPrimaryKey: Boolean;
  public
    constructor Create(const ColumnName: string; IsPrimaryKey: Boolean = False);
    property ColumnName: string read FColumnName;
    property PrimaryKey: Boolean read FIsPrimaryKey;
  end;

  TDatabaseType = (dtUnknown, dtSQLite, dtMySQL, dtMSSQL, dtPostgreSQL);

  IEntityFactory<T: class> = interface
    function CreateEntity: T;
  end;

  TEntityDatasetMapper<T: class, constructor> = class
  private
    FConnection: TFDConnection;
    FDatabaseType: TDatabaseType;
    FFactory: IEntityFactory<T>;

    function GetDelimiter: string;
    function FormatValueForSQL(const Value: TValue): string;
    function GetColumnName(const Prop: TRttiProperty): string;
    function IsPrimaryKey(const Prop: TRttiProperty): Boolean;
    procedure DatasetToEntity(Dataset: TDataSet; Entity: T);
    function GenerateWhere(Entity: T; const WhereFields: array of string): string;

    class function FirstOrDefault(const List: TObjectList<T>): T;
  public
    constructor Create(Connection: TFDConnection; DatabaseType: TDatabaseType; Factory: IEntityFactory<T>);

    function Insert(Entity: T; const TableName: string): T;
    function Update(Entity: T; const TableName: string; const WhereFields: array of string): T;
    function Delete(Entity: T; const TableName: string; const WhereFields: array of string): Boolean;
    function Select(const TableName: string; const Filter: string = ''): TObjectList<T>;
    function SelectSingle(const TableName: string; const FilterFields: array of string; const FilterValues: array of TValue): TObjectList<T>; overload;
    function SelectSingle(const TableName: string; const Entity: T): TObjectList<T>; overload;
    function SelectOneRow(const TableName: String; const Entity: T): T; overload;
    function SelectOneRow(const TableName: string; const FilterFields: array of string; const FilterValues: array of TValue): T; overload;
  end;

implementation

uses
  System.Classes,
  System.StrUtils;

constructor TColumnAttribute.Create(const ColumnName: string; IsPrimaryKey: Boolean);
begin
  FColumnName := ColumnName;
  FIsPrimaryKey := IsPrimaryKey;
end;

constructor TEntityDatasetMapper<T>.Create(Connection: TFDConnection; DatabaseType: TDatabaseType; Factory: IEntityFactory<T>);
begin
  FConnection := Connection;
  FDatabaseType := DatabaseType;
  FFactory := Factory;
end;

function TEntityDatasetMapper<T>.GetDelimiter: string;
begin
  case FDatabaseType of
    dtSQLite, dtMySQL, dtPostgreSQL:
      Result := '"';
    dtMSSQL:
      Result := '[';
  else
    Result := '';
  end;
end;

class function TEntityDatasetMapper<T>.FirstOrDefault(
  const List: TObjectList<T>): T;
begin
  if Assigned(List) and (List.Count > 0) then
    Result := List.First
  else
    Result := Default(T);
end;

function TEntityDatasetMapper<T>.FormatValueForSQL(const Value: TValue): string;
begin
  case Value.Kind of
    tkInteger, tkInt64:
      Result := Value.ToString;
    tkFloat:
      Result := FloatToStr(Value.AsExtended);
    tkString, tkLString, tkWString, tkUString:
      Result := QuotedStr(Value.AsString);
    tkEnumeration:
      if Value.TypeInfo = TypeInfo(Boolean) then
        Result := IfThen(Value.AsBoolean, '1', '0')
      else
        Result := Value.ToString;
    tkDynArray:
      if Value.TypeInfo = TypeInfo(TBytes) then
        Result := QuotedStr(StringOf(Value.AsType<TBytes>));
  else
    Result := '';
  end;
end;

function TEntityDatasetMapper<T>.GenerateWhere(Entity: T; const WhereFields: array of string): string;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Delimiter: string;
begin
  Delimiter := GetDelimiter;
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(TClass(Entity.ClassType));
    Result := '';
    for Prop in RttiType.GetProperties do
    begin
      if Prop.IsReadable then
      begin
        if MatchStr(GetColumnName(Prop), WhereFields) then
        begin
          if not Result.IsEmpty then
            Result := Result + ' AND ';
          Result := Result + Format('%s%s%s = :%s', [Delimiter, GetColumnName(Prop), Delimiter, GetColumnName(Prop)]);
        end;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

function TEntityDatasetMapper<T>.GetColumnName(const Prop: TRttiProperty): string;
var
  Attr: TCustomAttribute;
begin
  for Attr in Prop.GetAttributes do
    if Attr is TColumnAttribute  then
      Exit(TColumnAttribute(Attr).ColumnName);
  Result := Prop.Name;
end;

function TEntityDatasetMapper<T>.IsPrimaryKey(const Prop: TRttiProperty): Boolean;
var
  Attr: TCustomAttribute;
begin
  Attr := Prop.GetAttribute<TColumnAttribute>;
  if Assigned(Attr) then
    Result := TColumnAttribute(Attr).PrimaryKey
  else
    Result := False;
end;

function TEntityDatasetMapper<T>.Insert(Entity: T; const TableName: string): T;
var
  Query: TFDQuery;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  ColumnName, Delimiter, Columns, Values: string;
  WhereFields: TArray<string>;
begin
  Delimiter := GetDelimiter;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(TClass(Entity.ClassType));
      Columns := '';
      Values := '';
      for Prop in RttiType.GetProperties do
      begin
        if Prop.IsReadable then
        begin
          if IsPrimaryKey(Prop) then
            continue;
          ColumnName := GetColumnName(Prop);

          WhereFields := WhereFields + [ColumnName];

          if not Columns.IsEmpty then
            Columns := Columns + ', ';
          Columns := Columns + Delimiter + ColumnName + Delimiter;

          if not Values.IsEmpty then
            Values := Values + ', ';
          Values := Values + ':' + ColumnName;
        end;
      end;

      Query.SQL.Text := Format('INSERT INTO %s (%s) VALUES (%s)',
        [TableName, Columns, Values]);

      for Prop in RttiType.GetProperties do
      begin
        if Prop.IsReadable then
        begin
          if IsPrimaryKey(Prop) then
            continue;
          ColumnName := GetColumnName(Prop);
          Query.ParamByName(ColumnName).Value := Prop.GetValue(Pointer(Entity)).AsVariant;
        end;
      end;

      Query.ExecSQL;

      // Após a inserção, recuperamos a entidade completa usando SelectSingle
      Result := FirstOrDefault(SelectSingle(TableName, Entity));
    finally
      RttiContext.Free;
    end;
  finally
    Query.Free;
  end;
end;


function TEntityDatasetMapper<T>.Update(Entity: T; const TableName: string; const WhereFields: array of string): T;
var
  Query: TFDQuery;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  ColumnName, Delimiter: string;
  SetClauses, WhereClauses: TArray<string>;
  Value: TValue;
begin
  Delimiter := GetDelimiter;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(TClass(Entity.ClassType));

      for Prop in RttiType.GetProperties do
      begin
        if Prop.IsReadable then
        begin
          ColumnName := GetColumnName(Prop);
          Value := Prop.GetValue(Pointer(Entity));
          if not MatchStr(ColumnName, WhereFields) then
            SetClauses := SetClauses + [Format('%s%s%s = %s', [Delimiter, ColumnName, Delimiter, FormatValueForSQL(Value)])]
          else
            WhereClauses := WhereClauses + [Format('%s%s%s = %s', [Delimiter, ColumnName, Delimiter, FormatValueForSQL(Value)])];
        end;
      end;

      Query.SQL.Text := Format('UPDATE %s SET %s WHERE %s',
        [TableName, string.Join(', ', SetClauses), string.Join(' AND ', WhereClauses)]);
      Query.ExecSQL;

      Result := Entity;
    finally
      RttiContext.Free;
    end;
  finally
    Query.Free;
  end;
end;

procedure TEntityDatasetMapper<T>.DatasetToEntity(Dataset: TDataSet; Entity: T);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Field: TField;
  ColumnName: string;
  Value: TValue;
begin
  if not Assigned(Dataset) or not Assigned(Entity) then
    Exit;

  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(TClass(Entity.ClassType));
    for Prop in RttiType.GetProperties do
    begin
      if Prop.IsWritable then
      begin
        ColumnName := GetColumnName(Prop);
        Field := Dataset.FindField(ColumnName);
        if Assigned(Field) then
        begin
          case Prop.PropertyType.TypeKind of
            tkInteger, tkInt64:
              Prop.SetValue(Pointer(Entity), Field.AsInteger);
            tkFloat:
              if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
                Prop.SetValue(Pointer(Entity), Field.AsDateTime)
              else
                Prop.SetValue(Pointer(Entity), Field.AsFloat);
            tkString, tkLString, tkWString, tkUString:
              Prop.SetValue(Pointer(Entity), Field.AsString);
            tkEnumeration:
              if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                Prop.SetValue(Pointer(Entity), Field.AsBoolean)
              else
                Prop.SetValue(Pointer(Entity), Field.AsInteger);
            tkDynArray:
              if Prop.PropertyType.Handle = TypeInfo(TBytes) then
                Prop.SetValue(Pointer(Entity), TValue.From<TBytes>(Field.AsBytes));
          end;
        end;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

function TEntityDatasetMapper<T>.Delete(Entity: T; const TableName: string; const WhereFields: array of string): Boolean;
var
  Query: TFDQuery;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  ColumnName, Delimiter: string;
  WhereClauses: TArray<string>;
  Value: TValue;
begin
  Delimiter := GetDelimiter;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(TClass(Entity.ClassType));
      for Prop in RttiType.GetProperties do
      begin
        if Prop.IsReadable then
        begin
          ColumnName := GetColumnName(Prop);
          Value := Prop.GetValue(Pointer(Entity));
          if MatchStr(ColumnName, WhereFields) then
            WhereClauses := WhereClauses + [Format('%s%s%s = %s', [Delimiter, ColumnName, Delimiter, FormatValueForSQL(Value)])];
        end;
      end;

      Query.SQL.Text := Format('DELETE FROM %s WHERE %s',
        [TableName, string.Join(' AND ', WhereClauses)]);
      Query.ExecSQL;

      Result := True;
    finally
      RttiContext.Free;
    end;
  finally
    Query.Free;
  end;
end;

function TEntityDatasetMapper<T>.Select(const TableName: string; const Filter: string = ''): TObjectList<T>;
var
  Query: TFDQuery;
  Entity: T;
begin
  Result := TObjectList<T>.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM ' + TableName;
    if Filter <> '' then
      Query.SQL.Text := Query.SQL.Text + ' WHERE ' + Filter;
    Query.Open;

    Query.First;
    while not Query.Eof do
    begin
      Entity := FFactory.CreateEntity;
      DatasetToEntity(Query, Entity);
      Result.Add(Entity);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TEntityDatasetMapper<T>.SelectOneRow(const TableName: string;
  const FilterFields: array of string; const FilterValues: array of TValue): T;
begin
  Result := FirstOrDefault(SelectSingle(TableName, FilterFields, FilterValues));
end;

function TEntityDatasetMapper<T>.SelectOneRow(const TableName: String;
  const Entity: T): T;
begin
  Result := FirstOrDefault(SelectSingle(TableName, Entity));
end;

function TEntityDatasetMapper<T>.SelectSingle(const TableName: string; const Entity: T): TObjectList<T>;
var
  Query: TFDQuery;
  FilterFields: TArray<string>;
  FilterValues: TArray<TValue>;
  Prop: TRttiProperty;
  ColumnName: string;
  I: Integer;
begin
  SetLength(FilterFields, 0);
  SetLength(FilterValues, 0);

  // Preencher os campos e valores para filtrar com base nas propriedades preenchidas da entidade
  for Prop in TRttiContext.Create.GetType(TClass(Entity.ClassType)).GetProperties do
  begin
    if Prop.IsReadable then
    begin
      if IsPrimaryKey(Prop) then
        Continue;
      ColumnName := GetColumnName(Prop);
      if not Prop.GetValue(Pointer(Entity)).IsEmpty then
      begin
        SetLength(FilterFields, Length(FilterFields) + 1);
        SetLength(FilterValues, Length(FilterValues) + 1);
        FilterFields[High(FilterFields)] := ColumnName;
        FilterValues[High(FilterValues)] := Prop.GetValue(Pointer(Entity));
      end;
    end;
  end;

  // Realizar a consulta com base nos campos e valores preenchidos
  Result := SelectSingle(TableName, FilterFields, FilterValues);
end;


function TEntityDatasetMapper<T>.SelectSingle(const TableName: string; const FilterFields: array of string; const FilterValues: array of TValue): TObjectList<T>;
var
  Query: TFDQuery;
  Entity: T;
  Filter: string;
  I: Integer;
begin
  if Length(FilterFields) <> Length(FilterValues) then
    raise Exception.Create('Filter fields and values must have the same length.');

  Result := TObjectList<T>.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Filter := '';
    for I := 0 to High(FilterFields) do
    begin
      if I > 0 then
        Filter := Filter + ' AND ';
      Filter := Filter + Format('%s LIKE %s', [FilterFields[I], FormatValueForSQL(FilterValues[I])]);
    end;

    Query.SQL.Text := 'SELECT * FROM ' + TableName + ' WHERE ' + Filter;
    Query.Open;

    Query.First;
    while not Query.Eof do
    begin
      Entity := FFactory.CreateEntity;
      DatasetToEntity(Query, Entity);
      Result.Add(Entity);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

end.

