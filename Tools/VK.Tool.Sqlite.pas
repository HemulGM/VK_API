unit VK.Tool.Sqlite;

interface

uses
  System.Classes, FireDAC.Comp.Client, FireDAC.Stan.Param, REST.Json, VK.Entity.Common, VK.Entity.Group,
  VK.Entity.Profile, VK.Entity.Audio;

type
  TVkCacheDB = class;

  TVkCacheTable<T: TVkObject, constructor> = class
  private
    FDB: TVkCacheDB;
    FChecked: Boolean;
    FTableName: string;
    function GetSQLCreateTable(const TableName: string): string;
  public
    function Save(const Item: T): Boolean;
    function Find(const Id: Integer; var Item: T): Boolean; overload;
    function Find(const Id: Integer; var Item: T; var Date: TDateTime): Boolean; overload;
    function GetDate(const Id: Integer; var Date: TDateTime): Boolean;
    procedure CheckTable;
    constructor Create(DB: TVkCacheDB; const TableName: string);
  end;

  TVkCacheDB = class(TComponent)
  private
    FProfiles: TVkCacheTable<TVkProfile>;
    FGroups: TVkCacheTable<TVkGroup>;
    FAudios: TVkCacheTable<TVkAudio>;
  private
    FConnection: TFDConnection;
    procedure SetConnection(const Value: TFDConnection);
    function CreateQuery(const SQL: string): TFDQuery;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Profiles: TVkCacheTable<TVkProfile> read FProfiles;
    property Groups: TVkCacheTable<TVkGroup> read FGroups;
    property Audios: TVkCacheTable<TVkAudio> read FAudios;
  published
    property Connection: TFDConnection read FConnection write SetConnection;
  end;

implementation

uses
  System.SysUtils;

{ TVkCacheTable }

procedure TVkCacheTable<T>.CheckTable;
begin
  if FChecked then
    Exit;
  FDB.Connection.ExecSQL(GetSQLCreateTable(FTableName));
  FChecked := True;
end;

constructor TVkCacheTable<T>.Create(DB: TVkCacheDB; const TableName: string);
begin
  inherited Create;
  FChecked := False;
  FDB := DB;
  FTableName := TableName;
end;

function TVkCacheTable<T>.Find(const Id: Integer; var Item: T; var Date: TDateTime): Boolean;
begin
  Result := False;
  CheckTable;
  try
    with FDB.CreateQuery('SELECT data, date FROM ' + FTableName + ' WHERE id = :id') do
    begin
      ParamByName('id').Value := Id;
      try
        Open;
        if not Eof then
        begin
          Date := FieldByName('date').AsDateTime;
          Item := TJson.JsonToObject<T>(FieldByName('data').AsString);
          Result := True;
        end;
      finally
        Free;
      end;
    end;
  except
    Result := False;
  end;
end;

function TVkCacheTable<T>.Find(const Id: Integer; var Item: T): Boolean;
var
  Date: TDateTime;
begin
  Result := Find(Id, Item, Date);
end;

function TVkCacheTable<T>.GetDate(const Id: Integer; var Date: TDateTime): Boolean;
begin
  Result := False;
  CheckTable;
  try
    with FDB.CreateQuery('SELECT date FROM ' + FTableName + ' WHERE id = :id') do
    begin
      ParamByName('id').Value := Id;
      try
        Open;
        if not Eof then
        begin
          Date := FieldByName('date').AsDateTime;
          Result := True;
        end;
      finally
        Free;
      end;
    end;
  except
    Result := False;
  end;
end;

function TVkCacheTable<T>.GetSQLCreateTable(const TableName: string): string;
begin
  Result := 'CREATE TABLE IF NOT EXISTS ' + TableName + ' id INTEGER PRIMARY KEY, data TEXT, date DATETIME';
end;

function TVkCacheTable<T>.Save(const Item: T): Boolean;
begin
  Result := False;
  CheckTable;
  try
    with FDB.CreateQuery('INSERT OR REPLACE INTO ' + FTableName + ' VALUES (:id, :data, :date)') do
    try
      ParamByName('id').Value := Item.Id;
      ParamByName('data').Value := TJson.ObjectToJsonString(Item);
      ParamByName('date').Value := Now;
      ExecSQL;
      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

{ TVkCacheDB }

constructor TVkCacheDB.Create(AOwner: TComponent);
begin
  inherited;
  FProfiles := TVkCacheTable<TVkProfile>.Create(Self, 'vk_profiles');
  FGroups := TVkCacheTable<TVkGroup>.Create(Self, 'vk_groups');
  FAudios := TVkCacheTable<TVkAudio>.Create(Self, 'vk_audios');
end;

function TVkCacheDB.CreateQuery(const SQL: string): TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
  Result.SQL.Text := SQL;
end;

destructor TVkCacheDB.Destroy;
begin
  FProfiles.Free;
  FGroups.Free;
  FAudios.Free;
  inherited;
end;

procedure TVkCacheDB.SetConnection(const Value: TFDConnection);
begin
  FConnection := Value;
end;

end.

