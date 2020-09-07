unit VK.Entity.Group.TokenPermissions;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkTokenPermission = class
  private
    FName: string;
    FSetting: Integer;
  public
    property Name: string read FName write FName;
    property Setting: Integer read FSetting write FSetting;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkTokenPermission;
  end;

  TVkTokenPermissions = class
  private
    FMask: Integer;
    FSettings: TArray<TVkTokenPermission>;
  public
    property Mask: Integer read FMask write FMask;
    property Settings: TArray<TVkTokenPermission> read FSettings write FSettings;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkTokenPermissions;
  end;

implementation

uses
  VK.CommonUtils;

{TVkTokenPermission}

function TVkTokenPermission.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkTokenPermission.FromJsonString(AJsonString: string): TVkTokenPermission;
begin
  result := TJson.JsonToObject<TVkTokenPermission>(AJsonString)
end;

{TVkTokenPermissions}

destructor TVkTokenPermissions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkTokenPermission>(FSettings);
  inherited;
end;

function TVkTokenPermissions.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkTokenPermissions.FromJsonString(AJsonString: string): TVkTokenPermissions;
begin
  result := TJson.JsonToObject<TVkTokenPermissions>(AJsonString)
end;

end.

