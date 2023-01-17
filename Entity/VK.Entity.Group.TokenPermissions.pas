unit VK.Entity.Group.TokenPermissions;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkTokenPermission = class(TVkEntity)
  private
    FName: string;
    FSetting: Integer;
  public
    property Name: string read FName write FName;
    property Setting: Integer read FSetting write FSetting;
  end;

  TVkTokenPermissions = class(TVkEntity)
  private
    FMask: Integer;
    FSettings: TArray<TVkTokenPermission>;
  public
    property Mask: Integer read FMask write FMask;
    property Settings: TArray<TVkTokenPermission> read FSettings write FSettings;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkTokenPermissions}

destructor TVkTokenPermissions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkTokenPermission>(FSettings);
  inherited;
end;

end.

