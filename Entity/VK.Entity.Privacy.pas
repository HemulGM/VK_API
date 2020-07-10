unit VK.Entity.Privacy;

interface

uses
  Generics.Collections, Rest.Json;

type
  TVkPrivacyOwners = class
  private
    FAllowed: TArray<Integer>;
    FExcluded: TArray<Integer>;
  public
    property Allowed: TArray<Integer> read FAllowed write FAllowed;
    property Excluded: TArray<Integer> read FExcluded write FExcluded;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPrivacyOwners;
  end;

  TVkPrivacy = class
  private
    FCategory: string;
    FOwners: TVkPrivacyOwners;
  public
    property Category: string read FCategory write FCategory;
    property Owners: TVkPrivacyOwners read FOwners write FOwners;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPrivacy;
  end;

implementation

{TVkPrivacyOwners}

function TVkPrivacyOwners.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPrivacyOwners.FromJsonString(AJsonString: string): TVkPrivacyOwners;
begin
  result := TJson.JsonToObject<TVkPrivacyOwners>(AJsonString)
end;

{TVkPrivacy}

constructor TVkPrivacy.Create;
begin
  inherited;
  FOwners := TVkPrivacyOwners.Create();
end;

destructor TVkPrivacy.Destroy;
begin
  FOwners.Free;
  inherited;
end;

function TVkPrivacy.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPrivacy.FromJsonString(AJsonString: string): TVkPrivacy;
begin
  result := TJson.JsonToObject<TVkPrivacy>(AJsonString)
end;

end.

