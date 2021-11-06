unit VK.Entity.Privacy;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkPrivacyOwners = class
  private
    FAllowed: TArray<Integer>;
    FExcluded: TArray<Integer>;
  public
    property Allowed: TArray<Integer> read FAllowed write FAllowed;
    property Excluded: TArray<Integer> read FExcluded write FExcluded;
  end;

  TVkPrivacy = class(TVkEntity)
  private
    FCategory: string;
    FOwners: TVkPrivacyOwners;
  public
    property Category: string read FCategory write FCategory;
    property Owners: TVkPrivacyOwners read FOwners write FOwners;
    destructor Destroy; override;
  end;

implementation

{TVkPrivacy}

destructor TVkPrivacy.Destroy;
begin
  if Assigned(FOwners) then
    FOwners.Free;
  inherited;
end;

end.

