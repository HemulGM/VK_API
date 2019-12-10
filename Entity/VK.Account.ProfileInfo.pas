unit VK.Account.ProfileInfo;

interface

uses
  Generics.Collections, Rest.Json;

type
  TRelation_requestsClass = class
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FId: Extended;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property can_access_closed: Boolean read FCan_access_closed write FCan_access_closed;
    property first_name: string read FFirst_name write FFirst_name;
    property id: Extended read FId write FId;
    property is_closed: Boolean read FIs_closed write FIs_closed;
    property last_name: string read FLast_name write FLast_name;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TRelation_requestsClass;
  end;

  TRelation_partnerClass = class
  private
    FCan_access_closed: Boolean;
    FFirst_name: string;
    FId: Extended;
    FIs_closed: Boolean;
    FLast_name: string;
  public
    property can_access_closed: Boolean read FCan_access_closed write FCan_access_closed;
    property first_name: string read FFirst_name write FFirst_name;
    property id: Extended read FId write FId;
    property is_closed: Boolean read FIs_closed write FIs_closed;
    property last_name: string read FLast_name write FLast_name;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TRelation_partnerClass;
  end;

  TCountryClass = class
  private
    FId: Extended;
    FTitle: string;
  public
    property id: Extended read FId write FId;
    property title: string read FTitle write FTitle;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TCountryClass;
  end;

  TProfileInfoClass = class
  private
    FBdate: string;
    FBdate_visibility: Extended;
    FCountry: TCountryClass;
    FFirst_name: string;
    FHome_town: string;
    FLast_name: string;
    FPhone: string;
    FRelation: Extended;
    FRelation_partner: TRelation_partnerClass;
    FRelation_requests: TArray<TRelation_requestsClass>;
    FScreen_name: string;
    FSex: Extended;
    FStatus: string;
  public
    property bdate: string read FBdate write FBdate;
    property bdate_visibility: Extended read FBdate_visibility write FBdate_visibility;
    property country: TCountryClass read FCountry write FCountry;
    property first_name: string read FFirst_name write FFirst_name;
    property home_town: string read FHome_town write FHome_town;
    property last_name: string read FLast_name write FLast_name;
    property phone: string read FPhone write FPhone;
    property relation: Extended read FRelation write FRelation;
    property relation_partner: TRelation_partnerClass read FRelation_partner write FRelation_partner;
    property relation_requests: TArray<TRelation_requestsClass> read FRelation_requests write FRelation_requests;
    property screen_name: string read FScreen_name write FScreen_name;
    property sex: Extended read FSex write FSex;
    property status: string read FStatus write FStatus;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TProfileInfoClass;
  end;

implementation

{TRelation_requestsClass}

function TRelation_requestsClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TRelation_requestsClass.FromJsonString(AJsonString: string): TRelation_requestsClass;
begin
  result := TJson.JsonToObject<TRelation_requestsClass>(AJsonString)
end;

{TRelation_partnerClass}

function TRelation_partnerClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TRelation_partnerClass.FromJsonString(AJsonString: string): TRelation_partnerClass;
begin
  result := TJson.JsonToObject<TRelation_partnerClass>(AJsonString)
end;

{TCountryClass}

function TCountryClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TCountryClass.FromJsonString(AJsonString: string): TCountryClass;
begin
  result := TJson.JsonToObject<TCountryClass>(AJsonString)
end;

{TProfileInfoClass}

constructor TProfileInfoClass.Create;
begin
  inherited;
  FCountry := TCountryClass.Create();
  FRelation_partner := TRelation_partnerClass.Create();
end;

destructor TProfileInfoClass.Destroy;
var
  Lrelation_requestsItem: TRelation_requestsClass;
begin

  for Lrelation_requestsItem in FRelation_requests do
    Lrelation_requestsItem.Free;

  FCountry.Free;
  FRelation_partner.Free;
  inherited;
end;

function TProfileInfoClass.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TProfileInfoClass.FromJsonString(AJsonString: string): TProfileInfoClass;
begin
  result := TJson.JsonToObject<TProfileInfoClass>(AJsonString)
end;

end.

