unit VK.Entity.Fave;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Link, VK.Entity.Media, VK.Entity.Video, VK.Entity.Market, VK.Entity.Photo;

type
  TVkFaveType = (ftPost, ftVideo, ftProduct, ftArticle, ftLink);

  TVkFaveTypeHelper = record helper for TVkFaveType
    function ToString: string; inline;
    class function FromString(Value: string): TVkFaveType; static; inline;
  end;

  TVkFaveTag = class
  private
    FId: Integer;
    FName: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaveTag;
  end;

  TVkFaveTags = class
  private
    FCount: Integer;
    FItems: TArray<TVkFaveTag>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFaveTag> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaveTags;
  end;

  TVkFave = class
  private
    FAdded_date: Int64;
    FSeen: Boolean;
    FTags: TArray<TVkFaveTag>;
    FType: string;
    FLink: TVkLink;
    FPost: TVkPost;
    FVideo: TVkVideo;
    FProduct: TVkProduct;
    FPhoto: TVkPhoto;
    function GetAdded_date: TDateTime;
    procedure SetAdded_date(const Value: TDateTime);
    function GetType: TVkFaveType;
    procedure SetType(const Value: TVkFaveType);
  public
    property AddedDate: TDateTime read GetAdded_date write SetAdded_date;
    property Link: TVkLink read FLink write FLink;
    property Post: TVkPost read FPost write FPost;
    property Video: TVkVideo read FVideo write FVideo;
    property Photo: TVkPhoto read FPhoto write FPhoto;
    property Product: TVkProduct read FProduct write FProduct;
    property Seen: Boolean read FSeen write FSeen;
    property Tags: TArray<TVkFaveTag> read FTags write FTags;
    property&Type: TVkFaveType read GetType write SetType;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFave;
  end;

  TVkFaves = class
  private
    FCount: Integer;
    FItems: TArray<TVkFave>;
  public
    property Count: Integer read FCount write FCount;
    property Items: TArray<TVkFave> read FItems write FItems;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkFaves;
  end;

implementation

uses
  System.DateUtils, VK.CommonUtils;

{TTagsClass}

function TVkFaveTag.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFaveTag.FromJsonString(AJsonString: string): TVkFaveTag;
begin
  result := TJson.JsonToObject<TVkFaveTag>(AJsonString)
end;

{TItemsClass}

constructor TVkFave.Create;
begin
  inherited;
  //FLink := TLinkClass.Create();
end;

destructor TVkFave.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFaveTag>(FTags);
  if Assigned(FLink) then
    FLink.Free;
  if Assigned(FPost) then
    FPost.Free;
  if Assigned(FVideo) then
    FVideo.Free;
  if Assigned(FProduct) then
    FProduct.Free;
  if Assigned(FPhoto) then
    FPhoto.Free;
  inherited;
end;

function TVkFave.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFave.FromJsonString(AJsonString: string): TVkFave;
begin
  result := TJson.JsonToObject<TVkFave>(AJsonString)
end;

function TVkFave.GetAdded_date: TDateTime;
begin
  Result := UnixToDateTime(FAdded_date, False);
end;

function TVkFave.GetType: TVkFaveType;
begin
  Result := TVkFaveType.FromString(FType);
end;

procedure TVkFave.SetAdded_date(const Value: TDateTime);
begin
  FAdded_date := DateTimeToUnix(Value, False);
end;

procedure TVkFave.SetType(const Value: TVkFaveType);
begin
  FType := Value.ToString;
end;

{TVkFaves}

destructor TVkFaves.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFave>(FItems);
  inherited;
end;

function TVkFaves.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkFaves.FromJsonString(AJsonString: string): TVkFaves;
begin
  result := TJson.JsonToObject<TVkFaves>(AJsonString)
end;

{ TVkFaveTags }

destructor TVkFaveTags.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkFaveTag>(FItems);
  inherited;
end;

class function TVkFaveTags.FromJsonString(AJsonString: string): TVkFaveTags;
begin
  result := TJson.JsonToObject<TVkFaveTags>(AJsonString)
end;

function TVkFaveTags.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

{ TVkFaveTypeHelper }

class function TVkFaveTypeHelper.FromString(Value: string): TVkFaveType;
begin
  if Value = 'post' then
    Result := ftPost
  else if Value = 'video' then
    Result := ftVideo
  else if Value = 'product' then
    Result := ftProduct
  else if Value = 'article' then
    Result := ftArticle
  else if Value = 'link' then
    Result := ftLink
  else
    Result := ftPost;
end;

function TVkFaveTypeHelper.ToString: string;
begin
  case Self of
    ftPost:
      Result := 'post';
    ftVideo:
      Result := 'video';
    ftProduct:
      Result := 'product';
    ftArticle:
      Result := 'article';
    ftLink:
      Result := 'link';
  end;
end;

end.

