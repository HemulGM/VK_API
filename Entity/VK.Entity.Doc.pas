unit VK.Entity.Doc;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkPreviewPhoto = class
  private
    FSizes: TVkSizes;
  public
    property Sizes: TVkSizes read FSizes write FSizes;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPreviewPhoto;
  end;

  TVkPreview = class
  private
    FPhoto: TVkPreviewPhoto;
  public
    property Photo: TVkPreviewPhoto read FPhoto write FPhoto;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPreview;
  end;

  TVkDocument = class
  private
    FAccess_key: string;
    FDate: Int64;
    FExt: string;
    FId: Integer;
    FIs_licensed: Boolean;
    FOwner_id: Integer;
    FPreview: TVkPreview;
    FSize: Integer;
    FTitle: string;
    {
    1 Ч текстовые документы;
    2 Ч архивы;
    3 Ч gif;
    4 Ч изображени€;
    5 Ч аудио;
    6 Ч видео;
    7 Ч электронные книги;
    8 Ч неизвестно.
    }
    FType: Integer;
    FUrl: string;
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetSizeStr: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Date: TDateTime read GetDate write SetDate;
    property Ext: string read FExt write FExt;
    property Id: Integer read FId write FId;
    property IsLicensed: Boolean read FIs_licensed write FIs_licensed;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Preview: TVkPreview read FPreview write FPreview;
    property Size: Integer read FSize write FSize;
    property SizeStr: string read GetSizeStr;
    property Title: string read FTitle write FTitle;
    property&Type: Integer read FType write FType;
    property Url: string read FUrl write FUrl;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    function ToAttachment: string;
    class function FromJsonString(AJsonString: string): TVkDocument;
  end;

  TVkDocuments = class
  private
    FItems: TArray<TVkDocument>;
    FCount: Integer;
    FSaveObjects: Boolean;
    procedure SetSaveObjects(const Value: Boolean);
  public
    property Items: TArray<TVkDocument> read FItems write FItems;
    property Count: Integer read FCount write FCount;
    property SaveObjects: Boolean read FSaveObjects write SetSaveObjects;
    procedure Append(Audios: TVkDocuments);
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDocuments;
  end;

implementation

uses
  VK.Types, System.SysUtils, System.DateUtils;

{TVkPreviewPhoto}

destructor TVkPreviewPhoto.Destroy;
var
  LsizesItem: TVkSize;
begin

  for LsizesItem in FSizes do
    LsizesItem.Free;

  inherited;
end;

function TVkPreviewPhoto.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPreviewPhoto.FromJsonString(AJsonString: string): TVkPreviewPhoto;
begin
  result := TJson.JsonToObject<TVkPreviewPhoto>(AJsonString)
end;

{TVkPreview}

constructor TVkPreview.Create;
begin
  inherited;
  FPhoto := TVkPreviewPhoto.Create();
end;

destructor TVkPreview.Destroy;
begin
  FPhoto.Free;
  inherited;
end;

function TVkPreview.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkPreview.FromJsonString(AJsonString: string): TVkPreview;
begin
  result := TJson.JsonToObject<TVkPreview>(AJsonString)
end;

{TVkDocument}

constructor TVkDocument.Create;
begin
  inherited;
  FPreview := TVkPreview.Create();
end;

destructor TVkDocument.Destroy;
begin
  FPreview.Free;
  inherited;
end;

function TVkDocument.ToAttachment: string;
begin
  Result := Attachment.Doc(FId, FOwner_id, FAccess_key);
end;

function TVkDocument.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkDocument.FromJsonString(AJsonString: string): TVkDocument;
begin
  result := TJson.JsonToObject<TVkDocument>(AJsonString)
end;

function TVkDocument.GetDate: TDateTime;
begin
  Result := UnixToDateTime(FDate, False);
end;

function TVkDocument.GetSizeStr: string;
begin
  if FSize / (1024 * 1024) > 1 then
    Result := FormatFloat('0.00 мб', FSize / 1024 / 1024)
  else
    Result := FormatFloat('0.00 кб', FSize / 1024);
end;

procedure TVkDocument.SetDate(const Value: TDateTime);
begin
  FDate := DateTimeToUnix(Value, False);
end;

{ TVkDocuments }

procedure TVkDocuments.Append(Audios: TVkDocuments);
var
  OldLen: Integer;
begin
  OldLen := Length(Items);
  SetLength(FItems, OldLen + Length(Audios.Items));
  Move(Audios.Items[0], FItems[OldLen], Length(Audios.Items) * SizeOf(TVkDocument));
end;

constructor TVkDocuments.Create;
begin
  inherited;
  FSaveObjects := False;
end;

destructor TVkDocuments.Destroy;
var
  LItemsItem: TVkDocument;
begin
  if not FSaveObjects then
  begin
    for LItemsItem in FItems do
      LItemsItem.Free;
  end;

  inherited;
end;

class function TVkDocuments.FromJsonString(AJsonString: string): TVkDocuments;
begin
  result := TJson.JsonToObject<TVkDocuments>(AJsonString);
end;

procedure TVkDocuments.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

function TVkDocuments.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

end.

