unit VK.Entity.Doc;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkPreviewPhoto = class
  private
    FSizes: TArray<TVkSizes>;
  public
    property Sizes: TArray<TVkSizes> read FSizes write FSizes;
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
    FDate: Extended;
    FExt: string;
    FId: Integer;
    FIs_licensed: Extended;
    FOwner_id: Integer;
    FPreview: TVkPreview;
    FSize: Extended;
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
    FType: Extended;
    FUrl: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Date: Extended read FDate write FDate;
    property Ext: string read FExt write FExt;
    property Id: Integer read FId write FId;
    property IsLicensed: Extended read FIs_licensed write FIs_licensed;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Preview: TVkPreview read FPreview write FPreview;
    property Size: Extended read FSize write FSize;
    property Title: string read FTitle write FTitle;
    property&Type: Extended read FType write FType;
    property Url: string read FUrl write FUrl;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    function ToAttachment: string;
    class function FromJsonString(AJsonString: string): TVkDocument;
  end;

implementation

uses
  VK.Types, System.SysUtils;

{TVkPreviewPhoto}

destructor TVkPreviewPhoto.Destroy;
var
  LsizesItem: TVkSizes;
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
  Result := CreateAttachment('doc', FOwner_id, FId, FAccess_key);
end;

function TVkDocument.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkDocument.FromJsonString(AJsonString: string): TVkDocument;
begin
  result := TJson.JsonToObject<TVkDocument>(AJsonString)
end;

end.

