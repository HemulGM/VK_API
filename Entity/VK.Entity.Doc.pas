unit VK.Entity.Doc;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json, VK.Entity.Common, VK.Entity.Attachment,
  VK.Entity.Common.List;

type
  TVkPreviewPhoto = class
  private
    FSizes: TVkSizes;
  public
    property Sizes: TVkSizes read FSizes write FSizes;
    destructor Destroy; override;
  end;

  TVkPreview = class
  private
    FPhoto: TVkPreviewPhoto;
  public
    property Photo: TVkPreviewPhoto read FPhoto write FPhoto;
    constructor Create;
    destructor Destroy; override;
  end;

  TVkDocument = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FExt: string;
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
    function GetSizeStr: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Date: TDateTime read FDate write FDate;
    property Ext: string read FExt write FExt;
    property IsLicensed: Boolean read FIs_licensed write FIs_licensed;
    property OwnerId: Integer read FOwner_id write FOwner_id;
    property Preview: TVkPreview read FPreview write FPreview;
    property Size: Integer read FSize write FSize;
    property SizeStr: string read GetSizeStr;
    property Title: string read FTitle write FTitle;
    property&Type: Integer read FType write FType;
    property Url: string read FUrl write FUrl;
    constructor Create; override;
    destructor Destroy; override;
    function ToAttachment: string;
  end;

  TVkDocuments = TVkEntityList<TVkDocument>;

implementation

uses
  VK.Types, System.SysUtils, System.DateUtils, VK.CommonUtils;

{TVkPreviewPhoto}

destructor TVkPreviewPhoto.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  {$ENDIF}
  inherited;
end;

{TVkPreview}

constructor TVkPreview.Create;
begin
  inherited;
  FPhoto := TVkPreviewPhoto.Create();
end;

destructor TVkPreview.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  FPhoto.Free;
  {$ENDIF}
  inherited;
end;

{TVkDocument}

constructor TVkDocument.Create;
begin
  inherited;
  FPreview := TVkPreview.Create();
end;

destructor TVkDocument.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  FPreview.Free;
  {$ENDIF}
  inherited;
end;

function TVkDocument.ToAttachment: string;
begin
  Result := Attachment.Doc(FId, FOwner_id, FAccess_key);
end;

function TVkDocument.GetSizeStr: string;
begin
  if FSize / (1024 * 1024) > 1 then
    Result := FormatFloat('0.00 мб', FSize / 1024 / 1024)
  else
    Result := FormatFloat('0.00 кб', FSize / 1024);
end;

end.

