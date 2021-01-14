unit VK.Entity.Doc;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common, VK.Entity.Attachment;

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
    FDate: Int64;
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
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetSizeStr: string;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    property Date: TDateTime read GetDate write SetDate;
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

  TVkDocuments = class(TVkEntity)
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
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  VK.Types, System.SysUtils, System.DateUtils, VK.CommonUtils;

{TVkPreviewPhoto}

destructor TVkPreviewPhoto.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
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
  FPhoto.Free;
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
  FPreview.Free;
  inherited;
end;

function TVkDocument.ToAttachment: string;
begin
  Result := Attachment.Doc(FId, FOwner_id, FAccess_key);
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
begin
  {$IFNDEF AUTOREFCOUNT}
  if not FSaveObjects then
  begin
    TArrayHelp.FreeArrayOfObject<TVkDocument>(FItems);
  end;
  {$ENDIF}
  inherited;
end;

procedure TVkDocuments.SetSaveObjects(const Value: Boolean);
begin
  FSaveObjects := Value;
end;

end.

