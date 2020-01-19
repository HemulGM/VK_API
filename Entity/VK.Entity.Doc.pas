unit VK.Entity.Doc;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkPreviewPhoto = class
  private
    FSizes: TArray<TVkSizes>;
  public
    property sizes: TArray<TVkSizes> read FSizes write FSizes;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkPreviewPhoto;
  end;

  TVkPreviewGraffiti = class
  private
    FWidth: integer;
    FSrc: string;
    FHeight: integer;
  public
    property src: string read FSrc write FSrc;
    property width: integer read FWidth write FWidth;
    property height: integer read FHeight write FHeight;
  end;

  TVkPreviewAudioMessage = class
  private
    FDuration: integer;
    Fink_mp3: string;
    FLink_ogg: string;
    FWaveform: TArray<Integer>;
  public
    property duration: integer read FDuration write FDuration;
    property waveform: TArray<Integer> read FWaveform write FWaveform;
    property link_ogg: string read FLink_ogg write FLink_ogg;
    property link_mp3: string read Fink_mp3 write Fink_mp3;
  end;

  TVkPreview = class
  private
    FPhoto: TVkPreviewPhoto;
  public
    property photo: TVkPreviewPhoto read FPhoto write FPhoto;
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
    FId: Extended;
    FIs_licensed: Extended;
    FOwner_id: Extended;
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
    property access_key: string read FAccess_key write FAccess_key;
    property date: Extended read FDate write FDate;
    property ext: string read FExt write FExt;
    property id: Extended read FId write FId;
    property is_licensed: Extended read FIs_licensed write FIs_licensed;
    property owner_id: Extended read FOwner_id write FOwner_id;
    property preview: TVkPreview read FPreview write FPreview;
    property size: Extended read FSize write FSize;
    property title: string read FTitle write FTitle;
    property&type: Extended read FType write FType;
    property url: string read FUrl write FUrl;
    constructor Create;
    destructor Destroy; override;
    function ToJsonString: string;
    class function FromJsonString(AJsonString: string): TVkDocument;
  end;

implementation

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

function TVkDocument.ToJsonString: string;
begin
  result := TJson.ObjectToJsonString(self);
end;

class function TVkDocument.FromJsonString(AJsonString: string): TVkDocument;
begin
  result := TJson.JsonToObject<TVkDocument>(AJsonString)
end;

end.

