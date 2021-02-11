unit VK.Entity.Doc;

interface

uses
  Generics.Collections, REST.JsonReflect, REST.Json.Interceptors, Rest.Json,
  Vk.Types, VK.Entity.Common, VK.Entity.Attachment, VK.Entity.Common.List,
  VK.Wrap.Interceptors, VK.Entity.AudioMessage;

type
  TVkPreviewPhoto = class
  private
    FSizes: TVkSizes;
  public
    property Sizes: TVkSizes read FSizes write FSizes;
    destructor Destroy; override;
  end;

  TVkPreview = class(TVkEntity)
  private
    FPhoto: TVkPreviewPhoto;
    FGraffiti: TVkSize;
    FAudio_message: TVkAudioMessage;
  public
    /// <summary>
    /// »зображени€ дл€ предпросмотра
    /// </summary>
    property Photo: TVkPreviewPhoto read FPhoto write FPhoto;
    /// <summary>
    /// ƒанные о граффити
    /// </summary>
    property Graffiti: TVkSize read FGraffiti write FGraffiti;
    /// <summary>
    /// ƒанные об аудиосообщении
    /// </summary>
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    destructor Destroy; override;
  end;

  /// <summary>
  /// ќбъект, описывающий документ
  /// </summary>
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
    [JsonReflectAttribute(ctString, rtString, TDocumentTypeInterceptor)]
    FType: TVkDocumentType;
    FUrl: string;
    function GetSizeStr: string;
  public
    /// <summary>
    /// »дентификатор документа
    /// </summary>
    property Id;
    /// <summary>
    ///  люч доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ƒата добавлени€
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// –асширение документа
    /// </summary>
    property Ext: string read FExt write FExt;
    property IsLicensed: Boolean read FIs_licensed write FIs_licensed;
    /// <summary>
    /// »дентификатор пользовател€, загрузившего документ
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    /// <summary>
    /// »нформаци€ дл€ предварительного просмотра документа
    /// </summary>
    property Preview: TVkPreview read FPreview write FPreview;
    /// <summary>
    /// –азмер документа в байтах
    /// </summary>
    property Size: Integer read FSize write FSize;
    property SizeStr: string read GetSizeStr;
    /// <summary>
    /// Ќазвание документа
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// “ип документа
    /// </summary>
    property&Type: TVkDocumentType read FType write FType;
    /// <summary>
    /// јдрес документа, по которому его можно загрузить
    /// </summary>
    property Url: string read FUrl write FUrl;
    constructor Create; override;
    destructor Destroy; override;
    function ToAttachment: string;
  end;

  TVkDocuments = TVkEntityList<TVkDocument>;

implementation

uses
  System.SysUtils, System.DateUtils, VK.CommonUtils;

{TVkPreviewPhoto}

destructor TVkPreviewPhoto.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  TArrayHelp.FreeArrayOfObject<TVkSize>(FSizes);
  {$ENDIF}
  inherited;
end;

{TVkPreview}

destructor TVkPreview.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FGraffiti) then
    FGraffiti.Free;
  if Assigned(FAudio_message) then
    FAudio_message.Free;
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
  Result := TAttachment.Doc(FId, FOwner_id, FAccess_key);
end;

function TVkDocument.GetSizeStr: string;
begin
  if FSize / (1024 * 1024) > 1 then
    Result := FormatFloat('0.00 мб', FSize / 1024 / 1024)
  else
    Result := FormatFloat('0.00 кб', FSize / 1024);
end;

end.

