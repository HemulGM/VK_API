unit VK.Entity.Doc;

interface

uses
  Generics.Collections, REST.JsonReflect, Rest.Json, Vk.Types, VK.Entity.Common,
  VK.Entity.Common.List, VK.Wrap.Interceptors, VK.Entity.AudioMessage;

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
    /// Изображения для предпросмотра
    /// </summary>
    property Photo: TVkPreviewPhoto read FPhoto write FPhoto;
    /// <summary>
    /// Данные о граффити
    /// </summary>
    property Graffiti: TVkSize read FGraffiti write FGraffiti;
    /// <summary>
    /// Данные об аудиосообщении
    /// </summary>
    property AudioMessage: TVkAudioMessage read FAudio_message write FAudio_message;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Объект, описывающий документ
  /// </summary>
  TVkDocument = class(TVkObject, IAttachment)
  private
    FAccess_key: string;
    [JsonReflectAttribute(ctString, rtString, TVkUnixDateTimeInterceptor)]
    FDate: TDateTime;
    FExt: string;
    [JsonReflectAttribute(ctString, rtString, TIntBooleanInterceptor)]
    FIs_licensed: Boolean;
    FOwner_id: TVkPeerId;
    FPreview: TVkPreview;
    FSize: Int64;
    FTitle: string;
    [JsonReflectAttribute(ctString, rtString, TDocumentTypeInterceptor)]
    FType: TVkDocumentType;
    FUrl: string;
    function GetSizeStr: string;
  public
    /// <summary>
    /// Идентификатор документа
    /// </summary>
    property Id;
    /// <summary>
    /// Ключ доступа
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// Дата добавления
    /// </summary>
    property Date: TDateTime read FDate write FDate;
    /// <summary>
    /// Расширение документа
    /// </summary>
    property Ext: string read FExt write FExt;
    property IsLicensed: Boolean read FIs_licensed write FIs_licensed;
    /// <summary>
    /// Идентификатор пользователя, загрузившего документ
    /// </summary>
    property OwnerId: TVkPeerId read FOwner_id write FOwner_id;
    /// <summary>
    /// Информация для предварительного просмотра документа
    /// </summary>
    property Preview: TVkPreview read FPreview write FPreview;
    /// <summary>
    /// Размер документа в байтах
    /// </summary>
    property Size: Int64 read FSize write FSize;
    /// <summary>
    /// Размер документа в строковом представлении
    /// </summary>
    property SizeStr: string read GetSizeStr;
    /// <summary>
    /// Название документа
    /// </summary>
    property Title: string read FTitle write FTitle;
    /// <summary>
    /// Тип документа
    /// </summary>
    property &Type: TVkDocumentType read FType write FType;
    /// <summary>
    /// Адрес документа, по которому его можно загрузить
    /// </summary>
    property Url: string read FUrl write FUrl;
    destructor Destroy; override;
    function ToAttachment: TAttachment;
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

destructor TVkDocument.Destroy;
begin
  {$IFNDEF AUTOREFCOUNT}
  if Assigned(FPreview) then
    FPreview.Free;
  {$ENDIF}
  inherited;
end;

function TVkDocument.ToAttachment: TAttachment;
begin
  Result := TAttachment.Doc(OwnerId, Id, FAccess_key);
end;

function TVkDocument.GetSizeStr: string;
begin
  if FSize / (1024 * 1024) > 1 then
    Result := FormatFloat('0.00 мб', FSize / 1024 / 1024)
  else
    Result := FormatFloat('0.00 кб', FSize / 1024);
end;

end.

