unit VK.Entity.Sticker;

interface

uses
  VK.Entity.Photo, VK.Entity.Common;

type
  TVkSticker = class
  private
    FImages: TArray<TVkImage>;
    FImages_with_background: TArray<TVkImage>;
    FProduct_id: Integer;
    FSticker_id: Integer;
    FAccess_key: string;
    FAnimation_url: string;
    FIs_allowed: Boolean;
  public
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// URL �������� �������
    /// </summary>
    property AnimationUrl: string read FAnimation_url write FAnimation_url;
    /// <summary>
    /// ����������� ��� ������� (� ���������� �����)
    /// </summary>
    property Images: TArray<TVkImage> read FImages write FImages;
    /// <summary>
    /// ����������� ��� ������� (� ������������ �����)
    /// </summary>
    property ImagesWithBackground: TArray<TVkImage> read FImages_with_background write FImages_with_background;
    /// <summary>
    /// ������������� ������
    /// </summary>
    property ProductId: Integer read FProduct_id write FProduct_id;
    /// <summary>
    /// ������������� �������
    /// </summary>
    property StickerId: Integer read FSticker_id write FSticker_id;
    /// <summary>
    /// ���������� � ���, �������� �� ������
    /// </summary>
    property IsAllowed: Boolean read FIs_allowed write FIs_allowed;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkSticker}

destructor TVkSticker.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkImage>(FImages);
  TArrayHelp.FreeArrayOfObject<TVkImage>(FImages_with_background);
  inherited;
end;

end.

