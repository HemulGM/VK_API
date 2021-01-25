unit VK.Entity.Donut;

interface

uses
  REST.Json, REST.JsonReflect, REST.Json.Interceptors, VK.Entity.Profile,
  VK.Entity.Group, VK.Entity.Common;

type
  TVkDonut = class(TVkEntity)
  private
    FIs_donut: Boolean;
    FPaid_duration: Integer;
    FPlaceholder: Integer;
    FCan_publish_free_copy: Boolean;
    FEdit_mode: string;
  public
    /// <summary>
    /// ������ �������� ������ ������� ����������� VK Donut
    /// </summary>
    property IsDonut: Boolean read FIs_donut write FIs_donut;
    /// <summary>
    /// ����� �� ������� ������ ��� ���� �������������, � �� ������ ����������� VK Donut
    /// </summary>
    property CanPublishFreeCopy: Boolean read FCan_publish_free_copy write FCan_publish_free_copy;
    /// <summary>
    /// �����, � ������� �������� ������ ����� �������� ������ ������� ����������� VK Donut
    /// </summary>
    property PaidDuration: Integer read FPaid_duration write FPaid_duration;
    /// <summary>
    /// �������� ��� �������������, ������� �� �������� �������� VK Donut. ������������ ������ ����������� ������
    /// </summary>
    property Placeholder: Integer read FPlaceholder write FPlaceholder;
    /// <summary>
    /// ���������� � ���, ����� �������� VK Donut ����� �������� � ������. ��������� ��������:
    /// all � ��� ���������� � VK Donut.
    /// duration � �����, � ������� �������� ������ ����� �������� ������ ������� ����������� VK Donut.
    /// </summary>
    property EditMode: string read FEdit_mode write FEdit_mode;
  end;

  TVkDonutInfo = class
  private
    FIs_don: Boolean;
    FPlaceholder: string;
  public
    /// <summary>
    /// �������� �� ����������� ����������� VK Donut
    /// </summary>
    property IsDon: Boolean read FIs_don write FIs_don;
    /// <summary>
    /// �������� ��� �������������, ������� �� �������� �������� VK Donut
    /// </summary>
    property Placeholder: string read FPlaceholder write FPlaceholder;
  end;

  TVkDonutSubscription = class(TVkEntity)
  private
    FAmount: Integer;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FNext_Payment_Date: TDateTime;
    FOwner_Id: Integer;
    FStatus: string;
  public
    /// <summary>
    /// ��������� �������� (� ������)
    /// </summary>
    property Amount: Integer read FAmount write FAmount;
    /// <summary>
    /// ���� ���������� �������
    /// </summary>
    property NextPaymentDate: TDateTime read FNext_Payment_Date write FNext_Payment_Date;
    /// <summary>
    /// ������������� ����������, ����� �������� �������� ������������
    /// </summary>
    property OwnerId: Integer read FOwner_Id write FOwner_Id;
    /// <summary>
    /// ������ ��������
    /// </summary>
    property Status: string read FStatus write FStatus;
  end;

  TVkDonutSubscriptions = class(TVkEntity)
  private
    FCount: Integer;
    FGroups: TArray<TVkGroup>;
    FProfiles: TArray<TVkProfile>;
    FSubscriptions: TArray<TVkDonutSubscription>;
  public
    property Count: Integer read FCount write FCount;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Subscriptions: TArray<TVkDonutSubscription> read FSubscriptions write FSubscriptions;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{ TVkDonutSubscriptions }

destructor TVkDonutSubscriptions.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  TArrayHelp.FreeArrayOfObject<TVkDonutSubscription>(FSubscriptions);
  inherited;
end;

end.

