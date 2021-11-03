unit VK.Entity.Poll;

interface

uses
  Generics.Collections, REST.Json.Interceptors, REST.JsonReflect, Rest.Json,
  VK.Entity.Photo, VK.Entity.Profile, VK.Entity.Group, VK.Entity.Common,
  VK.Entity.Common.List, VK.Types;

type
  TVkPollFriends = class(TVkObject)
  end;

  TVkPollPoints = class
  private
    FColor: string;
    FPosition: Integer;
  public
    property Color: string read FColor write FColor;
    property Position: Integer read FPosition write FPosition;
  end;

  TVkPollBackground = class(TVkBasicObject)
  private
    FAngle: Integer;
    FColor: string;
    FPoints: TArray<TVkPollPoints>;
    FType: string;
    FWidth: Integer;
    FHeight: Integer;
    FImages: TArray<TVkSize>;
  public
    /// <summary>
    /// ������������� ����
    /// </summary>
    property Id;
    property Name;
    /// <summary>
    /// (��� type = gradient) ���� ��������� �� ��� X
    /// </summary>
    property Angle: Integer read FAngle write FAngle;
    /// <summary>
    /// HEX-��� ����������� ����� (��� #)
    /// </summary>
    property Color: string read FColor write FColor;
    /// <summary>
    /// (��� type = gradient) ����� ���������. ������ ��������, ������ �� ������� �������� ���� position (number) � ��������� ����� � � color (string) � HEX-��� ����� �����.
    /// </summary>
    property Points: TArray<TVkPollPoints> read FPoints write FPoints;
    /// <summary>
    /// ��� ����. ��������� ��������: gradient, tile
    /// </summary>
    property &Type: string read FType write FType;
    /// <summary>
    /// (��� type = tile) ������ ������ ��������
    /// </summary>
    property Width: Integer read FWidth write FWidth;
    /// <summary>
    /// (��� type = tile) ������ ������ ��������
    /// </summary>
    property Height: Integer read FHeight write FHeight;
    /// <summary>
    /// (��� type = tile) ����������� ������ ��������. ������ �������� �����������.
    /// </summary>
    property Images: TArray<TVkSize> read FImages write FImages;
    destructor Destroy; override;
  end;

  TVkPollBackgrounds = TVkEntityList<TVkPollBackground>;

  TVkPollAnswer = class(TVkObject)
  private
    FRate: Integer;
    FText: string;
    FVotes: Integer;
  public
    /// <summary>
    /// ������������� ������
    /// </summary>
    property Id;
    /// <summary>
    /// ������� ������
    /// </summary>
    property Rate: Integer read FRate write FRate;
    /// <summary>
    /// ����� ������
    /// </summary>
    property Text: string read FText write FText;
    /// <summary>
    /// ����� ��������������� �� ���� �����
    /// </summary>
    property Votes: Integer read FVotes write FVotes;
  end;

  TVkPoll = class(TVkObject, IAttachment)
  private
    FAnonymous: Boolean;
    FAnswer_ids: TArray<Integer>;
    FAnswers: TArray<TVkPollAnswer>;
    FAuthor_id: Integer;
    FBackground: TVkPollBackground;
    FCan_edit: Boolean;
    FCan_report: Boolean;
    FCan_share: Boolean;
    FCan_vote: Boolean;
    FClosed: Boolean;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FCreated: TDateTime;
    [JsonReflectAttribute(ctString, rtString, TUnixDateTimeInterceptor)]
    FEnd_date: TDateTime;
    FIs_board: Boolean;
    FMultiple: Boolean;
    FOwner_id: Integer;
    FQuestion: string;
    FVotes: Integer;
    FPhoto: TVkPhoto;
    FFriends: TArray<TVkPollFriends>;
    FDisable_unvote: Boolean;
    FProfiles: TArray<TVkProfile>;
    FGroups: TArray<TVkGroup>;
    FAccess_key: string;
  public
    /// <summary>
    /// ������������� ������ ��� ��������� ���������� � ��� ����� ����� polls.getById.
    /// </summary>
    property Id;
    /// <summary>
    /// ���� �������
    /// </summary>
    property AccessKey: string read FAccess_key write FAccess_key;
    /// <summary>
    /// ������������� ��������� ������
    /// </summary>
    property OwnerId: Integer read FOwner_id write FOwner_id;
    /// <summary>
    /// ���� ��������
    /// </summary>
    property Created: TDateTime read FCreated write FCreated;
    /// <summary>
    /// ����� �������
    /// </summary>
    property Question: string read FQuestion write FQuestion;
    /// <summary>
    /// ���������� �������
    /// </summary>
    property Votes: Integer read FVotes write FVotes;
    /// <summary>
    /// ������ ��������, ������� �������� ���������� � ��������� ������
    /// </summary>
    property Answers: TArray<TVkPollAnswer> read FAnswers write FAnswers;
    /// <summary>
    /// �������� �� ����� ���������.
    /// </summary>
    property Anonymous: Boolean read FAnonymous write FAnonymous;
    /// <summary>
    /// ��������� �� ����� ����� ���������� ��������� ������
    /// </summary>
    property Multiple: Boolean read FMultiple write FMultiple;
    /// <summary>
    /// �������������� ��������� ������, ��������� ������� �������������.
    /// </summary>
    property AnswerIds: TArray<Integer> read FAnswer_ids write FAnswer_ids;
    /// <summary>
    /// ���� ���������� ������. 0, ���� ����� ����������.
    /// </summary>
    property EndDate: TDateTime read FEnd_date write FEnd_date;
    /// <summary>
    /// �������� �� ����� �����������
    /// </summary>
    property Closed: Boolean read FClosed write FClosed;
    /// <summary>
    /// ��������� �� ����� � ����������
    /// </summary>
    property IsBoard: Boolean read FIs_board write FIs_board;
    /// <summary>
    /// ����� �� ��������������� �����
    /// </summary>
    property CanEdit: Boolean read FCan_edit write FCan_edit;
    /// <summary>
    /// ����� �� ������������� � ������
    /// </summary>
    property CanVote: Boolean read FCan_vote write FCan_vote;
    /// <summary>
    /// ����� �� ������������ �� �����
    /// </summary>
    property CanReport: Boolean read FCan_report write FCan_report;
    /// <summary>
    /// ����� �� ���������� �������
    /// </summary>
    property CanShare: Boolean read FCan_share write FCan_share;
    /// <summary>
    /// ������������� ������ ������
    /// </summary>
    property AuthorId: Integer read FAuthor_id write FAuthor_id;
    /// <summary>
    /// ���������� � ��� �������� ������
    /// </summary>
    property Photo: TVkPhoto read FPhoto write FPhoto;
    /// <summary>
    /// ��� �������� ������
    /// </summary>
    property Background: TVkPollBackground read FBackground write FBackground;
    /// <summary>
    /// �������������� 3 ������, ������� ������������� � ������
    /// </summary>
    property Friends: TArray<TVkPollFriends> read FFriends write FFriends;
    property DisableUnvote: Boolean read FDisable_unvote write FDisable_unvote;
    //Extended
    property Profiles: TArray<TVkProfile> read FProfiles write FProfiles;
    property Groups: TArray<TVkGroup> read FGroups write FGroups;
    //
    destructor Destroy; override;
    function ToAttachment: TAttachment;
  end;

  TVkPollVoters = class(TVkEntity)
  private
    FAnswer_id: Integer;
    FUsers: TVkProfiles;
  public
    property AnswerId: Integer read FAnswer_id write FAnswer_id;
    property Users: TVkProfiles read FUsers write FUsers;
  end;

implementation

uses
  VK.CommonUtils;

{TVkPollBackground}

destructor TVkPollBackground.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPollPoints>(FPoints);
  TArrayHelp.FreeArrayOfObject<TVkSize>(FImages);
  inherited;
end;

{TVkPoll}

destructor TVkPoll.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkPollAnswer>(FAnswers);
  TArrayHelp.FreeArrayOfObject<TVkPollFriends>(FFriends);
  TArrayHelp.FreeArrayOfObject<TVkGroup>(FGroups);
  TArrayHelp.FreeArrayOfObject<TVkProfile>(FProfiles);
  if Assigned(FPhoto) then
    FPhoto.Free;
  if Assigned(FBackground) then
    FBackground.Free;
  inherited;
end;

function TVkPoll.ToAttachment: TAttachment;
begin
  Result := TAttachment.Poll(OwnerId, Id, AccessKey);
end;

end.

