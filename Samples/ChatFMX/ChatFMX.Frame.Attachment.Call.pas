unit ChatFMX.Frame.Attachment.Call;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.Controls.Presentation, VK.API, VK.Entity.Call,
  VK.Types, ChatFMX.Frame.Attachment;

type
  TFrameAttachmentCall = class(TFrameAttachment)
    RectangleBG: TRectangle;
    Layout1: TLayout;
    Path1: TPath;
    LayoutContent: TLayout;
    LabelDetail: TLabel;
    LabelTitle: TLabel;
  private
    FState: TVkCallState;
    FDuration: Int64;
    FIncoming: Boolean;
    procedure SetState(const Value: TVkCallState);
    procedure UpdateState;
    procedure SetDuration(const Value: Int64);
    procedure SetIncoming(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); override;
    procedure Fill(Call: TVkCall);
    property State: TVkCallState read FState write SetState;
    property Duration: Int64 read FDuration write SetDuration;
    property Incoming: Boolean read FIncoming write SetIncoming;
  end;

implementation

uses
  ChatFMX.Utils, HGM.Common.DateUtils;

{$R *.fmx}

{ TFrameAttachmentCall }

constructor TFrameAttachmentCall.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited;
end;

procedure TFrameAttachmentCall.Fill(Call: TVkCall);
begin
  State := Call.State;
  Duration := Call.Duration;
  Incoming := Call.InitiatorId <> VK.UserId;
end;

procedure TFrameAttachmentCall.SetDuration(const Value: Int64);
begin
  FDuration := Value;
  UpdateState;
end;

procedure TFrameAttachmentCall.SetIncoming(const Value: Boolean);
begin
  FIncoming := Value;
  if FIncoming then
    LabelTitle.Text := 'Входящий звонок'
  else
    LabelTitle.Text := 'Исходящий звонок';
end;

procedure TFrameAttachmentCall.SetState(const Value: TVkCallState);
begin
  FState := Value;
  UpdateState;
end;

procedure TFrameAttachmentCall.UpdateState;
begin
  case FState of
    TVkCallState.CanceledByInitiator:
      LabelDetail.Text := 'Пропущен';
    TVkCallState.Reached:
      LabelDetail.Text := 'Завершён · ' + SecondsToMinFormat(Duration);
    TVkCallState.CanceledByReceiver:
      LabelDetail.Text := 'Отменён';
  end;
end;

end.

