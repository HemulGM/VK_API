unit ChatFMX.Frame.Attachment.Audio;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, VK.API,
  VK.Entity.AudioMessage, VK.Entity.Audio, ChatFMX.DM.Res;

type
  TFrameAttachmentAudio = class(TFrame)
    LayoutControl: TLayout;
    CircleControl: TCircle;
    PathControl: TPath;
    LayoutTrackInfo: TLayout;
    LabelArtist: TLabel;
    LabelDef: TLabel;
    LabelTitle: TLabel;
    RectangleOver: TRectangle;
    LayoutActions: TLayout;
    ButtonPlayNext: TButton;
    ButtonOptions: TButton;
    LayoutTrackBar: TLayout;
    RectangleTrackFill: TRectangle;
    RectangleTrackPosition: TRectangle;
    CirclePos: TCircle;
    LabelTime: TLabel;
    LayoutVolume: TLayout;
    RectangleVolume: TRectangle;
    RectangleVolumePos: TRectangle;
    CircleVolumePos: TCircle;
    procedure CircleControlClick(Sender: TObject);
    procedure LabelArtistResize(Sender: TObject);
    procedure LabelArtistMouseLeave(Sender: TObject);
    procedure LabelArtistMouseEnter(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
    procedure RectangleTrackFillMouseEnter(Sender: TObject);
    procedure RectangleTrackFillMouseLeave(Sender: TObject);
    procedure LayoutVolumeMouseEnter(Sender: TObject);
    procedure LayoutVolumeMouseLeave(Sender: TObject);
  private
    FIsPlay: Boolean;
    FVK: TCustomVK;
    FDuration: Int64;
    FIsPause: Boolean;
    FDurationPlayed: Int64;
    FTitle: string;
    FArtist: string;
    FMouseFrame: Boolean;
    FIsActive: Boolean;
    procedure SetIsPlay(const Value: Boolean);
    procedure SetDuration(const Value: Int64);
    procedure SetIsPause(const Value: Boolean);
    procedure SetDurationPlayed(const Value: Int64);
    procedure UpdateTime;
    procedure SetArtist(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetMouseFrame(const Value: Boolean);
    procedure SetIsActive(const Value: Boolean);
    procedure UpdateControls;
    property MouseFrame: Boolean read FMouseFrame write SetMouseFrame;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure Fill(Audio: TVkAudio);
    property IsPlay: Boolean read FIsPlay write SetIsPlay;
    property IsPause: Boolean read FIsPause write SetIsPause;
    property Duration: Int64 read FDuration write SetDuration;
    property DurationPlayed: Int64 read FDurationPlayed write SetDurationPlayed;
    property Artist: string read FArtist write SetArtist;
    property Title: string read FTitle write SetTitle;
    property IsActive: Boolean read FIsActive write SetIsActive;
  end;

const
  PlayPath = 'm 2.5 0.5 v 9 l 7 -4.5 Z';
  PausePath = 'M224,435.8V76.1c0-6.7-5.4-12.1-12.2-12.1h-71.6c-6.8,0-12.2,5.4-12.2,12.1v359.7c0,6.7,5.4,12.2,12.2,12.2h71.6   C218.6,448,224,442.6,224,435.8z'#13#10 +
    'M371.8,64h-71.6c-6.7,0-12.2,5.4-12.2,12.1v359.7c0,6.7,5.4,12.2,12.2,12.2h71.6c6.7,0,12.2-5.4,12.2-12.2V76.1   C384,69.4,378.6,64,371.8,64z';

implementation

uses
  System.Math, HGM.Common.DateUtils;

{$R *.fmx}

procedure TFrameAttachmentAudio.CircleControlClick(Sender: TObject);
begin
  IsPlay := not IsPlay;
end;

constructor TFrameAttachmentAudio.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
  CirclePos.Visible := False;
  CircleVolumePos.Visible := False;
  MouseFrame := False;
  IsActive := False;
end;

procedure TFrameAttachmentAudio.Fill(Audio: TVkAudio);
begin
  Artist := Audio.Artist;
  Title := Audio.Title;
  Duration := Audio.Duration;
  DurationPlayed := 0; //Duration div 2;
end;

procedure TFrameAttachmentAudio.FrameMouseEnter(Sender: TObject);
begin
  MouseFrame := True;
end;

procedure TFrameAttachmentAudio.FrameMouseLeave(Sender: TObject);
begin
  MouseFrame := False;
end;

procedure TFrameAttachmentAudio.LabelArtistMouseEnter(Sender: TObject);
var
  Item: TLabel absolute Sender;
begin
  Item.TextSettings.Font.Style := Item.TextSettings.Font.Style + [TFontStyle.fsUnderline];
  FrameMouseEnter(nil);
end;

procedure TFrameAttachmentAudio.LabelArtistMouseLeave(Sender: TObject);
var
  Item: TLabel absolute Sender;
begin
  Item.TextSettings.Font.Style := Item.TextSettings.Font.Style - [TFontStyle.fsUnderline];
  FrameMouseLeave(nil);
end;

procedure TFrameAttachmentAudio.LabelArtistResize(Sender: TObject);
begin
  if LabelArtist.Width > 130 then
  begin
    LabelArtist.AutoSize := False;
    LabelArtist.Width := 130;
  end
  else
    LabelArtist.AutoSize := True;
end;

procedure TFrameAttachmentAudio.LayoutVolumeMouseEnter(Sender: TObject);
begin
  CircleVolumePos.Visible := True;
  FrameMouseEnter(nil);
end;

procedure TFrameAttachmentAudio.LayoutVolumeMouseLeave(Sender: TObject);
begin
  CircleVolumePos.Visible := False;
  FrameMouseLeave(nil);
end;

procedure TFrameAttachmentAudio.RectangleTrackFillMouseEnter(Sender: TObject);
begin
  CirclePos.Visible := True;
  FrameMouseEnter(nil);
end;

procedure TFrameAttachmentAudio.RectangleTrackFillMouseLeave(Sender: TObject);
begin
  CirclePos.Visible := False;
  FrameMouseLeave(nil);
end;

procedure TFrameAttachmentAudio.SetIsActive(const Value: Boolean);
begin
  FIsActive := Value;
  UpdateControls;
end;

procedure TFrameAttachmentAudio.SetIsPause(const Value: Boolean);
begin
  FIsPause := Value;
end;

procedure TFrameAttachmentAudio.SetIsPlay(const Value: Boolean);
begin
  FIsPlay := Value;
  if FIsPlay then
  begin
    PathControl.Data.Data := PausePath;
    IsActive := True;
  end
  else
  begin
    PathControl.Data.Data := PlayPath;
    IsActive := False;
  end;
end;

procedure TFrameAttachmentAudio.UpdateControls;
begin
  LayoutTrackBar.Visible := FIsActive;
  LayoutVolume.Visible := FIsActive;
  ButtonPlayNext.Visible := FMouseFrame and (not FIsActive);
  RectangleOver.Visible := FMouseFrame or FIsActive;
  ButtonOptions.Visible := FMouseFrame;
  LabelTime.Visible := not FMouseFrame;
  UpdateTime;
end;

procedure TFrameAttachmentAudio.SetMouseFrame(const Value: Boolean);
begin
  FMouseFrame := Value;
  UpdateControls;
end;

procedure TFrameAttachmentAudio.SetArtist(const Value: string);
begin
  FArtist := Value;
  LabelArtist.Text := FArtist;
end;

procedure TFrameAttachmentAudio.SetTitle(const Value: string);
begin
  FTitle := Value;
  LabelTitle.Text := FTitle;
end;

procedure TFrameAttachmentAudio.SetDuration(const Value: Int64);
begin
  FDuration := Value;
  UpdateTime;
end;

procedure TFrameAttachmentAudio.SetDurationPlayed(const Value: Int64);
begin
  FDurationPlayed := Value;
  UpdateTime;
end;

procedure TFrameAttachmentAudio.UpdateTime;
begin
  if not FIsActive then
    LabelTime.Text := SecondsToMinFormat(FDuration)
  else
    LabelTime.Text := SecondsToMinFormat(FDurationPlayed - FDuration);
end;

end.

