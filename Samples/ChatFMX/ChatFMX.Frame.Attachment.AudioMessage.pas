unit ChatFMX.Frame.Attachment.AudioMessage;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, VK.API,
  VK.Entity.AudioMessage;

type
  TFrameAttachmentAudioMessage = class(TFrame)
    LabelTime: TLabel;
    LayoutControl: TLayout;
    CircleControl: TCircle;
    PathControl: TPath;
    PaintBoxWave: TPaintBox;
    procedure PaintBoxWavePaint(Sender: TObject; Canvas: TCanvas);
    procedure CircleControlClick(Sender: TObject);
    procedure PaintBoxWaveResize(Sender: TObject);
  private
    FIsPlay: Boolean;
    FVK: TCustomVK;
    FWave: TArray<Integer>;
    FWaveMax: Integer;
    FDuration: Int64;
    FIsPause: Boolean;
    FDurationPlayed: Int64;
    FDrawWave: TArray<Single>;
    procedure SetIsPlay(const Value: Boolean);
    procedure SetWave(const Value: TArray<Integer>);
    procedure SetDuration(const Value: Int64);
    procedure SetIsPause(const Value: Boolean);
    procedure SetDurationPlayed(const Value: Int64);
    procedure UpdateTime;
    procedure CalcWave;
  public
    constructor Create(AOwner: TComponent; AVK: TCustomVK); reintroduce;
    procedure Fill(AudioMessage: TVkAudioMessage);
    property IsPlay: Boolean read FIsPlay write SetIsPlay;
    property IsPause: Boolean read FIsPause write SetIsPause;
    property Wave: TArray<Integer> read FWave write SetWave;
    property Duration: Int64 read FDuration write SetDuration;
    property DurationPlayed: Int64 read FDurationPlayed write SetDurationPlayed;
  end;

const
  PlayPath = 'm 2.5 0.5 v 9 l 7 -4.5 Z';
  PausePath = 'M224,435.8V76.1c0-6.7-5.4-12.1-12.2-12.1h-71.6c-6.8,0-12.2,5.4-12.2,12.1v359.7c0,6.7,5.4,12.2,12.2,12.2h71.6   C218.6,448,224,442.6,224,435.8z'#13#10 +
    'M371.8,64h-71.6c-6.7,0-12.2,5.4-12.2,12.1v359.7c0,6.7,5.4,12.2,12.2,12.2h71.6c6.7,0,12.2-5.4,12.2-12.2V76.1   C384,69.4,378.6,64,371.8,64z';

implementation

uses
  System.Math, HGM.Common.DateUtils;

{$R *.fmx}

procedure TFrameAttachmentAudioMessage.CircleControlClick(Sender: TObject);
begin
  IsPlay := not IsPlay;
end;

constructor TFrameAttachmentAudioMessage.Create(AOwner: TComponent; AVK: TCustomVK);
begin
  inherited Create(AOwner);
  FVK := AVK;
  Name := '';
end;

procedure TFrameAttachmentAudioMessage.Fill(AudioMessage: TVkAudioMessage);
begin
  Wave := AudioMessage.WaveForm;
  Duration := AudioMessage.Duration;
  DurationPlayed := Duration div 2;
end;

procedure TFrameAttachmentAudioMessage.PaintBoxWavePaint(Sender: TObject; Canvas: TCanvas);
begin
  if Length(FDrawWave) <= 0 then
    Exit;
  var H := PaintBoxWave.Height;
  Canvas.BeginScene;
  try
    if (DurationPlayed < Duration) and (DurationPlayed <> 0) then
    begin
      Canvas.Fill.Color := $FF577CA1;
      for var i := Low(FDrawWave) to High(FDrawWave) do
      begin
        var Item := FDrawWave[i];
        var Offset :=(H - (H * (((100 / FWaveMax) * Item) / 100))) / 2;
        Offset := Min(Offset, H / 2 - 2);
        Canvas.FillRect(TRectF.Create(i * 3, Offset, i * 3 + 2, H - Offset), 1);
      end;

      var ITo := Trunc((((100 / Duration) * DurationPlayed) / 100) * High(FDrawWave));
      Canvas.Fill.Color := $FF71AAEB;
      for var i := Low(FDrawWave) to ITo do
      begin
        var Item := FDrawWave[i];
        var Offset :=(H - (H * (((100 / FWaveMax) * Item) / 100))) / 2;
        Offset := Min(Offset, H / 2 - 2);
        Canvas.FillRect(TRectF.Create(i * 3, Offset, i * 3 + 2, H - Offset), 1);
      end;
    end
    else
    begin
      Canvas.Fill.Color := $FF71AAEB;
      for var i := Low(FDrawWave) to High(FDrawWave) do
      begin
        var Item := FDrawWave[i];
        var Offset :=(H - (H * (((100 / FWaveMax) * Item) / 100))) / 2;
        Offset := Min(Offset, H / 2 - 2);
        Canvas.FillRect(TRectF.Create(i * 3, Offset, i * 3 + 2, H - Offset), 1);
      end;
    end;
  finally
    Canvas.EndScene;
  end;
end;

procedure TFrameAttachmentAudioMessage.CalcWave;
begin
  var Cnt := Length(FWave);
  if Cnt <= 0 then
    Exit;
  var NCnt :=(Trunc(PaintBoxWave.Width) + 1) div 3;
  if NCnt <= 0 then
    Exit;
  var AvgValue := Cnt div NCnt;
  if AvgValue <= 0 then
    Exit;
  SetLength(FDrawWave, NCnt);
  for var i := Low(FDrawWave) to High(FDrawWave) do
    FDrawWave[i] := -1;
  var NIdx := -1;
  var MidCnt := -1;
  for var w := Low(FWave) to High(FWave) do
  begin
    if (w = 0) or (w mod AvgValue = 0) then
    begin
      if MidCnt <> -1 then
        FDrawWave[NIdx] := FDrawWave[NIdx] / MidCnt;
      Inc(NIdx);
      MidCnt := 0;
    end;
    if NIdx > High(FDrawWave) then
      Exit;
    Inc(MidCnt);
    if FDrawWave[NIdx] = -1 then
      FDrawWave[NIdx] := FWave[w]
    else
      FDrawWave[NIdx] := FDrawWave[NIdx] + FWave[w];
  end;
end;

procedure TFrameAttachmentAudioMessage.PaintBoxWaveResize(Sender: TObject);
begin
  CalcWave;
end;

procedure TFrameAttachmentAudioMessage.SetIsPause(const Value: Boolean);
begin
  FIsPause := Value;
end;

procedure TFrameAttachmentAudioMessage.SetIsPlay(const Value: Boolean);
begin
  FIsPlay := Value;
  if FIsPlay then
    PathControl.Data.Data := PausePath
  else
    PathControl.Data.Data := PlayPath;
end;

procedure TFrameAttachmentAudioMessage.SetDuration(const Value: Int64);
begin
  FDuration := Value;
  UpdateTime;
end;

procedure TFrameAttachmentAudioMessage.SetDurationPlayed(const Value: Int64);
begin
  FDurationPlayed := Value;
  UpdateTime;
end;

procedure TFrameAttachmentAudioMessage.UpdateTime;
begin
  if FDurationPlayed <= 0 then
    LabelTime.Text := SecondsToMinFormat(FDuration)
  else
    LabelTime.Text := SecondsToMinFormat(FDurationPlayed - FDuration);
end;

procedure TFrameAttachmentAudioMessage.SetWave(const Value: TArray<Integer>);
begin
  FWave := Value;
  FWaveMax := 1;
  for var Item in FWave do
    if Item > FWaveMax then
      FWaveMax := Item;
  CalcWave;
  PaintBoxWave.Repaint;
end;

end.

