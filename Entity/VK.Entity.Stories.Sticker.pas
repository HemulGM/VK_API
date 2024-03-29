﻿unit VK.Entity.Stories.Sticker;

interface

uses
  Generics.Collections, Rest.Json, VK.Entity.Common;

type
  TVkClickableArea = class
  private
    FX: Integer;
    FY: Integer;
  public
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

  TVkClickableSticker = class
  private
    FClickable_area: TArray<TVkClickableArea>;
    FMention: string;
    FStyle: string;
    FType: string;
  public
    property ClickableArea: TArray<TVkClickableArea> read FClickable_area write FClickable_area;
    property Mention: string read FMention write FMention;
    property Style: string read FStyle write FStyle;
    property &Type: string read FType write FType;
    destructor Destroy; override;
  end;

  TVkStoriesStickersInfo = class(TVkEntity)
  private
    FClickable_stickers: TArray<TVkClickableSticker>;
    FOriginal_height: Integer;
    FOriginal_width: Integer;
  public
    property ClickableStickers: TArray<TVkClickableSticker> read FClickable_stickers write FClickable_stickers;
    property OriginalHeight: Integer read FOriginal_height write FOriginal_height;
    property OriginalWidth: Integer read FOriginal_width write FOriginal_width;
    destructor Destroy; override;
  end;

implementation

uses
  VK.CommonUtils;

{TVkClickableSticker}

destructor TVkClickableSticker.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkClickableArea>(FClickable_area);
  inherited;
end;

{TVkStoriesStickersInfo}

destructor TVkStoriesStickersInfo.Destroy;
begin
  TArrayHelp.FreeArrayOfObject<TVkClickableSticker>(FClickable_stickers);
  inherited;
end;

end.

