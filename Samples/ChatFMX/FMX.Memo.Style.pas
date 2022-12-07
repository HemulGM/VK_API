{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Memo.Style;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.Classes, System.UITypes, System.Generics.Collections, FMX.Platform, FMX.Memo, FMX.Graphics,
  FMX.Types, FMX.Controls, FMX.TextLayout, FMX.Objects, FMX.SpellChecker, FMX.Menus, FMX.Text,
  FMX.Presentation.Messages, FMX.Presentation.Style, FMX.Controls.Presentation, FMX.ScrollBox.Style, FMX.Memo.Types,
  FMX.Controls.Model, FMX.ScrollBox;

const
  vkLCommand         = $3D;  {  61 }
  vkRCommand         = $3E;  {  62 }

type

{ TEditActionStack }

  ///<summary>Record that describes text-editing operation</summary>
  TEditAction = record
    ///<summary>Type of change that was made (text added or removed)</summary>
    ActionType: TActionType;
    ///<summary>Defines that change was made right after the previous and was made in the similar way
    ///(e.g. text editing (delete and insert) via keyabord)</summary>
    PairedWithPrev: Boolean;
    ///<summary>Position in text from which text was deleted or into which text was inserted</summary>
    StartPosition: Integer;
    ///<summary>Fragmen of text that was deleted (for TActionType.Delete only)</summary>
    DeletedFragment: string;
    ///<summary>Length of text that was inserted (for <c>TActionType.Insert</c> only)</summary>
    Length: Integer;
    ///<summary>Was text inserted via typing from keyboard or not</summary>
    Typed: Boolean;
    ///<summary>Was removed text select or not</summary>
    WasSelected: Boolean;
    ///<summary>Was caret moved after text was removed or not</summary>
    CaretMoved: Boolean;
  end;

  ///<summary>List of text-editing operations</summary>
  TEditActionStack = class(TStack<TEditAction>)
  private
    [Weak] FModel: TCustomMemoModel;
  public
    constructor Create(const AModel: TCustomMemoModel);

    ///<summary>New fragment of text was inserted</summary>
    procedure FragmentInserted(const AStartPos, AFragmentLength: Integer; const APairedWithPrev, ATyped: Boolean);
    ///<summary>Some text fragment was removed</summary>
    procedure FragmentDeleted(const AStartPos: Integer; const AFragment: string; const ASelected, ACaretMoved: Boolean);
    ///<summary>Revert last change</summary>
    function RollBackAction: Boolean;
  end;

{ Selection Controller }

  TOnSelectionChanged = procedure(Sender: TObject; const ASelStart, ALength: Integer) of object;

  TSelectionController = class
  private
    FLineSource: ITextLinesSource;
    FSelStart: TCaretPosition;
    FSelFinish: TCaretPosition;
    { Hold selection part }
    FHoldSelBegin: TCaretPosition;
    FHoldSelEnd: TCaretPosition;
    FOnChanged: TOnSelectionChanged;
    procedure SetLength(const Value: Integer);
    procedure SetStartPos(const Value: Integer);
    procedure SetSelStart(const Value: TCaretPosition);
    procedure SetSelFinish(const Value: TCaretPosition);
    function GetLength: Integer;
  protected
    procedure DoSelectionChanged; virtual;
  public
    constructor Create(const ALineSource: ITextLinesSource);
    destructor Destroy; override;

    procedure SetRange(const ASelStart, ASelEnd: TCaretPosition); overload;
    procedure SetRange(const AStartPos, ALength: Integer); overload;
    procedure HoldSelection;
    procedure UnholdSelection;

    procedure Reset;

    { Normalized selection bounds }

    function SelBegin: TCaretPosition;
    function BeginPos: Integer;
    function SelEnd: TCaretPosition;
    function EndPos: Integer;

    { Hold normalized selection bounds }

    property HoldSelBegin: TCaretPosition read FHoldSelBegin;
    property HoldSelEnd: TCaretPosition read FHoldSelEnd;

    { Unnormalized selection bounds }

    property Start: TCaretPosition read FSelStart write SetSelStart;
    property StartPos: Integer write SetStartPos;
    property Finish: TCaretPosition read FSelFinish write SetSelFinish;

    property Length: Integer read GetLength write SetLength;
    function IsSelected: Boolean;

    property OnChanged: TOnSelectionChanged read FOnChanged write FOnChanged;
  end;

{ Lines layout }

  ///<summary>Visual presentation single rendering text-line.</summary>
  TLineObject = class
  public type
    TState = (InvalidSize, InvalidPosition, InvalidLayout);
    TStates = set of TState;
  private
    FLayout: TTextLayout;
    FRect: TRectF;
    FState: TStates;
    procedure SetRect(const Value: TRectF);
    function GetSize: TSizeF;
    procedure SetSize(const Value: TSizeF);
    procedure SetLocation(const Value: TPointF);
    function GetLocation: TPointF;
    procedure SetLayout(const Value: TTextLayout);
  protected
    procedure DoRender(const ACanvas: TCanvas); virtual;
  public
    constructor Create; overload;
    destructor Destroy; override;

    procedure ReleaseLayoutIfNotVisible(const AViewportRect: TRectF);
    function IsVisible(const AViewportRect: TRectF): Boolean;
    function IsVerticallyIn(const AViewportRect: TRectF): Boolean;
    function ContainsPoint(const AHitPoint: TPointF): Boolean;

    ///<summary>Reset current size and line rectangle if line parameters were changed</summary>
    procedure InvalidateSize;
    procedure InvalidatePosition;
    procedure InvalidateLayout;
    procedure Invalidate;

    procedure Render(const ACanvas: TCanvas);
  public
    property State: TStates read FState;
    property Size: TSizeF read GetSize write SetSize;
    property Rect: TRectF read FRect write SetRect;
    property Location: TPointF read GetLocation write SetLocation;
    property Layout: TTextLayout read FLayout write SetLayout;
  end;

  ///<summary>Providing a bridge between lines of text in TMemo.Model.Lines and the internal representation</summary>
  TLinesLayout = class
  private type
    TState = (NeedAlignment, ContentSizeChanged);
    TStates = set of TState;
  private
    FLineSource: ITextLinesSource;
    FScrollableContent: IScrollableContent;
    FLineHeight: Single;
    FLines: TObjectList<TLineObject>;
    FViewportRect: TRectF;
    FContentSize: TSizeF;
    { Optimization }
    FState: TStates;
    FUpdating: Integer;
    FFirstVisibleLineIndex: Integer;
    FLastVisibleLineIndex: Integer;
    { Appearance }
    FTextSettings: TTextSettings;
    FCaretWidth: Single;
    FOpacity: Single;
    procedure SetOpacity(const Value: Single);
    procedure SetTextSettings(const Value: TTextSettings);
    function CreateLayout(const S: string): TTextLayout;
    function IsWordWrap: Boolean;
    procedure UpdateLayoutParams(const ALayout: TTextLayout);
    procedure UpdateLayoutsColor;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TLineObject;
    procedure UpdateContentSize(const AContentSize: TSizeF);
    procedure SetViewportRect(const Value: TRectF);
    { Handlers }
    procedure TextSettingsChanged(Sender: TObject);
  protected
    procedure RefreshLineLayout(const ALine: Integer);
    procedure CalculateLineSize(const ALine: Integer);
    procedure CalculateLinePosition(const ALine: Integer);

    function CreateLineLayoutIfNotCreated(const ALine: Integer): TTextLayout;
    procedure CreateLayoutsForVisibleLinesIfNotCreated;
    procedure OffsetLinesLocationFrom(const ALineIndex: Integer; const AOffset: Single);
    procedure OffsetLinesLocationBetween(const AStartLineIndex, AEndLineIndex: Integer; const AOffset: Single);
    /// <summary>Recalculates indexes of visible lines.</summary>
    procedure UpdateVisibleIndexes;
    procedure MarkInvalidatePositionFrom(const AFromIndex: Integer);

    /// <summary>Returns caret position by <c>AHitPoint</c>.</summary>
    /// <remarks>Specified line has to contain <c>AHitPoint</c>, otherwise it returns invalid caret position.</remarks>
    function GetCaretPositionByPointInLine(const ALineIndex: Integer; const AHitPoint: TPointF; const ARoundToWord: Boolean = False): TCaretPosition;
  public
    constructor Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
    destructor Destroy; override;

    { Control updating process }

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;

    { Lines changes notifications }

    procedure InsertLine(const AIndex: Integer; const ALine: string);
    procedure DeleteLine(const AIndex: Integer);
    procedure ReplaceLine(const AIndex: Integer; const ALine: string);
    procedure ExchangeLines(const AOldIndex, ANewIndex: Integer);
    procedure Clear;

    ///<summary>Returns default line height according to current text decoration settings</summary>
    function GetLineHeight: Single;

    { Caret support }

    ///<summary>Get line number and position in line by the hit point in the content coordinates system.</summary>
    function GetCaretPositionByPoint(const AHitPoint: TPointF; const RoundToWord: Boolean = False): TCaretPosition;
    ///<summary>Returns coordinates of caret point by <c>ACaretPos</c>.</summary>
    function GetPointByCaretPosition(const ACaretPosition: TCaretPosition): TPointF;
    ///<summary>Get the coordinates on the region that holds range of text starting from defined line, position in
    ///that line and the defined length.</summary>
    function GetRegionForRange(const ACaretPosition: TCaretPosition; const ALength: Integer; const RoundToWord: Boolean = False): TRegion;

    /// <summary>Recalculates and layout text lines.</summary>
    procedure Realign;
    procedure RealignIfNeeded;

    /// <summary>Renders visible lines in <c>ViewportRect</c>.</summary>
    procedure Render(const ACanvas: TCanvas);
  public
    /// <summary>Lines count. A string with all hyphens is counted as one.</summary>
    property Count: Integer read GetCount;
    /// <summary>Access to line data.</summary>
    property Items[const Index: Integer]: TLineObject read GetItem; default;
    /// <summary>Index of the first visible line in the <c>ViewportRect</c>.</summary>
    /// <remarks>Returns -1, if there are no lines. Can return invalid value, if <c>IsUpdating</c> is <c>true</c>.</remarks>
    property FirstVisibleLineIndex: Integer read FFirstVisibleLineIndex;
    /// <summary>Index of the last visible line in the <c>ViewportRect</c>.</summary>
    /// <remarks>Returns -1, if there are no lines. Can return invalid value, if <c>IsUpdating</c> is <c>true</c>.</remarks>
    property LastVisibleLineIndex: Integer read FLastVisibleLineIndex;

    property CaretWidth: Single read FCaretWidth write FCaretWidth;
    property Opacity: Single read FOpacity write SetOpacity;
    property TextSettings: TTextSettings read FTextSettings write SetTextSettings;
    property ContentSize: TSizeF read FContentSize;
    property ViewportRect: TRectF read FViewportRect write SetViewportRect;
  end;

{ Spell checker }

  ///<summary>Information about a single misspelled word in the text</summary>
  TSpellingWord = class
  private
    FPosition: TCaretPosition;
    FLength: Integer;
    FBounds: TRegion;
    FWord: string;
  public
    constructor Create(const APosition: TCaretPosition; const AWord: string; const ABounds: TRegion);
    function HasBounds: Boolean;
    function PosAtCurrentPos(const APosition: TCaretPosition): Boolean;
    procedure InvalidateBounds;
    function TextRange: TTextRange;
    property Position: TCaretPosition read FPosition write FPosition;
    property Length: Integer read FLength write FLength;
    property Bounds: TRegion read FBounds write FBounds;
    property Word: string read FWord;
  end;

  TOnReplaceWordEvent = procedure(const ASpellingWord: TSpellingWord; const ASuggestion: string) of object;

  TMemoSpellingManager = class
  private
    FLinesSource: ITextLinesSource;
    FSpellService: IFMXSpellCheckerService;
    FSpellingWords: TObjectList<TSpellingWord>;
    FMenuItems: TList<TMenuItem>;
    { Appearance }
    FHightlightRect: TRectF;
    FFill: TBrush;
    FUnderlineStroke: TStrokeBrush;
    FOnReplaceWord: TOnReplaceWordEvent;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
    procedure ClearMenuItems;
    { Handlers }
    procedure SpellFixContextMenuHandler(Sender: TObject);
  protected
    procedure DoReplaceWord(const ASpellingWord: TSpellingWord; const ASuggestion: string); virtual;
    function FindSpellingWordByCaret(const ACaretPosition: TCaretPosition; out AIndex: Integer): Boolean;
  public
    constructor Create(const ALinesSource: ITextLinesSource);
    destructor Destroy; override;

    procedure Spell(var ACaretPosition: TCaretPosition; const AWord: string);
    function IsWordWrong(const ACaretPosition: TCaretPosition): Boolean;

    { Searching Spelling words }

    procedure FindSpellingErrorsInLines;
    procedure FindSpellingErrorsInLine(const ALineIndex: Integer);
    procedure RemoveSpellingErrorsForLine(const ALineIndex: Integer);
    function GetListOfPrepositions(const ACaretPosition: TCaretPosition): TArray<string>;

    { UI }

    procedure AddSuggestionsToPopupMenu(const APopupMenu: TPopupMenu; const ACaretPosition: TCaretPosition);
    procedure Reset;
    procedure ResetBounds;
    procedure HighlightSpell(const ALines: TLinesLayout; const ACaretPosition: TCaretPosition);
    procedure HideHighlightSpell;
    procedure UpdateHightlightRect(const ALines: TLinesLayout; const AViewportSize: TSizeF; const AOffset: TPointF);

    { Drawing }

    procedure DrawHightlightSpellingWords(const ALine: TLinesLayout; const AViewportSize: TSizeF; const ACanvas: TCanvas; const AOpacity: Single);
  public
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FUnderlineStroke write SetStroke;
    property SpellingWords: TObjectList<TSpellingWord> read FSpellingWords;
    property OnReplaceWord: TOnReplaceWordEvent read FOnReplaceWord write FOnReplaceWord;
  end;

{ TStyledMemo }

  TContextAction = (Cut, Copy, Paste, Delete, Undo, SelectAll);

  TContextMenuItem = class(TMenuItem)
  private
    FContextAction: TContextAction;
  public
    property ContextAction: TContextAction read FContextAction write FContextAction;
  end;

  TAutoscrollDirection = (LeftToRight, RightToLeft, TopToBottom, BottomToTop,
                          LeftTopToRightBottom, LeftBottomToRightTop, RightTopToLeftBottom, RightBottomToLeftTop);
  TOnAutoScrollEvent = procedure(const ADirection: TAutoscrollDirection; var AStop: Boolean) of object;

  /// <summary>Automatic scroll controller. In fact, it generates a monotonous sequence of scrolling events,
  /// in which the client performs actual scrolling of the content, cursor movement, and so on. The controller supports
  /// 8 scroll directions.</summary>
  TAutoscrollController = class
  public const
    DefaultScrollStepInterval = 100; // msec
    DefaultStartScrollDelay = 100; // msec
  private
    FStartAutoScrollTimer: TTimer;
    FAutoScrollTimer: TTimer;
    FScrollDirection: TAutoscrollDirection;
    FOnScroll: TOnAutoScrollEvent;
    procedure SetStartDelay(const Value: Integer);
    function GetStartDelay: Integer;
    function GetScrollInterval: Integer;
    procedure SetScrollInterval(const Value: Integer);
    { Handlers }
    procedure StartAutoScrollHandler(Sender: TObject);
    procedure AutoScrollHandler(Sender: TObject);
  protected
    procedure DoScroll(var AStop: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Starts autoscroll on specified direction.</summary>
    procedure Start(const ADirection: TAutoscrollDirection);
    /// <summary>Stops current autoscroll.</summary>
    procedure Stop;
  public
    /// <summary>Delay in msec between invocation <c>Start</c> and the beginning of generation <c>OnScroll</c> events.</summary>
    property StartDelay: Integer read GetStartDelay write SetStartDelay;
    /// <summary>Interval in msec between invocation of <c>OnScroll</c> event.</summary>
    property ScrollInterval: Integer read GetScrollInterval write SetScrollInterval;
    /// <summary>The event is called when developer need to scroll.</summary>
    property OnScroll: TOnAutoScrollEvent read FOnScroll write FOnScroll;
  end;

  TStyledMemo = class(TStyledCustomScrollBox, ITextInput, ITextSpellCheck, ITextSpellCheckActions)
  public const
    DefaultEmptySelectionWidth = 5;
    IMEWindowGap = 2; // Small space between conrol and IME window
    MarkedTextBackgroundOpacity = 0.9;
  protected type
    TSelectionMethod = (Keyboard, Mouse);
    TSelectionMethods = set of TSelectionMethod;
    TSelectionOption = (SelectWords);
    TSelectionOptions = set of TSelectionOption;
    TScrollDirection = (Up, Down);
  private
    FTextService: TTextService;
    FCaretPosition: TCaretPosition;
    FMemoPopupMenu: TPopupMenu;
    FActionStack: TEditActionStack;
    FLinesLayout: TLinesLayout;
    FCharsBuffer: string;
    FSetFocusOnUp: Boolean;
    FAutoscrollController: TAutoscrollController;
    { Selection }
    FSelectionController: TSelectionController;
    FSelectionMethods: TSelectionMethods;
    FSelectionOptions: TSelectionOptions;
    { Spelling }
    FSpellingManager: TMemoSpellingManager;
    { IME }
    FImeLayout: TTextLayout;
    function GetModel: TCustomMemoModel;
    function GetMemo: TCustomMemo;
    procedure SetCaretPosition(const Value: TCaretPosition);
    function GetPageSize: Single;
    { Handlers }
    procedure ReplaceWordHandler(const ASpellingWord: TSpellingWord; const ASuggestion: string);
    procedure SelectionChangedHandler(Sender: TObject; const ASelStart, ALength: Integer);
    procedure ContextMenuItemClick(Sender: TObject);
    procedure AutoScrollHandler(const ADirection: TAutoscrollDirection; var AStop: Boolean);
    procedure ContentGetClipRectHandler(Sender: TObject; var AClipRect: TRectF);
    function ConvertLocalPointFrom(const AControl: TControl;
      const AControlLocalPoint: TPointF): TPointF;
    function ConvertLocalPointTo(const AControl: TControl;
      const ALocalPoint: TPointF): TPointF;
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure UpdateScrollbarsPosition;                                        
    procedure UpdateCaretPosition;

    { Messages from model }
    procedure MMCharCaseChanged(var Message: TDispatchMessageWithValue<TEditCharCase>); message MM_MEMO_CHARCASE_CHANGED;
    procedure MMCheckSpellingChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_CHECKSPELLING_CHANGED;
    procedure MMHideSelectionOnExitChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_HIDESELECTIONONEXIT_CHANGED;
    procedure MMReadOnlyChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_READONLY_CHANGED;
    procedure MMImeModeChanged(var Message: TDispatchMessageWithValue<TImeMode>); message MM_MEMO_IMEMODE_CHANGED;
    procedure MMSetSelStart(var Message: TDispatchMessageWithValue<Integer>); message MM_MEMO_SELSTART_CHANGED;
    procedure MMSelLengthChanged(var Message: TDispatchMessageWithValue<Integer>); message MM_MEMO_SELLENGTH_CHANGED;
    procedure MMTextSettingsChanged(var Message: TDispatchMessage); message MM_MEMO_TEXT_SETTINGS_CHANGED;
    procedure MMLinesInsertLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_INSERT_LINE;
    procedure MMLinesReplaceLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_PUT_LINE;
    procedure MMLinesDeleteLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_DELETE_LINE;
    procedure MMLinesExchangeLines(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_EXCHANGE_LINES;
    procedure MMLinesClear(var Message: TDispatchMessage); message MM_MEMO_LINES_CLEAR;
    procedure MMUpdateStateChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_UPDATE_STATE_CHANGED;
    procedure MMGetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>); message MM_MEMO_GET_CARET_POSITION;
    procedure MMSetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>); message MM_MEMO_SET_CARET_POSITION;
    procedure MMCanSetFocus(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_CAN_SET_FOCUS;
    procedure MMLinesChanged(var Message: TDispatchMessage); message MM_MEMO_LINES_CHANGED;
    procedure MMMaxLengthChanged(var Message: TDispatchMessage); message MM_MEMO_MAXLENGTH_CHANGED;
    procedure MMGetCaretPositionByPoint(var Message: TDispatchMessageWithValue<TCustomMemoModel.TGetCaretPositionInfo>); message MM_MEMO_GET_CARET_POSITION_BY_POINT;

    { Messages from presented control }
    procedure PMInit(var Message: TDispatchMessage); message PM_INIT;
    procedure PMGotoLineBegin(var Message: TDispatchMessage); message PM_MEMO_GOTO_LINE_BEGIN;
    procedure PMGotoLineEnd(var Message: TDispatchMessage); message PM_MEMO_GOTO_LINE_END;
    procedure PMFragmentInserted(var Message: TDispatchMessageWithValue<TFragmentInserted>); message PM_MEMO_UNDO_MANAGER_INSERT_TEXT;
    procedure PMFragmentDeleted(var Message: TDispatchMessageWithValue<TFragmentDeleted>); message PM_MEMO_UNDO_MANAGER_DELETE_TEXT;
    procedure PMUndo(var Message: TDispatchMessage); message PM_MEMO_UNDO_MANAGER_UNDO;
    procedure PMSelectText(var Message: TDispatchMessage); message PM_MEMO_SELECT_TEXT;
    procedure PMRootChanged(var Message: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;

    { ITextInput }
    function GetTextService: TTextService;
    function GetTargetClausePointF: TPointF;
    procedure StartIMEInput;
    procedure EndIMEInput;
    procedure IMEStateUpdated;
    function GetSelection: string;
    function GetSelectionRect: TRectF;
    function GetSelectionBounds: TRect;
    function GetSelectionPointSize: TSizeF;
    function HasText: Boolean;

    { Rendering }
    procedure ContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
    procedure DrawSelection(const ACanvas: TCanvas); virtual;
    procedure DrawMarkedText(const ACanvas: TCanvas; const ARect: TRectF); virtual;

    { Selections }
    /// <summary>Returns selection region in the <c>Content</c> coordinate system.</summary>
    /// <remarks>It works slowly on a large number of lines of text. If you need to get a selection only within
    /// the visible area, use the method <c>GetVisibleSelectionRegion</c>.</remarks>
    function GetSelectionRegion: TRegion;
    /// <summary>Returns visible selection region in the <c>Content</c> coordinate system.</summary>
    function GetVisibleSelectionRegion: TRegion;
    function IsSelecting: Boolean;
    function NeedShowSelection: Boolean; virtual;

    { Autoscroll }
    procedure StartAutoScroll(const ALocalPoint: TPointF);

    { Caret }
    /// <summary>The carret position may become outdated and not fall within the text boundaries.
    /// This method allows to normalize the carriage position within the text.</summary>
    procedure NormalizeCaretPosition; overload;
    function NormalizeCaretPosition(const Value: TCaretPosition): TCaretPosition; overload;

    { ITextSpellCheck }
    function IsSpellCheckEnabled: Boolean;
    function IsCurrentWordWrong: Boolean;
    function GetListOfPrepositions: TArray<string>;
    procedure HighlightSpell;
    procedure HideHighlightSpell;

    { ITextSpellCheckActions }
    procedure Spell(const AWord: string);

    { Popup menu }
    procedure ExecuteContextAction(const AAction: TContextAction); virtual;
    procedure FillPopupMenu(const AMenu: TPopupMenu); virtual;
    ///<summary>Updates the current state of popup menu items.</summary>
    procedure UpdatePopupMenuItems(const AMenu: TPopupMenu); virtual;

    { Mouse events }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); override;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); override;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); override;

    { Gestures }
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure LongTap(const ALocalPoint: TPointF; const AFlags: TInteractiveGestureFlags);
    procedure DblTap;

    { IME }
    function HasImeMarkedText: Boolean;
    procedure UpdateTextInTextService;
    /// <summary>Specifies whether to hide the caret during IME typing?</summary>
    function NeedHideCaretInIME: Boolean; virtual;

    { inherited }
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean; override;
    procedure DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Resize; override;
    procedure DoChange; virtual;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecalcOpacity; override;

    /// <summary>Rollback the latest changes.</summary>
    procedure Undo;
    ///<summary>Repainting content in memo.</summary>
    procedure RepaintEdit;

    { Caret navigation }
    procedure GotoLineBegin;
    procedure GotoLineEnd;
    procedure MoveCaretVertical(const ALineDelta: Integer);
    procedure MoveCaretPageUp;
    procedure MoveCaretPageDown;
    /// <summary>Returns caret position in <c>Content</c> coordinate system.</summary>
    function GetCaretPositionPoint(const ACaretPos: TCaretPosition): TPointF;
    procedure PutCaretTo(const X, Y: Single; const APositionByWord: Boolean = False);

    { Viewport }
    procedure ScrollOnLine(const ADirection: TScrollDirection);
    function ViewportRect: TRectF;
  public
    property Model: TCustomMemoModel read GetModel;
    property Memo: TCustomMemo read GetMemo;
    property LinesLayout: TLinesLayout read FLinesLayout;
    property AutoscrollController: TAutoscrollController read FAutoscrollController;
    property SelectionController: TSelectionController read FSelectionController;
    ///<summary>Current caret position in text.</summary>
    property CaretPosition: TCaretPosition read FCaretPosition write SetCaretPosition;
    /// <summary>Average count of visible lines.</summary>
    property PageSize: Single read GetPageSize;
  end;

implementation

uses
  System.SysUtils, System.RTLConsts, System.Variants, System.Math, System.UIConsts, System.Character, System.Math.Vectors,
  FMX.Presentation.Factory, FMX.Utils, FMX.Consts, FMX.Clipboard;

const
  CutStyleName = 'cut'; //Do not localize
  UndoStyleName = 'undo'; //Do not localize
  CopyStyleName = 'copy'; //Do not localize
  PasteStyleName = 'paste'; //Do not localize
  DeleteStyleName = 'delete'; //Do not localize
  SelectAllStyleName = 'selectall'; //Do not localize

function RectsIntersect(const R1, R2: TRectF): Boolean;
begin
  Result := (R1.Left <= R2.Right) and (R1.Right >= R2.Left) and (R1.Top <= R2.Bottom) and (R1.Bottom >= R2.Top);
end;

type
  TMemoFields = class
  public
    NeedUpdateContentOffset: Boolean;
  end;

  TStyledMemoHelper = class
  private
    class var FCurrent: TStyledMemoHelper;
    class function GetCurrent: TStyledMemoHelper; static;
    class procedure DestroyCurrent;
  private
    FInstances: TObjectDictionary<TStyledMemo, TMemoFields>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Register(const AMemo: TStyledMemo);
    procedure Unregister(const AMemo: TStyledMemo);

    function NeedUpdateContentOffset(const AMemo: TStyledMemo): Boolean;
    procedure SetNeedUpdateContentOffset(const AMemo: TStyledMemo; const AValue: Boolean);

    class property Current: TStyledMemoHelper read GetCurrent;
  end;

{ TStyledMemoEx }

constructor TStyledMemoHelper.Create;
begin
  FInstances := TObjectDictionary<TStyledMemo, TMemoFields>.Create([doOwnsValues]);
end;

destructor TStyledMemoHelper.Destroy;
begin
  FreeAndNil(FInstances);
end;

class procedure TStyledMemoHelper.DestroyCurrent;
begin
  FreeAndNil(FCurrent);
end;

class function TStyledMemoHelper.GetCurrent: TStyledMemoHelper;
begin
  if FCurrent = nil then
    FCurrent := TStyledMemoHelper.Create;

  Result := FCurrent;
end;

function TStyledMemoHelper.NeedUpdateContentOffset(const AMemo: TStyledMemo): Boolean;
var
  Fields: TMemoFields;
begin
  if FInstances.TryGetValue(AMemo, Fields) then
    Result := Fields.NeedUpdateContentOffset
  else
    Result := True;
end;

procedure TStyledMemoHelper.Register(const AMemo: TStyledMemo);
begin
  FInstances.Add(AMemo, TMemoFields.Create);
end;

procedure TStyledMemoHelper.SetNeedUpdateContentOffset(const AMemo: TStyledMemo; const AValue: Boolean);
var
  Fields: TMemoFields;
begin
  if FInstances.TryGetValue(AMemo, Fields) then
    Fields.NeedUpdateContentOffset := AValue;
end;

procedure TStyledMemoHelper.Unregister(const AMemo: TStyledMemo);
begin
  FInstances.Remove(AMemo);

  if FInstances.Count = 0 then
    DestroyCurrent;
end;

{ TStyledMemo }

function TStyledMemo.GetMemo: TCustomMemo;
begin
  Result := PresentedControl as TCustomMemo;
end;

function TStyledMemo.GetModel: TCustomMemoModel;
begin
  Result := inherited GetModel<TCustomMemoModel>;
end;

constructor TStyledMemo.Create(AOwner: TComponent);
var
  PlatformTextService: IFMXTextService;
begin
  inherited;
  EnableExecuteAction := False;
  CanFocus := False;
  AutoCapture := True;
  SetAcceptsControls(False);
  FLinesLayout := TLinesLayout.Create(Model, Self);
  FSelectionController := TSelectionController.Create(Model);
  FSelectionController.OnChanged := SelectionChangedHandler;

  FMemoPopupMenu := TPopupMenu.Create(Self);
  FMemoPopupMenu.Stored := False;
  FMemoPopupMenu.PopupComponent := Self;
  FillPopupMenu(FMemoPopupMenu);
  FMemoPopupMenu.AddFreeNotify(Self);

  FActionStack := TEditActionStack.Create(Model);
  FCaretPosition := TCaretPosition.Zero;

  if FTextService <> nil then
    FTextService.ImeMode := TImeMode.imDontCare;

  FAutoscrollController := TAutoscrollController.Create;
  FAutoscrollController.OnScroll := AutoScrollHandler;

  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.DoubleTap, TInteractiveGesture.LongTap];

  FSpellingManager := TMemoSpellingManager.Create(Model);
  FSpellingManager.OnReplaceWord := ReplaceWordHandler;

  FImeLayout := TTextLayoutManager.DefaultTextLayout.Create;
  TStyledMemoHelper.Current.Register(Self);

  if TPlatformServices.Current.SupportsPlatformService(IFMXTextService, PlatformTextService) then
    FTextService := PlatformTextService.GetTextServiceClass.Create(Self, True);
end;

function TStyledMemo.DefineModelClass: TDataModelClass;
begin
  Result := TCustomMemoModel;
end;

destructor TStyledMemo.Destroy;
begin
  TStyledMemoHelper.Current.Unregister(Self);
  Content.OnPainting := nil;
  Content.OnGetClipRect := nil;
  FreeAndNil(FAutoscrollController);
  FreeAndNil(FImeLayout);
  FreeAndNil(FSpellingManager);
  FreeAndNil(FActionStack);
  FreeAndNil(FMemoPopupMenu);
  FreeAndNil(FSelectionController);
  FreeAndNil(FLinesLayout);
  FreeAndNil(FTextService);
  inherited;
end;

procedure TStyledMemo.HideHighlightSpell;
begin
  FSpellingManager.HideHighlightSpell;
  RepaintEdit;
end;

procedure TStyledMemo.DoEndUpdate;

  function IsLoading: Boolean;
  begin
    Result := csLoading in PresentedControl.ComponentState;
  end;

  function IsDestroying: Boolean;
  begin
    Result := csDestroying in PresentedControl.ComponentState;
  end;

begin
  inherited;
  FLinesLayout.EndUpdate;
  if not (IsUpdating or IsLoading or IsDestroying) then
  begin
    UpdateCaretPosition;
    if TStyledMemoHelper.Current.NeedUpdateContentOffset(Self) then
      UpdateScrollbarsPosition;
    RepaintEdit;
  end;
end;

procedure TStyledMemo.DoEnter;
begin
  inherited;
  UpdateTextInTextService;
  UpdateCaretPosition;
  if Model.AutoSelect then
    Memo.SelectAll;
end;

procedure TStyledMemo.DoExit;
begin
  DoChange;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    TLinkObservers.EditLinkUpdate(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueUpdate(Observers);
  inherited;
end;

procedure TStyledMemo.GotoLineBegin;
var
  Point: TPointF;
begin
  if HasText then
  begin
    Point := TPointF.Create(0, Model.Caret.Pos.Y + FLinesLayout.GetLineHeight / 2);
    CaretPosition := FLinesLayout.GetCaretPositionByPoint(Point);
  end;
end;

procedure TStyledMemo.GotoLineEnd;
var
  Point: TPointF;
begin
  if HasText then
  begin
    Point := TPointF.Create(FLinesLayout[CaretPosition.Line].Rect.Right - 1,
                            Model.Caret.Pos.Y + FLinesLayout.GetLineHeight / 2);
    CaretPosition := FLinesLayout.GetCaretPositionByPoint(Point);
  end;
end;

function TStyledMemo.GetCaretPositionPoint(const ACaretPos: TCaretPosition): TPointF;
begin
  Result := FLinesLayout.GetPointByCaretPosition(ACaretPos);
end;

procedure TStyledMemo.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);

  function GetLeftWordBegin(const APosition: TCaretPosition): TCaretPosition;
  var
    CurrentLine: string;
    LLines: TStrings;
  begin
    LLines := Model.Lines;
    if LLines.Count = 0 then
      Exit(APosition);

    Result.Pos := APosition.Pos;
    Result.Line := APosition.Line;
    CurrentLine := LLines[Result.Line];

    if APosition.Pos > 0 then
    begin
      Result.Pos := GetLexemeBegin(CurrentLine, APosition.Pos);
      // If cursor is placed in the beginning of word, we have to take beginning pos of previous word.
      if Result.Pos = APosition.Pos then
        Result.Pos := GetPrevLexemeBegin(CurrentLine, APosition.Pos);
    end
    else if (APosition.Line - 1 >= 0) and (APosition.Line - 1 <= LLines.Count - 1) then
    begin
      Result.Line := APosition.Line - 1;
      Result.Pos := CurrentLine.Length;
    end;
  end;

  procedure DeleteTextByBackspace(const AIsCtrlOrCmd: Boolean);
  var
    DeleteLength: Integer;
    LCaret: TCaretPosition;
  begin
    if Model.ReadOnly then
      Exit;

    if Model.SelLength <> 0 then
      Memo.DeleteSelection
    else if AIsCtrlOrCmd then
    begin
      // Deleting whole word
      LCaret := GetLeftWordBegin(CaretPosition);
      if LCaret.IsInvalid then
        Exit;
      Model.DeleteFrom(LCaret, Model.PosToTextPos(CaretPosition) - Model.PosToTextPos(LCaret),
        [TDeleteOption.MoveCaret, TDeleteOption.CanUndo]);
    end
    // Deleting single character
    else if Model.PosToTextPos(CaretPosition) > 0 then
    begin
      if (Model.Lines[CaretPosition.Line].Length > 0) and
        Model.Lines[CaretPosition.Line].Chars[CaretPosition.Pos - 1].IsLowSurrogate then
        Model.DeleteFrom(Model.GetPositionShift(CaretPosition, -2), 2,
          [TDeleteOption.MoveCaret, TDeleteOption.CanUndo])
      else
      begin
        if CaretPosition.Pos = 0 then
          DeleteLength := Model.Lines.LineBreak.Length
        else
          DeleteLength := 1;

         Model.DeleteFrom(Model.GetPositionShift(CaretPosition, -1), DeleteLength, [TDeleteOption.MoveCaret, TDeleteOption.CanUndo]);
      end;
    end;
  end;

  procedure DeleteTextByDel(const AIsCtrlOrCmd: Boolean);
  begin
    if Model.ReadOnly then
      Exit;

    if Model.SelLength <> 0 then
    begin
      if ssShift in Shift then
        Memo.CutToClipboard
      else
        Memo.DeleteSelection;
    end
    else if AIsCtrlOrCmd then
      Model.DeleteFrom(CaretPosition, Min(FMX.Text.GetLexemeEnd(Model.Lines[CaretPosition.Line],
        CaretPosition.Pos), Model.Lines[CaretPosition.Line].Length) - CaretPosition.Pos + 1,
        [TDeleteOption.CanUndo])
    else if HasText then
    begin
      if (CaretPosition.Pos < Model.Lines[CaretPosition.Line].Length) and
        Model.Lines[CaretPosition.Line].Chars[CaretPosition.Pos].IsHighSurrogate then
        Model.DeleteFrom(CaretPosition, 2, [TDeleteOption.CanUndo])
      else if CaretPosition.Pos = Model.Lines[CaretPosition.Line].Length then
        // We are in the end of line, So we have to remove line break
        Model.DeleteFrom(CaretPosition, Model.Lines.LineBreak.Length, [TDeleteOption.CanUndo])
      else
        Model.DeleteFrom(CaretPosition, 1, [TDeleteOption.CanUndo]);
    end;
  end;

var
  TmpS: string;
  IsCtrlOrCmd: Boolean;
  LTmpOptions: TInsertOptions;
  KeyHandled: Boolean;
  IgnoreResetSelection: Boolean;
begin
  KeyHandled := False;
  IgnoreResetSelection := False;

  IsCtrlOrCmd := Shift * [ssCtrl, ssCommand] <> [];

  // Shift key can be used for pressing Uppercase characters, In this case we don't need to consider this case as selection.
  if (ssShift in Shift) and (KeyChar = #0) then
    Include(FSelectionMethods, TSelectionMethod.Keyboard);

  if IsCtrlOrCmd and (Key in [vkControl, vkUp, vkDown, vkC, vkLCommand, vkRCommand]) then
    IgnoreResetSelection := True;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    if (Key in [vkReturn, vkBack, vkDelete]) or ((Key = vkInsert) and (ssShift in Shift)) then
      if TLinkObservers.EditLinkEdit(Observers) then
        TLinkObservers.EditLinkModified(Observers)
      else
      begin
        TLinkObservers.EditLinkReset(Observers);
        Exit;
      end;

    if (KeyChar >= #32) and not TLinkObservers.EditLinkIsValidChar(Observers, KeyChar) then
    begin
      KeyChar := #0;
      Exit;
    end;
    case KeyChar of
      ^H, ^V, ^X, #32 .. High(Char):
        if TLinkObservers.EditLinkEdit(Observers) then
          TLinkObservers.EditLinkModified(Observers)
        else
        begin
          KeyChar := #0;
          TLinkObservers.EditLinkReset(Observers);
          Exit;
        end;
      #27:
        begin
          TLinkObservers.EditLinkReset(Observers);
          Memo.SelectAll;
          KeyChar := #0;
          Exit;
        end;
    end;
  end;

  if Observers.IsObserving(TObserverMapping.ControlValueID) and (KeyChar <> #0) then
    TLinkObservers.ControlValueModified(Observers);

  inherited KeyDown(Key, KeyChar, Shift);

  // We don't process any combination with Alt key.
  if ssAlt in Shift then
    Exit;

  if (Key = vkReturn) and not (ssCommand in Shift) and not Model.ReadOnly then
  begin
    if Model.SelLength > 0 then
    begin
      Model.DeleteFrom(FSelectionController.SelBegin, Model.SelLength, [TDeleteOption.MoveCaret, TDeleteOption.CanUndo,
                       TDeleteOption.Selected]);
      LTmpOptions := [TInsertOption.UndoPairedWithPrev];
    end
    else
      LTmpOptions := [];
    TmpS := Model.Lines.LineBreak;
    Model.InsertAfter(CaretPosition, TmpS, LTmpOptions + [TInsertOption.MoveCaret, TInsertOption.CanUndo]);
    Model.SelLength := 0;
    Key := 0;
    DoChange;
  end;
  case Key of
    vkA:
      if IsCtrlOrCmd then
      begin
        Memo.SelectAll;
        IgnoreResetSelection := True;
        KeyHandled := True;
      end;
    vkC:
      if IsCtrlOrCmd then
      begin
        Memo.CopyToClipboard;
        KeyHandled := True;
      end;
    vkV:
      if IsCtrlOrCmd then
      begin
        Memo.PasteFromClipboard;
        KeyHandled := True;
      end;
    vkX:
      if IsCtrlOrCmd and not Model.ReadOnly then
      begin
        Memo.CutToClipboard;
        KeyHandled := True;
      end;
    vkZ:
      if IsCtrlOrCmd then
      begin
        Undo;
        KeyHandled := True;
      end;
    vkEnd:
      begin
        if IsCtrlOrCmd then
          TCustomMemo(PresentedControl).GoToTextEnd
        else
          GotoLineEnd;
        KeyHandled := True;
      end;
    vkHome:
      begin
        if IsCtrlOrCmd then
          TCustomMemo(PresentedControl).GoToTextBegin
        else
          GotoLineBegin;
        KeyHandled := True;
      end;
    vkLeft:
      begin
        if IsCtrlOrCmd then
          CaretPosition := GetLeftWordBegin(CaretPosition)
        else
          Model.MoveCaretLeft;
        KeyHandled := True;
      end;
    vkRight:
      begin
        if IsCtrlOrCmd then
          CaretPosition := Model.GetNextWordBegin(CaretPosition)
        else
          Model.MoveCaretRight;
        KeyHandled := True;
      end;
    vkUp:
      begin
        if IsCtrlOrCmd then
          ScrollOnLine(TScrollDirection.Up)
        else
          MoveCaretVertical(-1);
        KeyHandled := True;
      end;
    vkDown:
      begin
        if IsCtrlOrCmd then
          ScrollOnLine(TScrollDirection.Down)
        else
          MoveCaretVertical(1);
        KeyHandled := True;
      end;
    vkPrior:
      begin
        MoveCaretPageUp;
        KeyHandled := True;
      end;
    vkNext:
      begin
        MoveCaretPageDown;
        KeyHandled := True;
      end;
    vkDelete:
      begin
        DeleteTextByDel(IsCtrlOrCmd);
        KeyHandled := True;
      end;
    vkBack:
      begin
        DeleteTextByBackspace(IsCtrlOrCmd);
        KeyHandled := True;
      end;
    vkInsert:
      if IsCtrlOrCmd then
      begin
        Memo.CopyToClipboard;
        KeyHandled := True;
      end
      else if [ssShift] * Shift <> [] then
      begin
        Memo.PasteFromClipboard;
        KeyHandled := True;
      end;
    vkProcessKey:
      IgnoreResetSelection := True;
  end;

  if (KeyChar <> #0) and not Model.ReadOnly then
  begin
    FCharsBuffer := FCharsBuffer + KeyChar;
    if not KeyChar.IsHighSurrogate then
    begin
      if Model.SelLength > 0 then
      begin
        Model.DeleteFrom(FSelectionController.SelBegin, Model.SelLength, [TDeleteOption.MoveCaret, TDeleteOption.CanUndo]);
        LTmpOptions := [TInsertOption.UndoPairedWithPrev];
      end
      else
        LTmpOptions := [];
      Model.InsertAfter(CaretPosition, FCharsBuffer, LTmpOptions + [TInsertOption.MoveCaret, TInsertOption.CanUndo,
                        TInsertOption.Typed]);
      FCharsBuffer := string.Empty;
      Model.SelLength := 0;
    end;
    KeyHandled := True;
  end
  else
  begin
    FCharsBuffer := string.Empty;
    if Key in [vkEnd, vkHome, vkLeft, vkRight, vkUp, vkDown, vkPrior, vkNext] then
    begin
      if IsSelecting then
        FSelectionController.Finish := CaretPosition;

      RepaintEdit;
      KeyHandled := True;
    end;
  end;
  if not IsSelecting and not IgnoreResetSelection then
    FSelectionController.SetRange(CaretPosition, CaretPosition);

  if KeyHandled then
  begin
    Key := 0;
    KeyChar := #0;
  end;

  Exclude(FSelectionMethods, TSelectionMethod.Keyboard);
end;

procedure TStyledMemo.KeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  Exclude(FSelectionMethods, TSelectionMethod.Keyboard);
end;

procedure TStyledMemo.LongTap(const ALocalPoint: TPointF; const AFlags: TInteractiveGestureFlags);
begin
  SelectionController.UnholdSelection;
  Exclude(FSelectionMethods, TSelectionMethod.Mouse);

  if not (csDesigning in ComponentState) and not PresentedControl.IsFocused then
    PresentedControl.SetFocus;

  if TInteractiveGestureFlag.gfEnd in AFlags then
    ShowContextMenu(LocalToScreen(ALocalPoint));
end;

procedure TStyledMemo.MMCanSetFocus(var Message: TDispatchMessageWithValue<Boolean>);
begin
  Message.Value := not FSetFocusOnUp;
end;

procedure TStyledMemo.MMCharCaseChanged(var Message: TDispatchMessageWithValue<TEditCharCase>);
begin
  if FTextService <> nil then
  begin
    UpdateTextInTextService;
    FTextService.CharCase := Model.CharCase;
  end;
  RepaintEdit;
end;

procedure TStyledMemo.MMCheckSpellingChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  if Message.Value then
    FSpellingManager.FindSpellingErrorsInLines
  else
    FSpellingManager.Reset;
  RepaintEdit;
end;

procedure TStyledMemo.PMFragmentDeleted(var Message: TDispatchMessageWithValue<TFragmentDeleted>);
begin
  FActionStack.FragmentDeleted(Message.Value.StartPos, Message.Value.Fragment, Message.Value.Selected,
    Message.Value.CaretMoved);
end;

procedure TStyledMemo.PMFragmentInserted(var Message: TDispatchMessageWithValue<TFragmentInserted>);
begin
  FActionStack.FragmentInserted(Message.Value.StartPos, Message.Value.FragmentLength, Message.Value.PairedWithPrev,
    Message.Value.Typed);
end;

procedure TStyledMemo.MMGetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>);
begin
  Message.Value := CaretPosition;
end;

procedure TStyledMemo.MMGetCaretPositionByPoint(var Message: TDispatchMessageWithValue<TCustomMemoModel.TGetCaretPositionInfo>);
begin
  Message.Value.CaretPosition := FLinesLayout.GetCaretPositionByPoint(Message.Value.HitPoint + ViewportPosition, Message.Value.RoundToWord);
end;

procedure TStyledMemo.PMGotoLineBegin(var Message: TDispatchMessage);
begin
  GotoLineBegin;
end;

procedure TStyledMemo.PMGotoLineEnd(var Message: TDispatchMessage);
begin
  GotoLineEnd;
end;

procedure TStyledMemo.MMHideSelectionOnExitChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  RepaintEdit;
end;

procedure TStyledMemo.MMImeModeChanged(var Message: TDispatchMessageWithValue<TImeMode>);
begin
  if (FTextService <> nil) and (FTextService.ImeMode <> Message.Value) then
    FTextService.ImeMode := Message.Value;
end;

procedure TStyledMemo.MMLinesChanged(var Message: TDispatchMessage);
begin
end;

procedure TStyledMemo.MMLinesClear(var Message: TDispatchMessage);
begin
  FLinesLayout.Clear;

  CaretPosition := TCaretPosition.Zero;
  FSelectionController.Reset;
  FSpellingManager.Reset;
  UpdateCaretPosition;
  UpdateScrollbarsPosition;
  RepaintEdit;
end;

procedure TStyledMemo.MMLinesDeleteLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);

  procedure AdjustCaretPosition;
  var
    NewCaretPosition: TCaretPosition;
  begin
    if CaretPosition.Line >= Model.Lines.Count then
      // Old caret line is out of lines count.
      NewCaretPosition.Line := CaretPosition.Line - 1
    else
      // Trying to keep current line.
      NewCaretPosition.Line := CaretPosition.Line;
    NewCaretPosition.Pos := CaretPosition.Pos;
    CaretPosition := NormalizeCaretPosition(NewCaretPosition);
  end;

begin
  FLinesLayout.DeleteLine(Message.Value.Index);
  if CaretPosition.Line >= Message.Value.Index then
    // We are trying to save the position of the caret in the following lines.
    AdjustCaretPosition;
  SelectionController.SetRange(CaretPosition, CaretPosition);
  if Model.CheckSpelling then
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.Index);
end;

procedure TStyledMemo.MMLinesExchangeLines(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);
begin
  if Model.CheckSpelling then
  begin
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.Index);
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.ExtraIndex);
  end;
  FLinesLayout.ExchangeLines(Message.Value.Index, Message.Value.ExtraIndex);
  if Model.CheckSpelling then
  begin
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.Index);
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.ExtraIndex);
  end;

  if not IsUpdating then
    RepaintEdit;
end;

procedure TStyledMemo.MMLinesInsertLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);

  procedure AdjustCaretPosition;
  var
    NewCaretPosition: TCaretPosition;
  begin
    NewCaretPosition.Line := CaretPosition.Line + 1;
    NewCaretPosition.Pos := CaretPosition.Pos;
    CaretPosition := NormalizeCaretPosition(NewCaretPosition);
  end;

begin
  FLinesLayout.InsertLine(Message.Value.Index, Message.Value.Text);
  if CaretPosition.Line >= Message.Value.Index then
    // We are trying to save the position of the carret in the following lines.
    AdjustCaretPosition;
  if Model.CheckSpelling then
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.Index);
end;

procedure TStyledMemo.MMLinesReplaceLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);
begin
  FLinesLayout.ReplaceLine(Message.Value.Index, Message.Value.Text);
  NormalizeCaretPosition;
  SelectionController.SetRange(CaretPosition, CaretPosition);
  if Model.CheckSpelling then
  begin
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.Index);
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.Index);
  end;
end;

procedure TStyledMemo.MMMaxLengthChanged(var Message: TDispatchMessage);
begin
  if FTextService <> nil then
    FTextService.MaxLength := Model.MaxLength;
end;

procedure TStyledMemo.MMReadOnlyChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  Model.Caret.ReadOnly := Message.Value;
end;

procedure TStyledMemo.MMSelLengthChanged(var Message: TDispatchMessageWithValue<Integer>);
begin
  FSelectionController.SetRange(Model.SelStart, Message.Value);
end;

procedure TStyledMemo.MMSetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>);
begin
  CaretPosition := Message.Value;
end;

procedure TStyledMemo.MMSetSelStart(var Message: TDispatchMessageWithValue<Integer>);
begin
  FSelectionController.SetRange(Message.Value, 0);
  CaretPosition := FSelectionController.SelBegin;
end;

procedure TStyledMemo.MMTextSettingsChanged(var Message: TDispatchMessage);
var
  TextSettings: TTextSettings;
begin
  TextSettings := Model.TextSettingsInfo.ResultingTextSettings;
  FLinesLayout.TextSettings := TextSettings;

  FImeLayout.BeginUpdate;
  try
    FImeLayout.WordWrap := FLinesLayout.IsWordWrap;
    FImeLayout.Font := TextSettings.Font;
    FImeLayout.Color := TextSettings.FontColor;
    FImeLayout.Opacity := Opacity;
    FImeLayout.RightToLeft := TFillTextFlag.RightToLeft in FillTextFlags;
  finally
    FImeLayout.EndUpdate;
  end;

  if not (csLoading in ComponentState) then
  begin
    if FLinesLayout.TextSettings.WordWrap then
      AniCalculations.TouchTracking := AniCalculations.TouchTracking - [ttHorizontal]
    else
      AniCalculations.TouchTracking := AniCalculations.TouchTracking + [ttHorizontal];

    UpdateCaretPosition;
  end;
  RepaintEdit;
end;

procedure TStyledMemo.PMUndo(var Message: TDispatchMessage);
begin
  Undo;
end;

procedure TStyledMemo.MMUpdateStateChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  if Message.Value then
    BeginUpdate
  else
    EndUpdate;
end;

procedure TStyledMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FSetFocusOnUp := ([ssDouble, ssTouch] * Shift) = [ssTouch];
  inherited;
  if Button <> TMouseButton.mbLeft then
    Exit;

  if ssDouble in Shift then
  begin
    Memo.SelectWord;
    // User uses double selection mode, we have to hold selection of one word and don't allow to reset
    // this selection word until selection is not finished.
    SelectionController.HoldSelection;
    Include(FSelectionOptions, TSelectionOption.SelectWords);
  end
  else
  begin
    PutCaretTo(X, Y);
    if IsSelecting then
      FSelectionController.Finish := FCaretPosition
    else
      FSelectionController.SetRange(FCaretPosition, FCaretPosition);
  end;
  Include(FSelectionMethods, TSelectionMethod.Mouse);
end;

procedure TStyledMemo.ContentGetClipRectHandler(Sender: TObject; var AClipRect: TRectF);
begin
  AClipRect := LinesLayout.ViewportRect;
end;

procedure TStyledMemo.ContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
begin
  FLinesLayout.RealignIfNeeded;
  FLinesLayout.Render(ACanvas);

  if NeedShowSelection then
    DrawSelection(ACanvas);

  if HasImeMarkedText then
    DrawMarkedText(ACanvas, ARect);

  if Model.CheckSpelling then
    FSpellingManager.DrawHightlightSpellingWords(FLinesLayout, Model.ViewportSize, ACanvas, AbsoluteOpacity);
end;

procedure TStyledMemo.DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
  inherited;
  FLinesLayout.ViewportRect := TRectF.Create(NewViewportPosition, Model.ViewportSize.Width, Model.ViewportSize.Height);
  if not (OldViewportPosition - NewViewportPosition).IsZero then
  begin
    if Model.CheckSpelling then
      FSpellingManager.UpdateHightlightRect(FLinesLayout, Model.ViewportSize, OldViewportPosition - NewViewportPosition);

    if HasImeMarkedText then
      FTextService.RefreshImePosition;
  end;
end;

procedure TStyledMemo.DrawMarkedText(const ACanvas: TCanvas; const ARect: TRectF);
const
  DarkLightBorder = 0.5;

  procedure InitImeTextLayout(const ATextSettings: TTextSettings);
  begin
    FImeLayout.BeginUpdate;
    try
      FImeLayout.TopLeft := GetCaretPositionPoint(FTextService.MarkedTextPosition);
      FImeLayout.MaxSize := TPointF.Create(ViewportRect.Width - FImeLayout.TopLeft.X, ARect.Height);
      FImeLayout.Color := ATextSettings.FontColor;
      FImeLayout.Opacity := Opacity;
      FImeLayout.Text := FTextService.MarkedText;
    finally
      FImeLayout.EndUpdate;
    end;
  end;

  procedure UnderlineRegion(const ARegions: TRegion; const ANeedAddSpace: Boolean);
  var
    I: Integer;
    Region: TRectF;
    HalfThickness: Single;
    StartPoint, EndPoint: TPointF;
    Thickness: Single;
    ShrinkValue: Integer;
  begin
    Thickness := ACanvas.Stroke.Thickness;
    HalfThickness := Thickness / 2;
    ShrinkValue := IFThen(ANeedAddSpace, 1, 0);

    for I := Low(ARegions) to High(ARegions) do
    begin
      Region := ACanvas.AlignToPixel(ARegions[I]);
      Region.Offset(0, Thickness);

      StartPoint := TPointF.Create(Region.Left, Region.Bottom);
      StartPoint.Offset(ShrinkValue, -HalfThickness);
      EndPoint := Region.BottomRight;
      EndPoint.Offset(-ShrinkValue, -HalfThickness);
      ACanvas.DrawLine(StartPoint, EndPoint, Opacity);
    end;
  end;

  procedure ApplyMarkedTextAttribute(const ACanvas: TCanvas; const AAttribute: TMarkedTextAttribute);
  var
    UnderlineThickness: Single;
    UnderlineDash: TStrokeDash;
  begin
    {$IFDEF MSWINDOWS}
    case AAttribute of
      TMarkedTextAttribute.Input:
        begin
          UnderlineThickness := 1;
          UnderlineDash := TStrokeDash.Dash
        end;
      TMarkedTextAttribute.TargetConverted:
        begin
          UnderlineThickness := 2;
          UnderlineDash := TStrokeDash.Solid
        end;
      TMarkedTextAttribute.Converted:
        begin
          UnderlineThickness := 1;
          UnderlineDash := TStrokeDash.Solid
        end;
      TMarkedTextAttribute.TargetNotConverted:
        begin
          UnderlineThickness := 4;
          UnderlineDash := TStrokeDash.Solid
        end;
      TMarkedTextAttribute.InputError:
        begin
          UnderlineThickness := 1;
          UnderlineDash := TStrokeDash.Dot
        end;
    else
      UnderlineThickness := 1;
      UnderlineDash := TStrokeDash.Solid;
    end;
  {$ENDIF}
  {$IFDEF MACOS}
    UnderlineDash := TStrokeDash.Solid;
    case AAttribute of
      TMarkedTextAttribute.Converted:
        UnderlineThickness := 1;
      TMarkedTextAttribute.TargetNotConverted:
        UnderlineThickness := 2;
    else
      UnderlineThickness := 1;
    end;
  {$ENDIF}
    ACanvas.Stroke.Dash := UnderlineDash;
    ACanvas.Stroke.Thickness := UnderlineThickness;
  end;

  procedure UnderlineMarkedText(const ALayout: TTextLayout);
  var
    SavedState: TCanvasSaveState;
    I: Integer;
    Attributes: TArray<TMarkedTextAttribute>;
    GroupRange: TTextRange;
    Region: TRegion;
    NeedAddSpace: Boolean;
  begin
    SavedState := ACanvas.SaveState;
    try
      ACanvas.Stroke.Assign(ACanvas.Fill);
      ACanvas.Stroke.Color := FImeLayout.Color;

      GroupRange := TTextRange.Create(0, 0);
      Attributes := FTextService.MarketTextAttributes;
      for I := Low(Attributes) to High(Attributes) do
      begin
        ApplyMarkedTextAttribute(ACanvas, Attributes[I]);

        if (I < High(Attributes)) and (Attributes[I] <> Attributes[I + 1]) or (I = High(Attributes)) then
        begin
          GroupRange.Length := I - GroupRange.Pos + 1;
          Region := ALayout.RegionForRange(GroupRange);
        {$IFDEF MACOS}
          NeedAddSpace := Attributes[I] = TMarkedTextAttribute.TargetNotConverted;
        {$ELSE}
          NeedAddSpace := False;
        {$ENDIF}
          UnderlineRegion(Region, NeedAddSpace);
          GroupRange := TTextRange.Create(I + 1, 0);
        end;
      end;
    finally
      ACanvas.RestoreState(SavedState);
    end;
  end;

  procedure DrawImeBackground(const ATextSettings: TTextSettings);
  const
  {$IFDEF MACOS}
    MaxThickness = 3;
  {$ELSE}
    MaxThickness = 4;
  {$ENDIF}
  var
    BackgroundRect: TRectF;
  begin
    if Luminance(ATextSettings.FontColor) > DarkLightBorder then
      ACanvas.Fill.Color := TAlphaColorRec.Black
    else
      ACanvas.Fill.Color := TAlphaColorRec.White;

    BackgroundRect := FImeLayout.TextRect;
    BackgroundRect.Inflate(0, 0, 0, MaxThickness);
    ACanvas.FillRect(BackgroundRect, MarkedTextBackgroundOpacity);
  end;

  procedure DrawImeText(const ACanvas: TCanvas);
  begin
    FImeLayout.RenderLayout(ACanvas);
    UnderlineMarkedText(FImeLayout);
  end;

var
  TextSettings: TTextSettings;
begin
  TextSettings := Model.TextSettingsInfo.ResultingTextSettings;

  InitImeTextLayout(TextSettings);
  DrawImeBackground(TextSettings);
  DrawImeText(ACanvas);
end;

procedure TStyledMemo.DrawSelection(const ACanvas: TCanvas);
var
  I: Integer;
  Region: TRegion;
begin
  Region := GetVisibleSelectionRegion;
  for I := Low(Region) to High(Region) do
  begin
    if Region[I].Width = 0 then
      Region[I].Width := DefaultEmptySelectionWidth;

    ACanvas.FillRect(Region[I], 1, Model.SelectionFill);
  end;
end;

procedure TStyledMemo.AniMouseDown(const Touch: Boolean; const X, Y: Single);
begin
  // Temporary disabling scroll animation until mobile platform are added.
end;

procedure TStyledMemo.AniMouseMove(const Touch: Boolean; const X, Y: Single);
begin
  // Temporary disabling scroll animation until mobile platform are added.
end;

procedure TStyledMemo.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  // Temporary disabling scroll animation until mobile platform are added.
end;

procedure TStyledMemo.ApplyStyle;
var
  StyleResource: TFmxObject;
  BrushObject: TBrushObject;
  FontObject: IFontObject;
  TextSettingsInfo: TTextSettingsInfo;
  TextSettings: TTextSettings;
  DefaultTextSettings: TTextSettings;
begin
  TextSettingsInfo := Model.TextSettingsInfo;
  TextSettings := TextSettingsInfo.TextSettings;
  DefaultTextSettings := TextSettingsInfo.DefaultTextSettings;

  TextSettings.BeginUpdate;
  try
    TextSettingsInfo.Design := False;
    inherited;

    if FindStyleResource<TBrushObject>('selection', BrushObject) then
      Model.SelectionFill := BrushObject.Brush;

    { Default Text settings }
    if FindStyleResource<TBrushObject>('foreground', BrushObject) then
      DefaultTextSettings.FontColor := BrushObject.Brush.Color;
    StyleResource := FindStyleResource('font');
    if Supports(StyleResource, IFontObject, FontObject) and not TextSettings.Font.IsSizeStored then
      DefaultTextSettings.Font := FontObject.Font;
    DefaultTextSettings.HorzAlign := TTextAlign.Leading;

    { Caret Color }
    StyleResource := FindStyleResource('caretcolor');
    if StyleResource is TColorObject then
      Model.Caret.DefaultColor := TColorObject(StyleResource).Color
    else
      Model.Caret.DefaultColor := TAlphaColorRec.Null;
    TextSettings.Change;
  finally
    TextSettings.EndUpdate;
    TextSettingsInfo.Design := csDesigning in ComponentState;
  end;
  FLinesLayout.TextSettings := Model.TextSettingsInfo.ResultingTextSettings;
end;

procedure TStyledMemo.RepaintEdit;
begin
  if Content <> nil then
    Content.Repaint;
end;

procedure TStyledMemo.ReplaceWordHandler(const ASpellingWord: TSpellingWord; const ASuggestion: string);
begin
  Model.Replace(ASpellingWord.Position, ASpellingWord.Length, ASuggestion);
  RepaintEdit;
end;

procedure TStyledMemo.Resize;
begin
  inherited;
  if Model.CheckSpelling then
    FSpellingManager.ResetBounds;
  UpdateCaretPosition;
  UpdateScrollbarsPosition;
end;

procedure TStyledMemo.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if TSelectionMethod.Mouse in FSelectionMethods then
  begin
    PutCaretTo(X, Y, TSelectionOption.SelectWords in FSelectionOptions);
    FSelectionController.Finish := CaretPosition;
    StartAutoScroll(TPointF.Create(X, Y));
    UpdateScrollbarsPosition;
  end;
end;

procedure TStyledMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FSetFocusOnUp and not AniCalculations.Moved then
  begin
    FSetFocusOnUp := False;
    if not (csDesigning in PresentedControl.ComponentState) and not PresentedControl.IsFocused then
      PresentedControl.SetFocus;
  end;
  if TSelectionMethod.Mouse in FSelectionMethods then
  begin
    PutCaretTo(X, Y, TSelectionOption.SelectWords in FSelectionOptions);
    FSelectionController.Finish := CaretPosition;
  end;
  UpdateCaretPosition;
  SelectionController.UnholdSelection;
  Exclude(FSelectionMethods, TSelectionMethod.Mouse);
  Exclude(FSelectionOptions, TSelectionOption.SelectWords);
  FAutoscrollController.Stop;
end;

procedure TStyledMemo.FillPopupMenu(const AMenu: TPopupMenu);
var
  LMenuItem: TContextMenuItem;
begin
  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditUndo);
  LMenuItem.StyleName := UndoStyleName;
  LMenuItem.ContextAction := TContextAction.Undo;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := SMenuSeparator;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditCut);
  LMenuItem.StyleName := CutStyleName;
  LMenuItem.ContextAction := TContextAction.Cut;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditCopy);
  LMenuItem.StyleName := CopyStyleName;
  LMenuItem.ContextAction := TContextAction.Copy;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditPaste);
  LMenuItem.StyleName := PasteStyleName;
  LMenuItem.ContextAction := TContextAction.Paste;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditDelete);
  LMenuItem.StyleName := DeleteStyleName;
  LMenuItem.ContextAction := TContextAction.Delete;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := SMenuSeparator;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditSelectAll);
  LMenuItem.StyleName := SelectAllStyleName;
  LMenuItem.ContextAction := TContextAction.SelectAll;
  LMenuItem.OnClick := ContextMenuItemClick;
end;

procedure TStyledMemo.UpdatePopupMenuItems(const AMenu: TPopupMenu);
var
  ClipService: IFMXExtendedClipboardService;

  procedure SetEnabled(const AParamName: string; const AValue : Boolean);
  var
    MenuItem : TMenuItem;
  begin
    MenuItem := TMenuItem(AMenu.FindStyleResource(AParamName));
    if MenuItem <> nil then
      MenuItem.Enabled := AValue;
  end;

begin
  SetEnabled(UndoStyleName, not Model.ReadOnly and (FActionStack.Count > 0));
  SetEnabled(CutStyleName, FSelectionController.IsSelected and not Model.ReadOnly);
  SetEnabled(CopyStyleName, FSelectionController.IsSelected);
  SetEnabled(PasteStyleName, TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, ClipService) and
    ClipService.HasText and not Model.ReadOnly);
  SetEnabled(DeleteStyleName, FSelectionController.IsSelected and not Model.ReadOnly);
  SetEnabled(SelectAllStyleName, Model.SelLength <> Model.Lines.Text.Length);
end;

procedure TStyledMemo.UpdateScrollbarsPosition;
type
  TRelativeLocation = (Above, Below, &In, OnTheLeftSide, OnTheRightSide);

  function RectLocation(const ARect: TRectF): TRelativeLocation;
  begin
    if ARect.Bottom > Model.ViewportSize.Height then
      Result := TRelativeLocation.Below
    else if ARect.Top < 0 then
      Result := TRelativeLocation.Above
    else if ARect.Left < 0 then
      Result := TRelativeLocation.OnTheLeftSide
    else if ARect.Left > Model.ViewportSize.Width then
      Result := TRelativeLocation.OnTheRightSide
    else
      Result := TRelativeLocation.&In;
  end;

  function CalculateViewportOffset(const ACaretRect: TRectF): TPointF;
  const
    DefaultOffset = 50;
  begin
    case RectLocation(ACaretRect) of
      TRelativeLocation.Above:
        Result := TPointF.Create(0, ACaretRect.Top);
      TRelativeLocation.Below:
        Result := TPointF.Create(0, ACaretRect.Bottom - Model.ViewportSize.Height);
      TRelativeLocation.OnTheLeftSide:
        Result := TPointF.Create(ACaretRect.Left - DefaultOffset, 0);
      TRelativeLocation.OnTheRightSide:
        Result := TPointF.Create(ACaretRect.Left - Model.ViewportSize.Width + DefaultOffset, 0);
    else
      Result := TPointF.Zero;
    end;
  end;

var
  CaretRegion: TRegion;
  CaretRect: TRectF;
begin
  if csLoading in ComponentState then
    Exit;

  CaretRegion := FLinesLayout.GetRegionForRange(CaretPosition, 1);
  if Length(CaretRegion) = 0 then
    Exit;

  // Convert rect to viewport coordinate system
  CaretRect := CaretRegion[0];
  CaretRect.Offset(-ViewportPosition);

  ViewportPosition := ViewportPosition + CalculateViewportOffset(CaretRect);
  // Only for Update
  TStyledMemoHelper.Current.SetNeedUpdateContentOffset(Self, False);
end;

procedure TStyledMemo.UpdateTextInTextService;
begin
  if HasText then
    FTextService.Text := Model.Lines[FCaretPosition.Line]
  else
    FTextService.Text := string.Empty;
end;

function TStyledMemo.ViewportRect: TRectF;
begin
  if ContentLayout = nil then
    Result := TRectF.Create(0, 0, Model.ViewportSize.Width, Model.ViewportSize.Height)
  else
    Result := ContentLayout.BoundsRect;
end;

procedure TStyledMemo.ScrollOnLine(const ADirection: TScrollDirection);
var
  LLineHeight : Single;
  NewViewportPosition: TPointF;
  Delta: Single;
begin
  if HasText and VScrollBar.Visible then
  begin
    LLineHeight := Model.ContentBounds.Height / Model.Lines.Count;
    NewViewportPosition := ViewportPosition;
    case ADirection of
      TStyledMemo.TScrollDirection.Up: Delta := -LLineHeight;
      TStyledMemo.TScrollDirection.Down: Delta := LLineHeight;
    else
      Delta := LLineHeight;
    end;
    NewViewportPosition.Offset(0, Delta);
    ViewportPosition := NewViewportPosition;
  end;
end;

procedure TStyledMemo.EndIMEInput;
begin
  // Some platforms have their own behavior in displaying the cursor. For example, macOS displays marked IME text
  // via underlining
  Model.Caret.TemporarilyHidden := False;
  if HasText and (CaretPosition.Line < Model.Lines.Count) then
    CaretPosition := TCaretPosition.Create(CaretPosition.Line, Min(CaretPosition.Pos, Model.Lines[CaretPosition.Line].Length));
end;

procedure TStyledMemo.ExecuteContextAction(const AAction: TContextAction);
begin
  case AAction of
    TContextAction.Cut:
      Memo.CutToClipboard;
    TContextAction.Copy:
      Memo.CopyToClipboard;
    TContextAction.Paste:
      Memo.PasteFromClipboard;
    TContextAction.Delete:
      Memo.DeleteSelection;
    TContextAction.Undo:
      Undo;
    TContextAction.SelectAll:
      Memo.SelectAll;
  end;
end;

procedure TStyledMemo.AutoScrollHandler(const ADirection: TAutoscrollDirection; var AStop: Boolean);
var
  PreviousCaretPosition: TCaretPosition;
begin
  PreviousCaretPosition := CaretPosition;
  case ADirection of
    TAutoscrollDirection.LeftToRight:
      CaretPosition := TCaretPosition.Create(CaretPosition.Line, CaretPosition.Pos + 1);

    TAutoscrollDirection.RightToLeft:
      CaretPosition := TCaretPosition.Create(CaretPosition.Line, Max(0, CaretPosition.Pos - 1));

    TAutoscrollDirection.BottomToTop,
    TAutoscrollDirection.LeftBottomToRightTop,
    TAutoscrollDirection.RightBottomToLeftTop:
      MoveCaretVertical(-1);

    TAutoscrollDirection.TopToBottom,
    TAutoscrollDirection.RightTopToLeftBottom:
      MoveCaretVertical(1);

    TAutoscrollDirection.LeftTopToRightBottom:
    begin
      MoveCaretVertical(1);
      if CaretPosition.Line = LinesLayout.Count - 1 then
        GotoLineEnd;
    end;
  end;

  AStop := PreviousCaretPosition = CaretPosition;
  if AStop then
    Exit;

  FSelectionController.Finish := CaretPosition;
end;

procedure TStyledMemo.DoBeginUpdate;
begin
  inherited;
  FLinesLayout.BeginUpdate;
end;

procedure TStyledMemo.DoChange;
begin
  if not (csLoading in ComponentState) then
    Model.Change;
end;

procedure TStyledMemo.CMGesture(var EventInfo: TGestureEventInfo);
begin
  case EventInfo.GestureID of
    igiLongTap:
      LongTap(AbsoluteToLocal(EventInfo.Location), EventInfo.Flags);

    igiDoubleTap:
      DblTap;
  else
    inherited;
  end;
end;

procedure TStyledMemo.ContextMenuItemClick(Sender: TObject);
var
  Action: TContextAction;
begin
  if Sender is TContextMenuItem then
  begin
    Action := TContextMenuItem(Sender).ContextAction;
    ExecuteContextAction(Action);
  end;
end;

function TStyledMemo.ShowContextMenu(const ScreenPosition: TPointF): Boolean;
var
  LCaretPosition: TCaretPosition;
begin
  Result := inherited;
  FSelectionMethods := [];
  if not Result and not (csDesigning in ComponentState) then
  begin
    UpdatePopupMenuItems(FMemoPopupMenu);
    if Model.CheckSpelling then
    begin
      LCaretPosition := FLinesLayout.GetCaretPositionByPoint(ScreenToLocal(ScreenPosition));
      FSpellingManager.AddSuggestionsToPopupMenu(FMemoPopupMenu, LCaretPosition);
    end;

    if Root <> nil then
      FMemoPopupMenu.Parent := Root.GetObject;
    try
      FMemoPopupMenu.Popup(Round(ScreenPosition.X), Round(ScreenPosition.Y));
    finally
      FMemoPopupMenu.Parent := nil;
    end;
    Result := True;
  end;
end;

procedure TStyledMemo.FreeNotification(AObject: TObject);
begin
  inherited;
  if AObject = FMemoPopupMenu then
    FMemoPopupMenu := nil;
end;

procedure TStyledMemo.FreeStyle;
begin
  inherited;
end;

procedure TStyledMemo.SetCaretPosition(const Value: TCaretPosition);
var
  NewCaretPosition: TCaretPosition;
  IsLineChanged: Boolean;
begin
  NewCaretPosition := NormalizeCaretPosition(Value);
  IsLineChanged := FCaretPosition.Line <> NewCaretPosition.Line;
  FCaretPosition := NewCaretPosition;

  if FTextService <> nil then
  begin
    if IsLineChanged then
      UpdateTextInTextService;
    FTextService.MarkedTextPosition := FCaretPosition;
    FTextService.CaretPosition := FCaretPosition;
    FImeLayout.TopLeft := GetCaretPositionPoint(FTextService.MarkedTextPosition);
  end;

  if IsUpdating then
    TStyledMemoHelper.Current.SetNeedUpdateContentOffset(Self, True)
  else
  begin
    UpdateCaretPosition;
    UpdateScrollbarsPosition;
    RepaintEdit;
  end;
end;

procedure TStyledMemo.RecalcOpacity;
begin
  inherited;
  FLinesLayout.Opacity := AbsoluteOpacity;
  FImeLayout.Opacity := AbsoluteOpacity;
end;

procedure TStyledMemo.DblTap;
begin
end;

function TStyledMemo.GetTextService: TTextService;
begin
  Result := FTextService;
end;

function TStyledMemo.GetVisibleSelectionRegion: TRegion;

  function Max(const AValue1, AValue2: TCaretPosition): TCaretPosition;
  begin
    if AValue1 >= AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  function Min(const AValue1, AValue2: TCaretPosition): TCaretPosition;
  begin
    if AValue1 <= AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  function IntersectSegments(const A1, A2, B1, B2: TCaretPosition; var C1, C2: TCaretPosition): Boolean;
  var
    IsABIntersection: Boolean;
    IsBAIntersection: Boolean;
  begin
    Assert(A1 <= A2);
    Assert(B1 <= B2);

    IsABIntersection := (B1 <= A2) and (A2 <= B2);
    IsBAIntersection := (A1 <= B2) and (B2 <= A2);
    Result := IsABIntersection or IsBAIntersection;
    if Result then
    begin
      C1 := Max(A1, B1);
      C2 := Min(A2, B2);
    end;
  end;

var
  VisibleSelBegin: TCaretPosition;
  VisibleSelEnd: TCaretPosition;
  VisibleSelLength: Integer;
  FirstVisibleLine: TCaretPosition;
  LastVisibleLine: TCaretPosition;
begin
  // FirstVisibleLineIndex and LastVisibleLineIndex can have invalid values in case of updating process.
  if (FLinesLayout.FirstVisibleLineIndex = -1) or (FLinesLayout.LastVisibleLineIndex = -1) or FLinesLayout.IsUpdating then
    Exit(nil);

  FirstVisibleLine := TCaretPosition.Create(FLinesLayout.FirstVisibleLineIndex, 0);
  LastVisibleLine := TCaretPosition.Create(FLinesLayout.LastVisibleLineIndex, Model.Lines[FLinesLayout.LastVisibleLineIndex].Length);

  if IntersectSegments(FSelectionController.SelBegin, FSelectionController.SelEnd,
                       FirstVisibleLine, LastVisibleLine,
                       VisibleSelBegin, VisibleSelEnd) then
  begin
    VisibleSelLength := Model.PosToTextPos(VisibleSelEnd) - Model.PosToTextPos(VisibleSelBegin);
    Result := FLinesLayout.GetRegionForRange(VisibleSelBegin, VisibleSelLength);
  end
  else
    Result := [];
end;

function LinkObserversValueModified(const AObservers: TObservers): Boolean;
begin
  Result := True;
  if AObservers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    Result := TLinkObservers.EditLinkEdit(AObservers);
    if Result then
      TLinkObservers.EditLinkModified(AObservers);
  end;
  if Result and AObservers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueModified(AObservers);
end;

procedure TStyledMemo.IMEStateUpdated;
var
  LCaret: TCaretPosition;
begin
  if FTextService <> nil then
  begin
    LCaret := FTextService.TargetClausePosition;
    if Model.Lines.Count > 0 then
      LCaret.Line := EnsureRange(LCaret.Line, 0, Model.Lines.Count - 1)
    else
       // This situation can occur when text is empty and user begins to input text from IME text input.
      LCaret.Line := 0;

    FImeLayout.Text := FTextService.MarkedText;
    UpdateCaretPosition;
    RepaintEdit;
  end;
end;

function TStyledMemo.GetTargetClausePointF: TPointF;
var
  TmpPt: TPointF;
begin
  if FTextService = nil then
    Result := TPointF.Zero
  else
  begin
    TmpPt := GetCaretPositionPoint(FTextService.MarkedTextPosition);
    TmpPt.Offset(0, FImeLayout.Height + IMEWindowGap);
    Result := Content.LocalToAbsolute(TmpPt);
  end;
end;

procedure TStyledMemo.Undo;
begin
  if not Model.ReadOnly then
    FActionStack.RollBackAction;
end;

procedure TStyledMemo.UpdateCaretPosition;

  function GetCaretSize: TSizeF;
  begin
    if (CaretPosition.Line >= 0) and (CaretPosition.Line < FLinesLayout.Count) and not LinesLayout.IsWordWrap then
      Result := TPointF.Create(Model.Caret.Size.cx, FLinesLayout[CaretPosition.Line].Size.Height)
    else
      Result := TPointF.Create(Model.Caret.Size.cx, FLinesLayout.GetLineHeight);
  end;

  function GetImeCaretPosition: TPointF;
  var
    MarkedTextPosition: Integer;
    OffsetPosition: Integer;
    TextRange: TTextRange;
    Region: TRegion;
  begin
    MarkedTextPosition := Model.PosToTextPos(FTextService.MarkedTextPosition);
    OffsetPosition := Model.PosToTextPos(FTextService.TargetClausePosition) - MarkedTextPosition;

    TextRange := TTextRange.Create(OffsetPosition, 1);

    FImeLayout.TopLeft := GetCaretPositionPoint(CaretPosition);
    Region := FImeLayout.RegionForRange(TextRange);
    Result := Region[0].TopLeft;
  end;

begin
  Model.Caret.BeginUpdate;
  try
    Model.Caret.Size := GetCaretSize;
    if HasImeMarkedText then
      Model.Caret.Pos := GetImeCaretPosition
    else
      Model.Caret.Pos := GetCaretPositionPoint(CaretPosition);
  finally
     Model.Caret.EndUpdate;
  end;
end;

function TStyledMemo.IsCurrentWordWrong: Boolean;
begin
  Result := FSpellingManager.IsWordWrong(CaretPosition);
end;

function TStyledMemo.IsSelecting: Boolean;
begin
  Result := FSelectionMethods <> [];
end;

function TStyledMemo.IsSpellCheckEnabled: Boolean;
begin
  Result := Model.CheckSpelling;
end;

function TStyledMemo.GetListOfPrepositions: TArray<string>;
begin
  Result := FSpellingManager.GetListOfPrepositions(CaretPosition);
end;

procedure TStyledMemo.Spell(const AWord: string);
var
  LCaretPosition: TCaretPosition;
begin
  LCaretPosition := CaretPosition;
  FSpellingManager.Spell(LCaretPosition, AWord);
  CaretPosition := LCaretPosition;
end;

procedure TStyledMemo.StartAutoScroll(const ALocalPoint: TPointF);
var
  Rect: TRectF;
  Direction: TAutoscrollDirection;
begin
  if not HasText then
    Exit;

  Rect := ViewportRect;
  if (ALocalPoint.X <= Rect.Left) and (ALocalPoint.Y <= Rect.Top) then
    Direction := TAutoscrollDirection.RightBottomToLeftTop
  else if (ALocalPoint.X <= Rect.Left) and (ALocalPoint.Y >= Rect.Bottom) then
    Direction := TAutoscrollDirection.RightTopToLeftBottom
  else if (ALocalPoint.X >= Rect.Right) and (ALocalPoint.Y <= Rect.Top) then
    Direction := TAutoscrollDirection.LeftBottomToRightTop
  else if (ALocalPoint.X >= Rect.Right) and (ALocalPoint.Y >= Rect.Bottom) then
    Direction := TAutoscrollDirection.LeftTopToRightBottom
  else if ALocalPoint.X <= Rect.Left then
    Direction := TAutoscrollDirection.RightToLeft
  else if ALocalPoint.X >= Rect.Right then
    Direction := TAutoscrollDirection.LeftToRight
  else if ALocalPoint.Y <= Rect.Top then
    Direction := TAutoscrollDirection.BottomToTop
  else if ALocalPoint.Y >= Rect.Bottom then
    Direction := TAutoscrollDirection.TopToBottom
  else
  begin
    FAutoscrollController.Stop;
    Exit;
  end;

  FAutoscrollController.Start(Direction);
end;

procedure TStyledMemo.StartIMEInput;
begin
  if FTextService = nil then
    Exit;

  Model.Caret.TemporarilyHidden := NeedHideCaretInIME;

  UpdateTextInTextService;
  FTextService.CaretPosition := CaretPosition;
  FTextService.MarkedTextPosition := CaretPosition;
  UpdateCaretPosition;
end;

function TStyledMemo.GetPageSize: Single;
begin
  Result := Model.ViewportSize.Height / FLinesLayout.GetLineHeight;
end;

procedure TStyledMemo.PMInit(var Message: TDispatchMessage);
var
  I: Integer;
begin
  inherited;
  if FTextService <> nil then
    FTextService.MaxLength := Model.MaxLength;
  if HasText then
  begin
    BeginUpdate;
    try
      for I := 0 to Model.Lines.Count - 1 do
        FLinesLayout.InsertLine(I, Model.Lines[I]);
    finally
      EndUpdate;
    end;
  end;
  if Model.Caret.Flasher <> nil then
    FLinesLayout.CaretWidth := Model.Caret.Flasher.Size.Width;
  Content.OnPainting := ContentPaint;
  Content.OnGetClipRect := ContentGetClipRectHandler;
end;

procedure TStyledMemo.PMRootChanged(var Message: TDispatchMessageWithValue<IRoot>);
begin
  inherited;
end;

procedure TStyledMemo.PMSelectText(var Message: TDispatchMessage);
begin
  FSelectionController.SetRange(Model.SelStart, Model.SelLength);
  CaretPosition := FSelectionController.SelEnd;
end;

procedure TStyledMemo.PutCaretTo(const X, Y: Single; const APositionByWord: Boolean);
type
  TRelativePositionToInitialSelection = (Before, &In, After);

  function DefineRelativePosition(const ACaretPosition: TCaretPosition): TRelativePositionToInitialSelection;
  begin
    if ACaretPosition < SelectionController.HoldSelBegin then
      Result := TRelativePositionToInitialSelection.Before
    else if SelectionController.HoldSelEnd < ACaretPosition then
      Result := TRelativePositionToInitialSelection.After
    else
      Result := TRelativePositionToInitialSelection.&In;
  end;

  function GetPrevWordEnd(const ACaretPosition: TCaretPosition): TCaretPosition;
  var
    WordStartIndex: Integer;
    WordEndIndex: Integer;
  begin
    Result := ACaretPosition;
    if FindWordBound(Model.Lines[ACaretPosition.Line], ACaretPosition.Pos, WordStartIndex, WordEndIndex) then
      if InRange(ACaretPosition.Pos, WordStartIndex, WordEndIndex) then
        Result.Pos := WordStartIndex
      else
        Result.Pos := WordEndIndex + 1
    else
    begin
      Result := Model.GetPrevWordBegin(ACaretPosition);
      if FindWordBound(Model.Lines[Result.Line], Result.Pos, WordStartIndex, WordEndIndex) then
        Result.Pos := WordEndIndex + 1;
    end;
  end;

  function FindNextWordBegin(const ACaretPosition: TCaretPosition): TCaretPosition;
  var
    WordStartIndex: Integer;
    WordEndIndex: Integer;
    NextWordCaretPosition: TCaretPosition;
  begin
    Result := ACaretPosition;
    if FindWordBound(Model.Lines[Result.Line], Result.Pos, WordStartIndex, WordEndIndex) then
    begin
      if Result.Pos = WordStartIndex then
        // We are at the word beginning
        Result.Pos := WordStartIndex
      else
        // Caret is in the bounds of new word, so move caret to the word end
        Result.Pos := WordEndIndex + 1
    end
    else
    begin
      // Caret is in words separators (spaces, coma, column)
      NextWordCaretPosition := Model.GetNextWordBegin(Result);
      if NextWordCaretPosition = Result then
      begin
        Result := NextWordCaretPosition;
        Result.Pos := Result.Pos +1;
      end
      else
        Result := NextWordCaretPosition;
    end;
  end;

  function FindPreviousWordEnd(const ACaretPosition: TCaretPosition): TCaretPosition;
  var
    WordStartIndex: Integer;
    WordEndIndex: Integer;
  begin
    Result := ACaretPosition;

    if FindWordBound(Model.Lines[Result.Line], Result.Pos, WordStartIndex, WordEndIndex) and InRange(Result.Pos, WordStartIndex, WordEndIndex) then
      // ¬нутри слова. сдвигаем курсор на начало слова
      Result.Pos := WordStartIndex
    else
      //  аретка попала на разделитель между словами, должны округлить позицию до конца предыдущего слова
      Result := GetPrevWordEnd(Result);
  end;

var
  ContentPoint: TPointF;
  NewCaretPosition: TCaretPosition;
  RelativePosition: TRelativePositionToInitialSelection;
begin
  // X, Y in the presentation coordinate system
  ContentPoint := ConvertLocalPointTo(Content, TPointF.Create(X, Y));

  NewCaretPosition := FLinesLayout.GetCaretPositionByPoint(ContentPoint, False);
  if APositionByWord then
    if SelectionController.IsSelected then
    begin
      // When user uses selection by words, we have to keep initial selected word independently of selection direction.
      RelativePosition := DefineRelativePosition(NewCaretPosition);
      case RelativePosition of
        TRelativePositionToInitialSelection.Before:
        begin
          SelectionController.SetRange(SelectionController.HoldSelEnd, SelectionController.SelBegin);
          NewCaretPosition := FindPreviousWordEnd(NewCaretPosition);
        end;
        TRelativePositionToInitialSelection.In:
        begin
          SelectionController.SetRange(SelectionController.HoldSelBegin, SelectionController.HoldSelEnd);
          NewCaretPosition := SelectionController.HoldSelEnd;
        end;
        TRelativePositionToInitialSelection.After:
        begin
          SelectionController.SetRange(SelectionController.HoldSelBegin, SelectionController.SelEnd);
          NewCaretPosition := FindNextWordBegin(NewCaretPosition);
        end;
      end;
    end
    else
      NewCaretPosition := Model.GetNextWordBegin(NewCaretPosition);

  CaretPosition := NewCaretPosition;
end;

function TStyledMemo.ConvertLocalPointFrom(const AControl: TControl; const AControlLocalPoint: TPointF): TPointF;
begin
  Assert(AControl <> nil);

  Result := AControl.LocalToAbsolute(AControlLocalPoint);
  Result := AbsoluteToLocal(Result);
end;

function TStyledMemo.ConvertLocalPointTo(const AControl: TControl; const ALocalPoint: TPointF): TPointF;
begin
  Assert(AControl <> nil);

  Result := LocalToAbsolute(ALocalPoint);
  Result := AControl.AbsoluteToLocal(Result);
end;

function TStyledMemo.HasImeMarkedText: Boolean;
begin
  Result := (FTextService <> nil) and FTextService.HasMarkedText;
end;

function TStyledMemo.HasText: Boolean;
begin
  Result := Model.Lines.Count > 0;
end;

procedure TStyledMemo.HighlightSpell;
begin
  FSpellingManager.HighlightSpell(FLinesLayout, CaretPosition);
  RepaintEdit;
end;

function TStyledMemo.GetSelection: string;
begin
  Result := Model.SelectedText;
end;

function TStyledMemo.GetSelectionBounds: TRect;
begin
  if FSelectionController.IsSelected then
    Result := TRect.Create(FSelectionController.SelBegin, FSelectionController.SelEnd)
  else
    Result := TRect.Create(FCaretPosition, FCaretPosition);
end;

function TStyledMemo.GetSelectionPointSize: TSizeF;
begin
  Result := TSizeF.Create(0, 0);
end;

function TStyledMemo.GetSelectionRect: TRectF;
var
  TmpRect, SelRect: TRectF;
  Region: TRegion;
  I: Integer;
  TmpPt: TPointF;
begin
  Region := FLinesLayout.GetRegionForRange(CaretPosition, 1);
  if Length(Region) = 0 then
    TmpPt := TPointF.Zero
  else
    TmpPt := Region[0].TopLeft;

  Result := TRectF.Create(TmpPt, 1, FLinesLayout.GetLineHeight);
  if NeedShowSelection and (Model.SelLength > 0) then
  begin
    Region := GetVisibleSelectionRegion;
    if Length(Region) > 0 then
      Result := Region[0];
    SelRect := ViewportRect;
    for I := Low(Region) to High(Region) do
    begin
      IntersectRect(TmpRect, Region[I], SelRect);
      Result := TRectF.Union(Result, TmpRect);
    end;
  end;
  Result.Location := ConvertLocalPointFrom(Content, Result.TopLeft);
end;

function TStyledMemo.GetSelectionRegion: TRegion;
var
  LCaret: TCaretPosition;
begin
  LCaret := FSelectionController.SelBegin;
  Result := FLinesLayout.GetRegionForRange(LCaret, Model.SelLength);
end;

procedure TStyledMemo.SelectionChangedHandler(Sender: TObject; const ASelStart, ALength: Integer);
begin
  Model.DisableNotify;
  try
    Model.SelStart := ASelStart;
    Model.SelLength := ALength;
  finally
    Model.EnableNotify;
  end;
  RepaintEdit;
end;

procedure TStyledMemo.MoveCaretVertical(const ALineDelta: Integer);
var
  Pt: TPointF;
  NewCaretPosition: TCaretPosition;
begin
  Pt := Model.Caret.Pos;
  Pt.Offset(0, FLinesLayout.GetLineHeight / 2 + ALineDelta * FLinesLayout.GetLineHeight);

  NewCaretPosition := FLinesLayout.GetCaretPositionByPoint(Pt);
  if not NewCaretPosition.IsInvalid then
  begin
    FCaretPosition := NewCaretPosition;
    UpdateCaretPosition;
    UpdateScrollbarsPosition;
  end;
end;

procedure TStyledMemo.MoveCaretPageDown;
var
  ScrollLineNumber: Integer;
begin
  ScrollLineNumber := Max(1, Trunc(GetPageSize));
  MoveCaretVertical(ScrollLineNumber);
end;

procedure TStyledMemo.MoveCaretPageUp;
var
  ScrollLineNumber: Integer;
begin
  ScrollLineNumber := Max(1, Trunc(GetPageSize));
  MoveCaretVertical(-ScrollLineNumber);
end;

function TStyledMemo.NeedHideCaretInIME: Boolean;
begin
{$IFDEF MACOS}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TStyledMemo.NeedShowSelection: Boolean;
begin
  Result := IsFocused or not Model.HideSelectionOnExit and FSelectionController.IsSelected;
end;

function TStyledMemo.NormalizeCaretPosition(const Value: TCaretPosition): TCaretPosition;
begin
  if Value.IsInvalid or (Model.Lines.Count = 0) then
    Result := TCaretPosition.Zero
  else
  begin
    Result.Line := EnsureRange(Value.Line, 0, Model.Lines.Count - 1);
    Result.Pos := EnsureRange(Value.Pos, 0, Model.Lines[Result.Line].Length);
  end;
end;

procedure TStyledMemo.NormalizeCaretPosition;
begin
  CaretPosition := NormalizeCaretPosition(CaretPosition);
end;

{ TEditActionStack }

constructor TEditActionStack.Create(const AModel: TCustomMemoModel);
begin
  inherited Create;
  FModel := AModel;
end;

procedure TEditActionStack.FragmentDeleted(const AStartPos: Integer; const AFragment: string;
  const ASelected, ACaretMoved: Boolean);
var
  TmpItem: TEditAction;
begin
  if AFragment.IsEmpty then
    Exit;

  if (Count = 0) or (Peek.ActionType <> TActionType.Delete) or (Peek.StartPosition - AStartPos - AFragment.Length > 1)
    or (Peek.StartPosition - AStartPos <= 0) or ASelected then
  begin
    TmpItem.ActionType := TActionType.Delete;
    TmpItem.StartPosition := AStartPos;
    TmpItem.DeletedFragment := AFragment;
    TmpItem.PairedWithPrev := False;
    TmpItem.WasSelected := ASelected;
    TmpItem.CaretMoved := ACaretMoved;
    Push(TmpItem);
  end
  else if AStartPos >= 0 then
  begin
    TmpItem := Pop;
    if AStartPos < TmpItem.StartPosition then
      TmpItem.DeletedFragment := AFragment + TmpItem.DeletedFragment
    else
      TmpItem.DeletedFragment := TmpItem.DeletedFragment + AFragment;
    TmpItem.StartPosition := AStartPos;
    Push(TmpItem);
  end;
end;

procedure TEditActionStack.FragmentInserted(const AStartPos, AFragmentLength: Integer; const APairedWithPrev, ATyped: Boolean);
var
  TmpItem: TEditAction;
begin
  if AFragmentLength = 0 then
    Exit;

  if (Count = 0) or (Peek.ActionType <> TActionType.Insert) or ((Peek.StartPosition + Peek.Length) <> AStartPos) or
    not ATyped or (Peek.Typed <> ATyped) then
  begin
    TmpItem.ActionType := TActionType.Insert;
    TmpItem.StartPosition := AStartPos;
    TmpItem.Length := AFragmentLength;
    TmpItem.PairedWithPrev := APairedWithPrev;
    TmpItem.Typed := ATyped;
    Push(TmpItem);
  end
  else
  begin
    TmpItem := Pop;
    TmpItem.Length := TmpItem.Length + AFragmentLength;
    Push(TmpItem);
  end;
end;

function TEditActionStack.RollBackAction: Boolean;
var
  TmpItem: TEditAction;
  WasPaired: Boolean;
  LTmpOptions: TInsertOptions;
begin
  Result := (Count > 0);

  if Result and (FModel <> nil) then
  repeat
    TmpItem := Pop;

    if TmpItem.WasSelected then
      LTmpOptions := [TInsertOption.Selected]
    else
      LTmpOptions := [];
    if TmpItem.CaretMoved then
      LTmpOptions := LTmpOptions + [TInsertOption.MoveCaret];

    FModel.DisableNotify;
    try
      FModel.SelLength := 0;
    finally
      FModel.EnableNotify;
    end;

    case TmpItem.ActionType of
      TActionType.Delete:
        FModel.InsertAfter(FModel.TextPosToPos(TmpItem.StartPosition), TmpItem.DeletedFragment,
          LTmpOptions);
      TActionType.Insert:
        FModel.DeleteFrom(FModel.TextPosToPos(TmpItem.StartPosition), TmpItem.Length, [TDeleteOption.MoveCaret]);
    end;

    WasPaired := TmpItem.PairedWithPrev;
  until (Count = 0) or (not WasPaired);
end;

{ TLineObject }

function TLineObject.ContainsPoint(const AHitPoint: TPointF): Boolean;
begin
  Result := ((AHitPoint.Y > FRect.Top) or SameValue(AHitPoint.Y, FRect.Top, TEpsilon.Position)) and
            ((AHitPoint.Y < FRect.Bottom) or SameValue(AHitPoint.Y, FRect.Bottom, TEpsilon.Position));
end;

constructor TLineObject.Create;
begin
  FLayout := nil;
  FRect := TRectF.Empty;
  FState := [TState.InvalidSize, TState.InvalidPosition, TState.InvalidLayout];
end;

destructor TLineObject.Destroy;
begin
  FreeAndNil(FLayout);
  inherited;
end;

procedure TLineObject.DoRender(const ACanvas: TCanvas);
begin
  FLayout.RenderLayout(ACanvas);
end;

function TLineObject.GetLocation: TPointF;
begin
  Result := FRect.TopLeft;
end;

function TLineObject.GetSize: TSizeF;
begin
  Result := FRect.Size;
end;

procedure TLineObject.Invalidate;
begin
  InvalidateLayout;
  InvalidateSize;
  InvalidatePosition;
end;

procedure TLineObject.InvalidateLayout;
begin
  if Layout <> nil then
    Include(FState, TState.InvalidLayout);
end;

procedure TLineObject.InvalidatePosition;
begin
  Include(FState, TState.InvalidPosition);
end;

procedure TLineObject.InvalidateSize;
begin
  Include(FState, TState.InvalidSize);
end;

procedure TLineObject.ReleaseLayoutIfNotVisible(const AViewportRect: TRectF);
begin
  if not IsVisible(AViewportRect) then
    FreeAndNil(FLayout);
end;

procedure TLineObject.Render(const ACanvas: TCanvas);
begin
  if FLayout <> nil then
    DoRender(ACanvas);
end;

procedure TLineObject.SetLayout(const Value: TTextLayout);
begin
  FLayout := Value;
  if FLayout <> nil then
    Layout.TopLeft := TPointF.Create(0, Rect.Top)
end;

procedure TLineObject.SetLocation(const Value: TPointF);
begin
  FRect.Location := Value;
  if FLayout <> nil then
    FLayout.TopLeft := TPointF.Create(0, Value.Y);
end;

procedure TLineObject.SetRect(const Value: TRectF);
begin
  FRect := Value;
  if FLayout <> nil then
    FLayout.TopLeft := TPointF.Create(0, FRect.Top);
end;

procedure TLineObject.SetSize(const Value: TSizeF);
begin
  FRect.Size := Value;
end;

function TLineObject.IsVerticallyIn(const AViewportRect: TRectF): Boolean;
begin
  Result := (FRect.Top < AViewportRect.Bottom) and (FRect.Bottom > AViewportRect.Top);
end;

function TLineObject.IsVisible(const AViewportRect: TRectF): Boolean;
begin
  Result := IntersectRect(AViewportRect, Rect);
end;

{ TLinesLayout }

procedure TLinesLayout.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TLinesLayout.CalculateLinePosition(const ALine: Integer);
var
  Line: TLineObject;
  LineTop: Single;
begin
  Line := FLines[ALine];

  if ALine = 0 then
    LineTop := 0
  else
    LineTop := FLines[ALine - 1].Rect.Bottom;

  Line.Location := TPointF.Create(Line.Location.X, LineTop);
  Exclude(Line.FState, TLineObject.TState.InvalidPosition);
end;

procedure TLinesLayout.CalculateLineSize(const ALine: Integer);
var
  Layout: TTextLayout;
  Line: TLineObject;
begin
  Line := FLines[ALine];

  CreateLineLayoutIfNotCreated(ALine);
  Layout := Line.Layout;
  Line.Size := TSizeF.Create(Max(1, Layout.Width), Layout.Height);
  Line.ReleaseLayoutIfNotVisible(ViewportRect);
  Exclude(Line.FState, TLineObject.TState.InvalidSize);
end;

procedure TLinesLayout.Clear;
begin
  FLines.Clear;
  UpdateVisibleIndexes;
  if IsUpdating then
    Include(FState, TState.ContentSizeChanged)
  else
    UpdateContentSize(TSizeF.Create(0, 0));
end;

constructor TLinesLayout.Create(const ALineSource: ITextLinesSource; const AScrollableContent: IScrollableContent);
begin
  inherited Create;
  FLines := TObjectList<TLineObject>.Create;
  FLineSource := ALineSource;
  FScrollableContent := AScrollableContent;
  FLineHeight := InvalidSize.Height;
  FUpdating := 0;
  FTextSettings := TTextSettings.Create(nil);
  FTextSettings.OnChanged := TextSettingsChanged;
  FFirstVisibleLineIndex := -1;
  FLastVisibleLineIndex := -1;
  FContentSize := TSizeF.Create(0, 0);
end;

function TLinesLayout.CreateLayout(const S: string): TTextLayout;
begin
  Result := TTextLayoutManager.DefaultTextLayout.Create;
  Result.BeginUpdate;
  try
    UpdateLayoutParams(Result);
    if S.IsEmpty then
      //Setting some string if text is empty to recreate layout,
      //if other properties have default values
      Result.Text := ' ';
    Result.Text := S;
  finally
    Result.EndUpdate;
  end;
end;

procedure TLinesLayout.CreateLayoutsForVisibleLinesIfNotCreated;
var
  I: Integer;
begin
  for I := Max(0, FFirstVisibleLineIndex) to Min(FLastVisibleLineIndex, Count - 1) do
    CreateLineLayoutIfNotCreated(I);
end;

function TLinesLayout.CreateLineLayoutIfNotCreated(const ALine: Integer): TTextLayout;
var
  Line: TLineObject;
begin
  Line := FLines[ALine];
  if Line.Layout = nil then
    Line.Layout := CreateLayout(FLineSource.GetLine(ALine));
  Result := Line.Layout;
end;

function TLinesLayout.GetPointByCaretPosition(const ACaretPosition: TCaretPosition): TPointF;
var
  Region: TRegion;
begin
  Region := GetRegionForRange(ACaretPosition, 1);
  if Length(Region) > 0 then
    Result := Region[0].TopLeft
  else
    Result := TPointF.Zero;
end;

function TLinesLayout.GetCaretPositionByPointInLine(const ALineIndex: Integer; const AHitPoint: TPointF; const ARoundToWord: Boolean): TCaretPosition;
var
  Line: TLineObject;
  Layout: TTextLayout;
  CenteredHitPoint: TPointF;
begin
  if not InRange(ALineIndex, 0, Count - 1) then
    Exit(TCaretPosition.Invalid);

  Line := FLines[ALineIndex];
  Layout := CreateLineLayoutIfNotCreated(ALineIndex);
  try
    // Above
    if AHitPoint.Y < Line.Rect.Top then
      CenteredHitPoint := TPointF.Create(AHitPoint.X, Line.Rect.Top + GetLineHeight / 2)
    else // Under
      CenteredHitPoint := TPointF.Create(AHitPoint.X, Line.Rect.Bottom - GetLineHeight / 2);

    Result.Line := ALineIndex;
    Result.Pos := Layout.PositionAtPoint(CenteredHitPoint, ARoundToWord);
  finally
    Line.ReleaseLayoutIfNotVisible(ViewportRect);
  end;

  if Result.Pos <> -1 then
    Exit;

  // If Point out of Line bounds, Trying to round the position to the border values
  if CenteredHitPoint.X < Line.Rect.Left then
    Result.Pos := 0
  else
    Result.Pos := FLineSource.Lines[ALineIndex].Length;
end;

function TLinesLayout.GetCount: Integer;
begin
  Result := FLines.Count;
end;

function TLinesLayout.GetItem(const Index: Integer): TLineObject;
begin
  Result := FLines[Index];
end;

function TLinesLayout.GetLineHeight: Single;
begin
  if FLineHeight <= 0 then
  begin
    TCanvasManager.MeasureCanvas.Font.Assign(FTextSettings.Font);
    FLineHeight := Round(TCanvasManager.MeasureCanvas.TextHeight('Ply|'));
  end;
  Result := FLineHeight;
end;

function TLinesLayout.GetCaretPositionByPoint(const AHitPoint: TPointF; const RoundToWord: Boolean): TCaretPosition;
var
  Point: TPointF;
  I: Integer;
  LPos: Integer;
  Rgn: TRegion;
  Line: TLineObject;
  Layout: TTextLayout;
begin
  Result := TCaretPosition.Invalid;
  if FLines.Count = 0  then
    Exit;

  for I := 0 to FLines.Count - 1 do
  begin
    Line := FLines[I];
    if not Line.ContainsPoint(AHitPoint) then
      Continue;

    Layout := CreateLineLayoutIfNotCreated(I);
    try
      Point := TPointF.Create(EnsureRange(AHitPoint.X, Layout.TextRect.Left, Layout.TextRect.Right), AHitPoint.Y);
      LPos := Layout.PositionAtPoint(Point, RoundToWord);
      if LPos < 0 then
      begin
        LPos := Layout.PositionAtPoint(TPointF.Create(AHitPoint.X, Layout.TextRect.Bottom - GetLineHeight / 2), RoundToWord);
        if LPos < 0 then
          raise ETextLayoutException.Create(SPointInTextLayoutError);
      end;
      if LPos >= 0 then
      begin
        // If user uses WordWrap mode, line break location does not contain control symbols,
        // so we shouldn't consider them.
        if (LPos > 0) and not IsWordWrap then
        begin
          Rgn := Layout.RegionForRange(TTextRange.Create(LPos, 1), RoundToWord);
          if (Length(Rgn) > 0) and (Rgn[0].Top > AHitPoint.Y) then
            Dec(LPos);
        end;
        Result := TCaretPosition.Create(I, LPos);
        Exit;
      end;
    finally
      Line.ReleaseLayoutIfNotVisible(ViewportRect);
    end;
  end;
  // We don't find caret position, So last try to determinate it
  if AHitPoint.Y > FLines.Last.Rect.Bottom then
    // Below
    Result := GetCaretPositionByPointInLine(FLines.Count - 1, AHitPoint)
  else
    // Above
    Result := GetCaretPositionByPointInLine(0, AHitPoint);
end;

function TLinesLayout.GetRegionForRange(const ACaretPosition: TCaretPosition; const ALength: Integer; const RoundToWord: Boolean): TRegion;
var
  I, J: Integer;
  LPos, RemainLength, LLength, LineLength: Integer;
  Layout: TTextLayout;
  LRegion: TRegion;
  Line: TLineObject;
begin
  SetLength(Result, 0);
  if not InRange(ACaretPosition.Line, 0, FLines.Count - 1) then
    Exit;

  LPos := ACaretPosition.Pos;
  RemainLength := ALength;
  for I := ACaretPosition.Line to FLines.Count - 1 do
  begin
    // Checking layout for contains a part of requested range
    if RemainLength <= 0 then
      Break;

    Line := FLines[I];

    LineLength := FLineSource.GetLine(I).Length;
    LLength := Min(RemainLength, LineLength - LPos);

    CreateLineLayoutIfNotCreated(I);
    Layout := Line.Layout;
    try
      LRegion := Layout.RegionForRange(TTextRange.Create(LPos, LLength), RoundToWord);
      for J := 0 to High(LRegion) do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := LRegion[J];
        Result[High(Result)].Top := Max(FLines[I].Rect.Top, LRegion[J].Top);
        Result[High(Result)].Bottom := Min(FLines[I].Rect.Bottom, LRegion[J].Bottom);
      end;
    finally
      Line.ReleaseLayoutIfNotVisible(ViewportRect);
    end;

    Inc(LPos, LLength);
    if LPos >= LineLength then
    begin
      LPos := 0;
      Dec(RemainLength);
    end;
    Dec(RemainLength, LLength + FLineSource.GetLineBreak.Length - 1);
  end;

  for I := Low(Result) to High(Result) do
    Result[I].Right := Min(Result[I].Right, TTextLayout.MaxLayoutSize.X);
end;

procedure TLinesLayout.DeleteLine(const AIndex: Integer);

  function CalculateContentWidth: Single;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to FLines.Count - 1 do
      Result := Max(Result, FLines[I].Size.Width);

    case FTextSettings.HorzAlign of
      TTextAlign.Center,
      TTextAlign.Trailing:
        Result := Max(Result, ViewportRect.Width);
    end;
  end;

var
  DeletedLineHeight: Single;
  NewContentSize: TSizeF;
begin
  DeletedLineHeight := FLines[AIndex].Size.Height;
  FLines.Delete(AIndex);

  if IsUpdating then
  begin
    Include(FState, TState.NeedAlignment);
    Include(FState, TState.ContentSizeChanged);
    MarkInvalidatePositionFrom(AIndex);
  end
  else
  begin
    OffsetLinesLocationFrom(AIndex, -DeletedLineHeight);

    UpdateVisibleIndexes;

    { Adjust content size}
    if DeletedLineHeight > 0 then
    begin
      NewContentSize.Height := ContentSize.Height - DeletedLineHeight;
      NewContentSize.Width := CalculateContentWidth;
      UpdateContentSize(NewContentSize);
    end;
  end;
end;

destructor TLinesLayout.Destroy;
begin
  FreeAndNil(FTextSettings);
  FreeAndNil(FLines);
  inherited;
end;

procedure TLinesLayout.EndUpdate;
begin
  if not IsUpdating then
    Exit;

  Dec(FUpdating);
  if not IsUpdating then
    RealignIfNeeded;
end;

procedure TLinesLayout.ExchangeLines(const AOldIndex, ANewIndex: Integer);

  procedure SwapLocation(const ALine1, ALine2: Integer);
  var
    Line1Pos: TPointF;
    Line1: TLineObject;
    Line2: TLineObject;
  begin
    Line1 := FLines[ALine1];
    Line2 := FLines[ALine2];

    Line1Pos := Line1.Location;
    Line1.Location := Line2.Location;
    Line2.Location := Line1Pos;
  end;

var
  MinIndex: Integer;
  MaxIndex: Integer;
  Offset: Single;
begin
  MinIndex := Min(AOldIndex, ANewIndex);
  MaxIndex := Max(AOldIndex, ANewIndex);
  if IsUpdating then
  begin
    FLines.Exchange(AOldIndex, ANewIndex);
    Include(FState, TState.NeedAlignment);
    // We could mark only range of lines between AOldIndex, ANewIndex. Because lines outside the range do not change
    // their positions. But since we are using optimization in MarkInvalidatePositionFrom, based on the continuity
    // condition of TState.InvalidPosition, we don't do it.
    MarkInvalidatePositionFrom(MinIndex);
  end
  else
  begin
    Offset := FLines[MaxIndex].Size.Height - FLines[MinIndex].Size.Height;
    OffsetLinesLocationBetween(MinIndex, MaxIndex, Offset);

    FLines.Exchange(AOldIndex, ANewIndex);

    SwapLocation(MinIndex, MaxIndex);
    UpdateVisibleIndexes;
  end;
end;

procedure TLinesLayout.InsertLine(const AIndex: Integer; const ALine: string);
var
  NewLine: TLineObject;
  NewContentSize: TSizeF;
begin
  NewLine := TLineObject.Create;
  FLines.Insert(AIndex, NewLine);
  if IsUpdating then
  begin
    Include(FState, TState.NeedAlignment);
    // New line affects only changes of positions on the next lines
    MarkInvalidatePositionFrom(AIndex + 1);
  end
  else
  begin
    CalculateLineSize(AIndex);
    CalculateLinePosition(AIndex);

    OffsetLinesLocationFrom(AIndex + 1, NewLine.Size.Height);

    UpdateVisibleIndexes;

    { Adjust content size}
    NewContentSize.Height := ContentSize.Height + NewLine.Size.Height;
    NewContentSize.Width := Max(ContentSize.Width, NewLine.Size.Width);
    UpdateContentSize(NewContentSize);
  end;
end;

function TLinesLayout.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

function TLinesLayout.IsWordWrap: Boolean;
begin
  Result := TextSettings.WordWrap or (TextSettings.HorzAlign <> TTextAlign.Leading);
end;

procedure TLinesLayout.MarkInvalidatePositionFrom(const AFromIndex: Integer);
var
  I: Integer;
  Line: TLineObject;
begin
  for I := AFromIndex to Count - 1 do
  begin
    Line := FLines[I];
    if TLineObject.TState.InvalidPosition in Line.State then
      Break;
    Line.InvalidatePosition;
  end;
end;

procedure TLinesLayout.OffsetLinesLocationBetween(const AStartLineIndex, AEndLineIndex: Integer; const AOffset: Single);
var
  I: Integer;
  Line: TLineObject;
begin
  if IsZero(AOffset) then
    Exit;

  for I := AStartLineIndex to AEndLineIndex do
  begin
    Line := FLines[I];
    Line.Location := Line.Location + TPointF.Create(0, AOffset);
    Line.ReleaseLayoutIfNotVisible(ViewportRect);
  end;
end;

procedure TLinesLayout.OffsetLinesLocationFrom(const ALineIndex: Integer; const AOffset: Single);
begin
  OffsetLinesLocationBetween(ALineIndex, FLines.Count - 1, AOffset);
end;

procedure TLinesLayout.Render(const ACanvas: TCanvas);
var
  I: Integer;
begin
  for I := Max(0, FirstVisibleLineIndex) to Min(LastVisibleLineIndex, Count - 1) do
    Items[I].Render(ACanvas);
end;

procedure TLinesLayout.Realign;

  function LinesConvexHull: TSizeF;
  var
    I: Integer;
    TotalWidth: Single;
  begin
    TotalWidth := 0;
    for I := 0 to FLines.Count - 1 do
      TotalWidth := Max(FLines[I].Size.Width, TotalWidth);

    if FLines.Count > 0 then
      Result := TSizeF.Create(TotalWidth, FLines.Last.Rect.Bottom)
    else
      Result := TSizeF.Create(0, 0);
  end;

  procedure RealignLinesHorizontally(var ANewContentSize: TSizeF);
  var
    ContentCenterX: Extended;
    I: Integer;
    Line: TLineObject;
  begin
    case FTextSettings.HorzAlign of
      TTextAlign.Center:
      begin
        ANewContentSize.Width := Max(ANewContentSize.Width, ViewportRect.Width - 1);
        ContentCenterX := ANewContentSize.Width / 2;
        for I := 0 to FLines.Count - 1 do
        begin
          Line := FLines[I];
          Line.Location := TPointF.Create(ContentCenterX - Line.Size.Width / 2, Line.Rect.Top);
        end;
      end;
      TTextAlign.Leading:
        for I := 0 to FLines.Count - 1 do
        begin
          Line := FLines[I];
          Line.Location := TPointF.Create(0, Line.Rect.Top);
        end;
      TTextAlign.Trailing:
      begin
        ANewContentSize.Width := Max(ANewContentSize.Width, ViewportRect.Width);

        for I := 0 to FLines.Count - 1 do
        begin
          Line := FLines[I];
          Line.Location := TPointF.Create(ANewContentSize.Width - Line.Size.Width, Line.Rect.Top);
        end;
      end;
    end;
  end;

var
  I: Integer;
  Line: TLineObject;
  ContentSize: TSizeF;
begin
  if FLineSource.Count <> FLines.Count then
    raise Exception.Create('Realign(): Text lines are not matching rendering lines');

  for I := 0 to FLines.Count - 1 do
  begin
    Line := FLines[I];
    if TLineObject.TState.InvalidLayout in Line.State then
    begin
      CreateLineLayoutIfNotCreated(I);
      RefreshLineLayout(I);
    end;

    if TLineObject.TState.InvalidSize in Line.State then
    begin
      Include(FState, TState.ContentSizeChanged);
      CalculateLineSize(I);
    end;

    if TLineObject.TState.InvalidPosition in Line.State then
      CalculateLinePosition(I);

    Line.ReleaseLayoutIfNotVisible(ViewportRect);
  end;

  if TState.ContentSizeChanged in FState then
  begin
    ContentSize := LinesConvexHull;
    RealignLinesHorizontally(ContentSize);
    UpdateContentSize(ContentSize);
    UpdateVisibleIndexes;
  end;
  FState := [];
end;

procedure TLinesLayout.RealignIfNeeded;
begin
  if not (TState.NeedAlignment in FState) then
    Exit;

  Realign;
end;

procedure TLinesLayout.ReplaceLine(const AIndex: Integer; const ALine: string);
var
  Line: TLineObject;
  OldLineHeight: Single;
  NewContentSize: TSizeF;
  HeightOffset: Single;
begin
  Line := FLines[AIndex];
  OldLineHeight := Line.Size.Height;
  if Line.Layout <> nil then
    Line.Layout.Text := ALine;

  if IsUpdating then
  begin
    Include(FState, TState.NeedAlignment);
    Line.InvalidateSize;
    MarkInvalidatePositionFrom(AIndex + 1);
  end
  else
  begin
    CalculateLineSize(AIndex);
    CalculateLinePosition(AIndex);

    HeightOffset := Line.Size.Height - OldLineHeight;
    OffsetLinesLocationFrom(AIndex + 1, HeightOffset);
    UpdateVisibleIndexes;

    { Adjust content size}
    NewContentSize.Height := ContentSize.Height + HeightOffset;
    NewContentSize.Width := Max(ContentSize.Width, Line.Size.Width);
    UpdateContentSize(NewContentSize);
  end;
end;

procedure TLinesLayout.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
  UpdateLayoutsColor;
end;

procedure TLinesLayout.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings.Assign(Value);
end;

procedure TLinesLayout.SetViewportRect(const Value: TRectF);

  procedure MarkLinesForRealign;
  var
    I: Integer;
  begin
    for I := 0 to Count - 1 do
      FLines[I].Invalidate;
  end;

var
  IsWidthChanged: Boolean;
  IsHeightChanged: Boolean;
  IsXPositionChanged: Boolean;
  IsYPositionChanged: Boolean;
begin
  IsHeightChanged := not SameValue(FViewportRect.Size.Height, Value.Size.Height, TEpsilon.Position);
  IsWidthChanged := not SameValue(FViewportRect.Size.Width, Value.Size.Width, TEpsilon.Position);
  IsXPositionChanged := not SameValue(FViewportRect.Left, Value.Left, TEpsilon.Position);
  IsYPositionChanged := not SameValue(FViewportRect.Top, Value.Top, TEpsilon.Position);

  FViewportRect := Value;
  if IsWordWrap and IsWidthChanged then
  begin
    // Changing the width of the content may cause text hyphenation, so we have to recalculate lines.
    MarkLinesForRealign;
    Realign;
  end;

  if IsHeightChanged or IsYPositionChanged then
    UpdateVisibleIndexes
  else if IsXPositionChanged then
    // If we have short lines of text in viewport and user scrolls text by horizontally,
    // text line can be disappeared (it's a part of optimization).
    CreateLayoutsForVisibleLinesIfNotCreated;
end;

procedure TLinesLayout.TextSettingsChanged(Sender: TObject);

  procedure InvalidateLines;
  var
    I: Integer;
    Line: TLineObject;
  begin
    for I := 0 to Count - 1 do
    begin
      Line := FLines[I];
      Line.FState := [TLineObject.TState.InvalidSize, TLineObject.TState.InvalidPosition];
      Line.InvalidateLayout;
    end;
  end;

begin
  FLineHeight := InvalidSize.Height;

  InvalidateLines;
  Realign;
end;

procedure TLinesLayout.UpdateContentSize(const AContentSize: TSizeF);
begin
  if IsWordWrap then
    FContentSize := AContentSize
  else
    FContentSize := TSizeF.Create(AContentSize.Width + Max(1, CaretWidth), AContentSize.Height);

  FScrollableContent.ContentSize := FContentSize;
end;

procedure TLinesLayout.UpdateLayoutParams(const ALayout: TTextLayout);
begin
  ALayout.BeginUpdate;
  try
    ALayout.HorizontalAlign := TextSettings.HorzAlign;
    ALayout.Font := TextSettings.Font;
    ALayout.Color := TextSettings.FontColor;
    ALayout.WordWrap := IsWordWrap;
    if IsWordWrap then
      ALayout.MaxSize := TPointF.Create(ViewportRect.Width - 1, TTextLayout.MaxLayoutSize.Y)
    else
      ALayout.MaxSize := TTextLayout.MaxLayoutSize;
    ALayout.Opacity := Opacity;
  finally
    ALayout.EndUpdate;
  end;
end;

procedure TLinesLayout.UpdateLayoutsColor;
var
  I: Integer;
begin
  for I := 0 to FLines.Count - 1 do
    if FLines[I].Layout <> nil then
    begin
      FLines[I].Layout.Color := TextSettings.FontColor;
      FLines[I].Layout.Opacity := Opacity;
    end;
end;

procedure TLinesLayout.UpdateVisibleIndexes;
type
  TDirection = (Up, Down);

  function SkipInvisibleLines(const AIndex: Integer; const ADirection: TDirection): Integer;
  begin
    Result := AIndex;
    case ADirection of
      TDirection.Up:
        while (Result >= 0) and not FLines[Result].IsVerticallyIn(ViewportRect) do
          Dec(Result);
      TDirection.Down:
        while (Result < Count) and not FLines[Result].IsVerticallyIn(ViewportRect) do
          Inc(Result);
    end;
  end;

  function SkipVisibleLines(const AIndex: Integer; const ADirection: TDirection): Integer;
  begin
    Result := AIndex;
    case ADirection of
      TDirection.Up:
        while (Result >= 0) and FLines[Result].IsVerticallyIn(ViewportRect) do
          Dec(Result);
      TDirection.Down:
        while (Result < Count) and FLines[Result].IsVerticallyIn(ViewportRect) do
          Inc(Result);
    end;
  end;

  function FindFirstVisibleLine(const AStartIndex: Integer): Integer;
  var
    I: Integer;
  begin
    if Count = 0 then
      Exit(-1);

    I := EnsureRange(AStartIndex, 0, Count - 1);
    if FLines[I].IsVerticallyIn(ViewportRect) then
      // The line is in viewport
      Result := SkipVisibleLines(I, TDirection.Up) + 1
    else if FLines[I].Rect.Bottom <= ViewportRect.Top then
      // The line is above viewport
      Result := SkipInvisibleLines(I, TDirection.Down)
    else
    begin
      // The line is below viewport
      I := SkipInvisibleLines(I, TDirection.Up);
      Result := SkipVisibleLines(I, TDirection.Up) + 1;
    end;
    Result := EnsureRange(Result, -1, Count - 1);
  end;

  function FindLastFirstVisibleLine(const AStartIndex: Integer): Integer;
  var
    I: Integer;
  begin
    if Count = 0 then
      Exit(-1);

    I := EnsureRange(AStartIndex, 0, Count - 1);
    if FLines[I].IsVerticallyIn(ViewportRect) then
      // The line is in viewport
      Result  := SkipVisibleLines(I, TDirection.Down) - 1
    else if FLines[I].Rect.Bottom <= ViewportRect.Top then
    begin
      // The line is above viewport
      I := SkipInvisibleLines(I, TDirection.Down);
      Result := SkipVisibleLines(I, TDirection.Down) - 1;
    end
    else
      // The line is below viewport
      Result := SkipInvisibleLines(I, TDirection.Up);

    Result := EnsureRange(Result, -1, Count - 1);
  end;

var
  I: Integer;
begin
  { Release layouts for hidden lines }

  for I := Max(0, FFirstVisibleLineIndex) to Min(FLastVisibleLineIndex, Count - 1) do
    FLines[I].ReleaseLayoutIfNotVisible(ViewportRect);

  { Definition first and last visible lines indexes }

  FFirstVisibleLineIndex := FindFirstVisibleLine(FFirstVisibleLineIndex);
  FLastVisibleLineIndex := FindLastFirstVisibleLine(FLastVisibleLineIndex);

  { Create layouts for visible lines }

  CreateLayoutsForVisibleLinesIfNotCreated;
end;

procedure TLinesLayout.RefreshLineLayout(const ALine: Integer);
var
  Line: TLineObject;
begin
  Line := FLines[ALine];

  UpdateLayoutParams(Line.Layout);
  Exclude(Line.FState, TLineObject.TState.InvalidLayout);
end;

{ TSpellingWord }

constructor TSpellingWord.Create(const APosition: TCaretPosition; const AWord: string; const ABounds: TRegion);
begin
  FPosition := APosition;
  FLength := AWord.Length;
  FBounds := ABounds;
  FWord := AWord;
end;

function TSpellingWord.HasBounds: Boolean;
begin
  Result := System.Length(Bounds) > 0;
end;

procedure TSpellingWord.InvalidateBounds;
begin
  Bounds := nil;
end;

function TSpellingWord.PosAtCurrentPos(const APosition: TCaretPosition): Boolean;
begin
  Result := (FPosition.Line = APosition.Line) and InRange(APosition.Pos, FPosition.Pos, FPosition.Pos + FLength);
end;

function TSpellingWord.TextRange: TTextRange;
begin
  Result := TTextRange.Create(FPosition.Pos, FLength);
end;

{ TMemoSpellingManager }

procedure TMemoSpellingManager.ClearMenuItems;
var
  I: Integer;
begin
  for I := 0 to FMenuItems.Count - 1 do
    FMenuItems[I].Parent := nil;
  FMenuItems.Clear;
end;

constructor TMemoSpellingManager.Create(const ALinesSource: ITextLinesSource);
begin
  inherited Create;
  FLinesSource := ALinesSource;
  TPlatformServices.Current.SupportsPlatformService(IFMXSpellCheckerService, FSpellService);

  FMenuItems := TList<TMenuItem>.Create;
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  FUnderlineStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColorRec.Red);
  FUnderlineStroke.Dash := TStrokeDash.Dot;
  FSpellingWords := TObjectList<TSpellingWord>.Create;
end;

destructor TMemoSpellingManager.Destroy;
begin
  FLinesSource := nil;
  FreeAndNil(FSpellingWords);
  FreeAndNil(FUnderlineStroke);
  FreeAndNil(FFill);
  FreeAndNil(FMenuItems);
  FSpellService := nil;
  inherited;
end;

procedure TMemoSpellingManager.DoReplaceWord(const ASpellingWord: TSpellingWord; const ASuggestion: string);
begin
  if Assigned(FOnReplaceWord) then
    FOnReplaceWord(ASpellingWord, ASuggestion);
end;

procedure TMemoSpellingManager.FindSpellingErrorsInLine(const ALineIndex: Integer);
var
  Shift, BegPos, EndPos: Integer;
  Line: string;
  SpellingWord: string;
begin
  if FSpellService = nil then
    Exit;

  Shift := 0;
  Line := FLinesSource.GetLine(ALineIndex);
  while not Line.IsEmpty do
  begin
    if FMX.Text.FindWordBound(Line, 0, BegPos, EndPos) then
    begin
      SpellingWord := Line.Substring(BegPos, EndPos - BegPos + 1);
      if Length(FSpellService.CheckSpelling(SpellingWord)) > 0 then
        FSpellingWords.Add(TSpellingWord.Create(TCaretPosition.Create(ALineIndex, BegPos + Shift), SpellingWord, nil));
    end
    else
      EndPos := BegPos;
    Inc(Shift, EndPos + 1);
    Line := Line.Remove(0, EndPos + 1);
  end;
end;

procedure TMemoSpellingManager.FindSpellingErrorsInLines;
var
  I: Integer;
begin
  FHightlightRect := TRectF.Empty;
  for I := 0 to FLinesSource.Count - 1 do
    FindSpellingErrorsInLine(I)
end;

function TMemoSpellingManager.FindSpellingWordByCaret(const ACaretPosition: TCaretPosition;
  out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to FSpellingWords.Count - 1 do
    if FSpellingWords[I].PosAtCurrentPos(ACaretPosition) then
    begin
      AIndex := I;
      Exit(True);
    end;
  Result := False;
end;

function TMemoSpellingManager.GetListOfPrepositions(const ACaretPosition: TCaretPosition): TArray<string>;
var
  BP, EP: Integer;
  Line: string;
begin
  if (FSpellService <> nil) and (FLinesSource.Count > 0) and (ACaretPosition.Line >= 0) and (ACaretPosition.Pos >= 0) then
  begin
    Line := FLinesSource.GetLine(ACaretPosition.Line);
    if FMX.Text.FindWordBound(Line, ACaretPosition.Pos, BP, EP) then
      Result := FSpellService.CheckSpelling(Line.Substring(BP, EP - BP + 1))
    else
      Result := nil;
  end
  else
    Result := nil;
end;

procedure TMemoSpellingManager.HideHighlightSpell;
begin
  FHightlightRect := TRectF.Empty;
end;

procedure TMemoSpellingManager.HighlightSpell(const ALines: TLinesLayout; const ACaretPosition: TCaretPosition);
var
  StartPos, EndPos: Integer;
  Region: TRegion;
begin
  if (FLinesSource.Count > 0) and (ACaretPosition.Line >= 0) and (ACaretPosition.Pos >= 0) and
    FMX.Text.FindWordBound(FLinesSource.GetLine(ACaretPosition.Line), ACaretPosition.Pos, StartPos, EndPos) then
  begin
    Region := ALines.GetRegionForRange(TCaretPosition.Create(ACaretPosition.Line, StartPos), EndPos - StartPos + 1);
    if Length(Region) > 0 then
      FHightlightRect := Region[0]
    else
      FHightlightRect := TRectF.Empty;
  end;
end;

procedure TMemoSpellingManager.DrawHightlightSpellingWords(const ALine: TLinesLayout; const AViewportSize: TSizeF; const ACanvas: TCanvas; const AOpacity: Single);

  procedure UnderlineWord(const ASpellingWord: TSpellingWord);
  var
    I: Integer;
    WordRect: TRectF;
  begin
    for I := Low(ASpellingWord.Bounds) to High(ASpellingWord.Bounds) do
    begin
      WordRect := ASpellingWord.Bounds[I];
      ACanvas.DrawLine(TPointF.Create(WordRect.Left, WordRect.Bottom), WordRect.BottomRight, AOpacity, FUnderlineStroke);
    end;
  end;

var
  TmpRect: TRectF;
  I: Integer;
  Line: TLineObject;
  SpellingWord: TSpellingWord;
begin
  if FSpellingWords.Count = 0 then
    Exit;

  TmpRect := TRectF.Create(0, 0, AViewportSize.Width, AViewportSize.Height);
  for I := 0 to FSpellingWords.Count - 1 do
  begin
    SpellingWord := FSpellingWords[I];
    Line := ALine[SpellingWord.Position.Line];

    if RectsIntersect(TmpRect, Line.Rect) then
    begin
      if not SpellingWord.HasBounds and (Line.Layout <> nil) then
        SpellingWord.Bounds := Line.Layout.RegionForRange(SpellingWord.TextRange);

      UnderlineWord(SpellingWord);
    end
    else
      FSpellingWords[I].InvalidateBounds;
  end;
  if not FHightlightRect.IsEmpty then
    ACanvas.FillRect(FHightlightRect, 0.2, FFill);
end;

function TMemoSpellingManager.IsWordWrong(const ACaretPosition: TCaretPosition): Boolean;
var
  Ignore: Integer;
begin
  Result := FindSpellingWordByCaret(ACaretPosition, Ignore);
end;

procedure TMemoSpellingManager.RemoveSpellingErrorsForLine(const ALineIndex: Integer);
var
  I: Integer;
  SpellingWord: TSpellingWord;
  Position: TCaretPosition;
begin
  for I := FSpellingWords.Count - 1 downto 0 do
  begin
    SpellingWord := FSpellingWords[I];
    if SpellingWord.Position.Line = ALineIndex then
      FSpellingWords.Delete(I)
    else if SpellingWord.Position.Line > ALineIndex then
    begin
      Position := SpellingWord.Position;
      Position.DecrementLine;
      SpellingWord.Position := Position;
      SpellingWord.InvalidateBounds;
    end;
  end;
end;

procedure TMemoSpellingManager.Reset;
begin
  ClearMenuItems;
  FSpellingWords.Clear;
  FHightlightRect := TRectF.Empty;
end;

procedure TMemoSpellingManager.ResetBounds;
var
  I: Integer;
begin
  for I := 0 to FSpellingWords.Count - 1 do
    FSpellingWords[I].InvalidateBounds;
end;

procedure TMemoSpellingManager.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TMemoSpellingManager.SetStroke(const Value: TStrokeBrush);
begin
  FUnderlineStroke.Assign(Value);
end;

procedure TMemoSpellingManager.Spell(var ACaretPosition: TCaretPosition; const AWord: string);
var
  I: Integer;
  Index: Integer;
  SpellingWord: TSpellingWord;
begin
  if FindSpellingWordByCaret(ACaretPosition, Index) then
    try
      SpellingWord := FSpellingWords[Index];
      DoReplaceWord(SpellingWord, AWord);
      HideHighlightSpell;
      for I := Index + 1 to FSpellingWords.Count - 1 do
        FSpellingWords[I].InvalidateBounds;
      ACaretPosition := TCaretPosition.Create(ACaretPosition.Line, SpellingWord.Position.Pos + AWord.Length);
    finally
      FSpellingWords.Delete(Index);
    end;
end;

procedure TMemoSpellingManager.SpellFixContextMenuHandler(Sender: TObject);
var
  SpellingWord: TSpellingWord;
begin
  if Sender is TMenuItem then
  begin
    SpellingWord := FSpellingWords[TMenuItem(Sender).Tag];
    DoReplaceWord(SpellingWord, TMenuItem(Sender).Text);
  end;
end;

procedure TMemoSpellingManager.UpdateHightlightRect(const ALines: TLinesLayout; const AViewportSize: TSizeF; const AOffset: TPointF);
var
  Content: TRectF;

  function IsVisibleLine(const ALine: Integer): Boolean;
  begin
    Result := RectsIntersect(Content, ALines[ALine].Rect);
  end;

  procedure OffsetBounds(const ASpellingWord: TSpellingWord);
  var
    I: Integer;
  begin
    for I := Low(ASpellingWord.Bounds) to High(ASpellingWord.Bounds) do
      ASpellingWord.Bounds[I].Offset(AOffset)
  end;

var
  I: Integer;
  SpellingWord: TSpellingWord;
begin
  if not FHightlightRect.IsEmpty then
    FHightlightRect.Offset(AOffset);

  Content := TRectF.Create(0, 0, AViewportSize.Width, AViewportSize.Height);
  for I := 0 to FSpellingWords.Count - 1 do
  begin
    SpellingWord := FSpellingWords[I];
    if SpellingWord.HasBounds and IsVisibleLine(SpellingWord.Position.Line) then
      OffsetBounds(SpellingWord)
    else
      SpellingWord.InvalidateBounds;
  end;
end;

procedure TMemoSpellingManager.AddSuggestionsToPopupMenu(const APopupMenu: TPopupMenu; const ACaretPosition: TCaretPosition);

  function CreateMenuItemForSuggestion(const AWord: string; const AWordIndex: Integer): TMenuItem;
  begin
    Result := TMenuItem.Create(APopupMenu);
    Result.Text := AWord;
    Result.Tag := AWordIndex;
    Result.Font.Style := Result.Font.Style + [TFontStyle.fsBold];
    Result.OnClick := SpellFixContextMenuHandler;
  end;

  procedure AddMenuItemToPopup(const AMenuItem: TMenuItem);
  begin
    APopupMenu.InsertObject(FMenuItems.Count, AMenuItem);
    FMenuItems.Add(AMenuItem);
  end;

var
  Suggestions: TArray<string>;
  MenuItem: TMenuItem;
  Index: Integer;
  Suggestion: string;
  SpellingWord: string;
begin
  ClearMenuItems;

  if (FSpellService <> nil) and FindSpellingWordByCaret(ACaretPosition, Index) then
  begin
    SpellingWord := FSpellingWords[Index].Word;
    Suggestions := FSpellService.CheckSpelling(SpellingWord);
    if Length(Suggestions) > 0 then
    begin
      { Suggestions }
      for Suggestion in Suggestions do
      begin
        MenuItem := CreateMenuItemForSuggestion(Suggestion, Index);
        AddMenuItemToPopup(MenuItem);
      end;
      { Separator }
      MenuItem := TMenuItem.Create(APopupMenu);
      MenuItem.Text := SMenuSeparator;
      AddMenuItemToPopup(MenuItem);
    end;
  end;
end;

{ TSelectionController }

constructor TSelectionController.Create(const ALineSource: ITextLinesSource);
begin
  inherited Create;
  FLineSource := ALineSource;
  Reset;
end;

destructor TSelectionController.Destroy;
begin
  FLineSource := nil;
  inherited;
end;

procedure TSelectionController.DoSelectionChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self, BeginPos, EndPos - BeginPos);
end;

function TSelectionController.IsSelected: Boolean;
begin
  Result := FSelStart <> FSelFinish;
end;

procedure TSelectionController.Reset;
begin
  SetRange(TCaretPosition.Zero, TCaretPosition.Zero);
end;

function TSelectionController.SelBegin: TCaretPosition;
begin
  if FSelStart < FSelFinish then
    Result := FSelStart
  else
    Result := FSelFinish;
end;

function TSelectionController.BeginPos: Integer;
begin
  Result := FLineSource.PosToTextPos(SelBegin);
end;

function TSelectionController.SelEnd: TCaretPosition;
begin
  if FSelStart < FSelFinish then
    Result := FSelFinish
  else
    Result := FSelStart;
end;

function TSelectionController.EndPos: Integer;
begin
  Result := FLineSource.PosToTextPos(SelEnd);
end;

function TSelectionController.GetLength: Integer;
begin
  Result := FLineSource.PosToTextPos(Finish) - FLineSource.PosToTextPos(Start);
end;

procedure TSelectionController.SetSelFinish(const Value: TCaretPosition);
begin
  SetRange(Start, Value);
end;

procedure TSelectionController.SetSelStart(const Value: TCaretPosition);
begin
  SetRange(Value, Finish);
end;

procedure TSelectionController.SetLength(const Value: Integer);
begin
  Finish := FLineSource.TextPosToPos(FLineSource.PosToTextPos(Start) + Value);
end;

procedure TSelectionController.HoldSelection;
begin
                                                                                                                          
  FHoldSelBegin := SelBegin;
  FHoldSelEnd := SelEnd;
end;

procedure TSelectionController.SetRange(const AStartPos, ALength: Integer);
begin
  SetRange(FLineSource.TextPosToPos(AStartPos), FLineSource.TextPosToPos(AStartPos + ALength));
end;

procedure TSelectionController.SetRange(const ASelStart, ASelEnd: TCaretPosition);
begin
  if (FSelStart <> ASelStart) or (FSelFinish <> ASelEnd) then
  begin
    FSelStart := ASelStart;
    FSelFinish := ASelEnd;

    DoSelectionChanged;
  end;
end;

procedure TSelectionController.SetStartPos(const Value: Integer);
begin
  Start := FLineSource.TextPosToPos(Value);
end;

procedure TSelectionController.UnholdSelection;
begin
  FHoldSelBegin := TCaretPosition.Invalid;
  FHoldSelEnd := TCaretPosition.Invalid;
end;

{ TAutoscrollController }

procedure TAutoscrollController.AutoScrollHandler(Sender: TObject);
var
  NeedStop: Boolean;
begin
  NeedStop := False;
  try
    DoScroll(NeedStop);
  finally
    if NeedStop then
      Stop;
  end;
end;

constructor TAutoscrollController.Create;
begin
  FStartAutoScrollTimer := TTimer.Create(nil);
  FStartAutoScrollTimer.Enabled := False;
  FStartAutoScrollTimer.Interval := DefaultStartScrollDelay;
  FStartAutoScrollTimer.OnTimer := StartAutoScrollHandler;

  FAutoScrollTimer := TTimer.Create(nil);
  FAutoScrollTimer.Interval := DefaultScrollStepInterval;
  FAutoScrollTimer.OnTimer := AutoScrollHandler;
  FAutoScrollTimer.Enabled := False;
end;

destructor TAutoscrollController.Destroy;
begin
  FreeAndNil(FAutoScrollTimer);
  FreeAndNil(FStartAutoScrollTimer);
  inherited;
end;

procedure TAutoscrollController.DoScroll(var AStop: Boolean);
begin
  if Assigned(FOnScroll) then
    FOnScroll(FScrollDirection, AStop);
end;

function TAutoscrollController.GetScrollInterval: Integer;
begin
  Result := FAutoScrollTimer.Interval;
end;

function TAutoscrollController.GetStartDelay: Integer;
begin
  Result := FStartAutoScrollTimer.Interval;
end;

procedure TAutoscrollController.SetScrollInterval(const Value: Integer);
begin
  FAutoScrollTimer.Interval := Max(0, Value);
end;

procedure TAutoscrollController.SetStartDelay(const Value: Integer);
begin
  FStartAutoScrollTimer.Interval := Max(0, Value);
end;

procedure TAutoscrollController.Start(const ADirection: TAutoscrollDirection);
begin
  FScrollDirection := ADirection;
  FStartAutoScrollTimer.Enabled := not FAutoScrollTimer.Enabled;
end;

procedure TAutoscrollController.StartAutoScrollHandler(Sender: TObject);
begin
  FStartAutoScrollTimer.Enabled := False;
  FAutoScrollTimer.Enabled := True;
end;

procedure TAutoscrollController.Stop;
begin
  FStartAutoScrollTimer.Enabled := False;
  FAutoScrollTimer.Enabled := False;
end;

{ Only for update }

type
  TIMEParagraphLine = record
    Range: TTextRange;
    LineRect: TRectF;
  end;
  TIMEParagraphLines = TArray<TIMEParagraphLine>;

  /// <summary>Extension for TextLayout for layout IME text as paragraph with first line offset.</summary>
  TIMETextLayout = class
  public const
    MarkedTextBackgroundOpacity = 0.9;
  {$IFDEF MACOS}
    MaxThickness = 3;
  {$ELSE}
    MaxThickness = 4;
  {$ENDIF}
  private
    FTextLayout: TTextLayout;
    FFirstLineOffset: Single;
    FMaxSize: TSizeF;
    FTopLeft: TPointF;
    FText: string;
    FColor: TAlphaColor;
    FOpacity: Single;
    FNeedRecalculate: Boolean;
    FMarkedTextAttributes: TArray<TMarkedTextAttribute>;
    { Paragraph Metrics }
    FLines: TIMEParagraphLines;
    procedure SetTopLeft(const Value: TPointF);
    procedure SetMaxSize(const Value: TSizeF);
    procedure SetText(const Value: string);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure SetOpacity(const Value: Single);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetRightToLeft(const Value: Boolean);
    function GetRightToLeft: Boolean;
    function GetLines: TIMEParagraphLines;
    function GetBoundsRect: TRectF;
  private
    function GetTextRangeFor(const ARegion: TRectF): TTextRange;
    function GetAbsoluteTextRectFor(const ARegion: TRectF): TRectF;
    procedure RecalculateMetrics;
    { Painting }
    procedure DrawBackground(const ACanvas: TCanvas);
    procedure DrawLines(const ACanvas: TCanvas);
    procedure UnderlineLine(const ACanvas: TCanvas; const ARange: TTextRange);
    procedure UnderlineRegion(const ACanvas: TCanvas; const ARegions: TRegion; const ANeedAddSpace: Boolean);
    procedure ApplyMarkedTextAttribute(const ACanvas: TCanvas; const AAttribute: TMarkedTextAttribute);
    class function MarkedTextAttributeToStrokeDash(const AAttribute: TMarkedTextAttribute): TStrokeDash;
    class function MarkedTextAttributeToThickness(const AAttribute: TMarkedTextAttribute): Single;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Renders IME text according specified properties on the <c>ACanvas</c>.</summary>
    procedure Render(const ACanvas: TCanvas);

    procedure BeginUpdate;
    procedure EndUpdate;

    /// <summary>Returns Top-Left corner of character bounds at specified position <c>APos</c>.</summary>
    /// <remarks>If <c>APos</c> is invalid value, the it retruns <c>TPointF.Zero</c>.</remarks>
    function GetPositionPoint(const APos: Integer): TPointF;

    /// <summary>The calculated the paragraph bounds.</summary>
    property BoundsRect: TRectF read GetBoundsRect;
  public
    /// <summary>Paragraph text.</summary>
    property Text: string read FText write SetText;
    /// <summary>Coordinates of Top-Left corner of paragraph bounds.</summary>
    property TopLeft: TPointF read FTopLeft write SetTopLeft;
    /// <summary>Max available size for rendering paragraph.</summary>
    property MaxSize: TSizeF read FMaxSize write SetMaxSize;
    /// <summary>IME text attributes.</summary>
    property TextAttributes: TArray<TMarkedTextAttribute> read FMarkedTextAttributes write FMarkedTextAttributes;
    /// <summary>Local horizontal offset (from TopLeft) of first paragraph line.</summary>
    property FirstLineOffset: Single read FFirstLineOffset write FFirstLineOffset;
    /// <summary>Text font.</summary>
    property Font: TFont read GetFont write SetFont;
    /// <summary>Text color.</summary>
    property Color: TAlphaColor read FColor write SetColor;
    /// <summary>Text opacity.</summary>
    property Opacity: Single read FOpacity write SetOpacity;
    /// <summary>Should be used Right-To-Left rendering?</summary>
    property RightToLeft: Boolean read GetRightToLeft write SetRightToLeft;
    /// <summary>Calculated parameters of the paragraph lines.</summary>
    property Lines: TIMEParagraphLines read GetLines;
  end;

{ TIMETextLayout }

procedure TIMETextLayout.ApplyMarkedTextAttribute(const ACanvas: TCanvas; const AAttribute: TMarkedTextAttribute);
begin
  ACanvas.Stroke.Dash := MarkedTextAttributeToStrokeDash(AAttribute);
  ACanvas.Stroke.Thickness := MarkedTextAttributeToThickness(AAttribute);
end;

procedure TIMETextLayout.BeginUpdate;
begin
  FTextLayout.BeginUpdate;
end;

constructor TIMETextLayout.Create;
begin
  FOpacity := 1;
  FColor := TAlphaColorRec.Black;
  FTextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  FTextLayout.WordWrap := True;
  FTextLayout.Color := FColor;
  FTextLayout.Opacity := FOpacity;
end;

destructor TIMETextLayout.Destroy;
begin
  FreeAndNil(FTextLayout);
  inherited;
end;

procedure TIMETextLayout.DrawBackground(const ACanvas: TCanvas);
const
  LightTextThreshold = 0.5;
var
  BackgroundRect: TRectF;
  Line: TIMEParagraphLine;
begin
  if Luminance(FTextLayout.Color) > LightTextThreshold then
    ACanvas.Fill.Color := TAlphaColorRec.Black
  else
    ACanvas.Fill.Color := TAlphaColorRec.White;

  for Line in Lines do
  begin
    BackgroundRect := Line.LineRect;
    BackgroundRect.Inflate(0, 0, 0, MaxThickness);
    ACanvas.FillRect(BackgroundRect, MarkedTextBackgroundOpacity * FOpacity);
  end;
end;

procedure TIMETextLayout.DrawLines(const ACanvas: TCanvas);
var
  Line: TIMEParagraphLine;
begin
  FTextLayout.Opacity := FOpacity;
  FTextLayout.MaxSize := FMaxSize;
  for Line in Lines do
  begin
    FTextLayout.Text := FText.Substring(Line.Range.Pos, Line.Range.Length);
    FTextLayout.TopLeft := Line.LineRect.TopLeft;
    FTextLayout.RenderLayout(ACanvas);
    UnderlineLine(ACanvas, Line.Range);
  end;
end;

procedure TIMETextLayout.EndUpdate;
begin
  FTextLayout.EndUpdate;
end;

function TIMETextLayout.GetAbsoluteTextRectFor(const ARegion: TRectF): TRectF;
begin
  Result := TRectF.Create(FTopLeft + ARegion.TopLeft, ARegion.Width, ARegion.Height);
end;

function TIMETextLayout.GetBoundsRect: TRectF;
var
  Metric: TIMEParagraphLine;
begin
  Result := TRectF.Create(FTopLeft, 0, 0);
  for Metric in Lines do
    Result := TRectF.Union(Result, Metric.LineRect);
end;

function TIMETextLayout.GetFont: TFont;
begin
  Result := FTextLayout.Font;
end;

function TIMETextLayout.GetLines: TIMEParagraphLines;
begin
  if FNeedRecalculate then
  begin
    RecalculateMetrics;
    FNeedRecalculate := False;
  end;

  Result := FLines;
end;

function TIMETextLayout.GetPositionPoint(const APos: Integer): TPointF;

  function FindMeasurement(const APos: Integer; out AMeasurement: TIMEParagraphLine): Boolean;
  var
    I: Integer;
  begin
    for I := Low(Lines) to High(Lines) do
      if Lines[I].Range.InRange(APos) then
      begin
        AMeasurement := Lines[I];
        Exit(True);
      end;

    Result := False;
  end;

  function LocatePosition(const AMeasurement: TIMEParagraphLine; const APos: Integer): TPointF;
  var
    Regions: TRegion;
  begin
    FTextLayout.TopLeft := AMeasurement.LineRect.TopLeft;
    FTextLayout.MaxSize := AMeasurement.LineRect.Size;
    FTextLayout.Text := FText.Substring(AMeasurement.Range.Pos, AMeasurement.Range.Length);

    Regions := FTextLayout.RegionForRange(TTextRange.Create(APos - AMeasurement.Range.Pos, 1));
    Result := Regions[0].TopLeft;
  end;

var
  Measurement: TIMEParagraphLine;
begin
  if FindMeasurement(APos, Measurement) then
    Result := LocatePosition(Measurement, APos)
  else if APos >= FText.Length then
  begin
    Measurement := FLines[High(FLines)];
    Result := LocatePosition(Measurement, APos);
  end
  else
    Result := TPointF.Zero;
end;

function TIMETextLayout.GetRightToLeft: Boolean;
begin
  Result := FTextLayout.RightToLeft;
end;

function TIMETextLayout.GetTextRangeFor(const ARegion: TRectF): TTextRange;
var
  StartPos: Integer;
  EndPos: Integer;
begin
  StartPos := FTextLayout.PositionAtPoint(TPointF.Create(0, ARegion.CenterPoint.Y));
  if StartPos = -1 then
    StartPos := 0;
  EndPos := FTextLayout.PositionAtPoint(TPointF.Create(ARegion.Width, ARegion.CenterPoint.Y));
  if EndPos = -1 then
    EndPos := 1;

  Result.Pos := StartPos;
  Result.Length := EndPos - StartPos;
end;

class function TIMETextLayout.MarkedTextAttributeToStrokeDash(const AAttribute: TMarkedTextAttribute): TStrokeDash;
begin
{$IFDEF MSWINDOWS}
  case AAttribute of
    TMarkedTextAttribute.Input:
      Result := TStrokeDash.Dash;
    TMarkedTextAttribute.TargetConverted,
    TMarkedTextAttribute.Converted,
    TMarkedTextAttribute.TargetNotConverted:
      Result := TStrokeDash.Solid;
    TMarkedTextAttribute.InputError:
      Result := TStrokeDash.Dot
  else
    Result := TStrokeDash.Solid;
  end;
{$ENDIF}
{$IFDEF MACOS}
  Result := TStrokeDash.Solid;
{$ENDIF}
end;

class function TIMETextLayout.MarkedTextAttributeToThickness(const AAttribute: TMarkedTextAttribute): Single;
begin
{$IFDEF MSWINDOWS}
  case AAttribute of
    TMarkedTextAttribute.Input,
    TMarkedTextAttribute.Converted,
    TMarkedTextAttribute.InputError:
      Result := 1;
    TMarkedTextAttribute.TargetConverted:
      Result := 2;
    TMarkedTextAttribute.TargetNotConverted:
      Result := 4;
  else
    Result := 1;
  end;
{$ENDIF}
{$IFDEF MACOS}
  case AAttribute of
    TMarkedTextAttribute.Converted:
      Result := 1;
    TMarkedTextAttribute.TargetNotConverted:
      Result := 2;
  else
    Result := 1;
  end;
{$ENDIF}
end;

procedure TIMETextLayout.RecalculateMetrics;
var
  Regions: TRegion;
  Region: TRectF;
  I: Integer;
  FirstLine: TIMEParagraphLine;
  Range: TTextRange;
begin
  FTextLayout.BeginUpdate;
  try
    FTextLayout.TopLeft := TPointF.Zero;
    FTextLayout.WordWrap := True;
    FTextLayout.MaxSize := TSizeF.Create(FMaxSize.Width - FFirstLineOffset, FMaxSize.Height);
    FTextLayout.Text := FText;
  finally
    FTextLayout.EndUpdate;
  end;

  Regions := FTextLayout.RegionForRange(TTextRange.Create(0, FText.Length));
  if Length(Regions) = 0 then
  begin
    FTextLayout.MaxSize := TSizeF.Create(FMaxSize.Width, Single.MaxValue);
    Regions := FTextLayout.RegionForRange(TTextRange.Create(0, FText.Length));
    SetLength(FLines, Length(Regions));
    for I := Low(Regions) to High(Regions) do
    begin
      Region := Regions[I];
      FLines[I].LineRect := GetAbsoluteTextRectFor(Region);
      FLines[I].Range := GetTextRangeFor(Region);
    end;
  end
  else
  begin
    SetLength(FLines, 1);
    Region := Regions[0];
    FLines[0].Range := GetTextRangeFor(Region);
    FLines[0].LineRect := TRectF.Create(FTopLeft + Region.TopLeft + TPointF.Create(FFirstLineOffset, 0), Region.Width, Region.Height);

    if Length(Regions) > 1 then
    begin
      FirstLine := FLines[0];

      FTextLayout.MaxSize := TSizeF.Create(FMaxSize.Width, Single.MaxValue);
      FTextLayout.Text := FText.Substring(FLines[0].Range.Length);
      Regions := FTextLayout.RegionForRange(TTextRange.Create(0, FText.Length));

      SetLength(FLines, Length(Regions) + 1);
      for I := Low(Regions) to High(Regions) do
      begin
        Region := Regions[I];
        Range := GetTextRangeFor(Region);
        FLines[I + 1].Range := TTextRange.Create(FirstLine.Range.Pos + FirstLine.Range.Length + Range.Pos, Range.Length);
        FLines[I + 1].LineRect := TRectF.Create(FTopLeft + Region.TopLeft + TPointF.Create(0, FirstLine.LineRect.Height), Region.Width, Region.Height);
      end;
    end;
  end;
end;

procedure TIMETextLayout.Render(const ACanvas: TCanvas);
begin
  if ACanvas = nil then
    raise EArgumentNilException.CreateResFmt(@SWrongParameter, ['ACanvas']);

  DrawBackground(ACanvas);
  DrawLines(ACanvas);
end;

procedure TIMETextLayout.SetFont(const Value: TFont);
begin
  FTextLayout.Font := Value;
  FNeedRecalculate := True;
end;

procedure TIMETextLayout.SetMaxSize(const Value: TSizeF);
begin
  if FMaxSize <> Value then
  begin
    FMaxSize := Value;
    FNeedRecalculate := True;
  end;
end;

procedure TIMETextLayout.SetRightToLeft(const Value: Boolean);
begin
  FTextLayout.RightToLeft := Value;
end;

procedure TIMETextLayout.SetText(const Value: string);
begin
  FText := Value;
  FNeedRecalculate := True;
end;

procedure TIMETextLayout.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FTextLayout.Color := Value;
  end;
end;

procedure TIMETextLayout.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value) then
  begin
    FOpacity := Value;
    FTextLayout.Opacity := Value;
  end;
end;

procedure TIMETextLayout.SetTopLeft(const Value: TPointF);
begin
  if FTopLeft <> Value then
  begin
    FTopLeft := Value;
    FNeedRecalculate := True;
  end;
end;

procedure TIMETextLayout.UnderlineLine(const ACanvas: TCanvas; const ARange: TTextRange);
var
  SavedState: TCanvasSaveState;
  I: Integer;
  Attributes: TArray<TMarkedTextAttribute>;
  GroupRange: TTextRange;
  Region: TRegion;
  NeedAddSpace: Boolean;
  MaxValue: Integer;
begin
  SavedState := ACanvas.SaveState;
  try
    ACanvas.Stroke.Assign(ACanvas.Fill);
    ACanvas.Stroke.Color := FTextLayout.Color;

    GroupRange := TTextRange.Create(0, 0);
    Attributes := FMarkedTextAttributes;
    MaxValue := Min(High(Attributes), ARange.Pos + ARange.Length - 1);
    for I := ARange.Pos to MaxValue do
    begin
      ApplyMarkedTextAttribute(ACanvas, Attributes[I]);

      if (I < High(Attributes)) and (Attributes[I] <> Attributes[I + 1]) or (I = MaxValue) then
      begin
        GroupRange.Length := I - GroupRange.Pos + 1 - ARange.Pos;
        Region := FTextLayout.RegionForRange(GroupRange);
      {$IFDEF MACOS}
        NeedAddSpace := Attributes[I] = TMarkedTextAttribute.TargetNotConverted;
      {$ELSE}
        NeedAddSpace := False;
      {$ENDIF}
        UnderlineRegion(ACanvas, Region, NeedAddSpace);
        GroupRange := TTextRange.Create(I + 1 - ARange.Pos, 0);
      end;
    end;
  finally
    ACanvas.RestoreState(SavedState);
  end;
end;

procedure TIMETextLayout.UnderlineRegion(const ACanvas: TCanvas; const ARegions: TRegion; const ANeedAddSpace: Boolean);
var
  I: Integer;
  Region: TRectF;
  HalfThickness: Single;
  StartPoint, EndPoint: TPointF;
  Thickness: Single;
  ShrinkValue: Integer;
begin
  Thickness := ACanvas.Stroke.Thickness;
  HalfThickness := Thickness / 2;
  ShrinkValue := IFThen(ANeedAddSpace, 1, 0);

  for I := Low(ARegions) to High(ARegions) do
  begin
    Region := ACanvas.AlignToPixel(ARegions[I]);
    Region.Offset(0, Thickness);

    StartPoint := TPointF.Create(Region.Left, Region.Bottom);
    StartPoint.Offset(ShrinkValue, -HalfThickness);
    EndPoint := Region.BottomRight;
    EndPoint.Offset(-ShrinkValue, -HalfThickness);
    ACanvas.DrawLine(StartPoint, EndPoint, Opacity);
  end;
end;

type
  TFixedStyledMemo = class(TStyledCustomScrollBox, ITextInput, ITextSpellCheck, ITextSpellCheckActions)
  public const
    DefaultEmptySelectionWidth = 5;
    IMEWindowGap = 2; // Small space between conrol and IME window
    MarkedTextBackgroundOpacity = 0.9;
  protected type
    TSelectionMethod = (Keyboard, Mouse);
    TSelectionMethods = set of TSelectionMethod;
    TSelectionOption = (SelectWords);
    TSelectionOptions = set of TSelectionOption;
    TScrollDirection = (Up, Down);
  private
    FTextService: TTextService;
    FCaretPosition: TCaretPosition;
    FMemoPopupMenu: TPopupMenu;
    FActionStack: TEditActionStack;
    FLinesLayout: TLinesLayout;
    FCharsBuffer: string;
    FSetFocusOnUp: Boolean;
    FNeedUpdateContentOffset: Boolean;
    FAutoscrollController: TAutoscrollController;
    { Selection }
    FSelectionController: TSelectionController;
    FSelectionMethods: TSelectionMethods;
    FSelectionOptions: TSelectionOptions;
    { Spelling }
    FSpellingManager: TMemoSpellingManager;
    { IME }
    FImeLayout: TIMETextLayout;
    function GetModel: TCustomMemoModel;
    function GetMemo: TCustomMemo;
    procedure SetCaretPosition(const Value: TCaretPosition);
    function GetPageSize: Single;
    { Handlers }
    procedure ReplaceWordHandler(const ASpellingWord: TSpellingWord; const ASuggestion: string);
    procedure SelectionChangedHandler(Sender: TObject; const ASelStart, ALength: Integer);
    procedure ContextMenuItemClick(Sender: TObject);
    procedure AutoScrollHandler(const ADirection: TAutoscrollDirection; var AStop: Boolean);
    procedure ContentGetClipRectHandler(Sender: TObject; var AClipRect: TRectF);
    function ConvertLocalPointFrom(const AControl: TControl;
      const AControlLocalPoint: TPointF): TPointF;
    function ConvertLocalPointTo(const AControl: TControl;
      const ALocalPoint: TPointF): TPointF;
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure UpdateContentOffset;
    procedure UpdateCaretPosition;

    { Messages from model }
    procedure MMCharCaseChanged(var Message: TDispatchMessageWithValue<TEditCharCase>); message MM_MEMO_CHARCASE_CHANGED;
    procedure MMCheckSpellingChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_CHECKSPELLING_CHANGED;
    procedure MMHideSelectionOnExitChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_HIDESELECTIONONEXIT_CHANGED;
    procedure MMReadOnlyChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_READONLY_CHANGED;
    procedure MMImeModeChanged(var Message: TDispatchMessageWithValue<TImeMode>); message MM_MEMO_IMEMODE_CHANGED;
    procedure MMSetSelStart(var Message: TDispatchMessageWithValue<Integer>); message MM_MEMO_SELSTART_CHANGED;
    procedure MMSelLengthChanged(var Message: TDispatchMessageWithValue<Integer>); message MM_MEMO_SELLENGTH_CHANGED;
    procedure MMTextSettingsChanged(var Message: TDispatchMessage); message MM_MEMO_TEXT_SETTINGS_CHANGED;
    procedure MMLinesInsertLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_INSERT_LINE;
    procedure MMLinesReplaceLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_PUT_LINE;
    procedure MMLinesDeleteLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_DELETE_LINE;
    procedure MMLinesExchangeLines(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>); message MM_MEMO_LINES_EXCHANGE_LINES;
    procedure MMLinesClear(var Message: TDispatchMessage); message MM_MEMO_LINES_CLEAR;
    procedure MMUpdateStateChanged(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_UPDATE_STATE_CHANGED;
    procedure MMGetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>); message MM_MEMO_GET_CARET_POSITION;
    procedure MMSetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>); message MM_MEMO_SET_CARET_POSITION;
    procedure MMCanSetFocus(var Message: TDispatchMessageWithValue<Boolean>); message MM_MEMO_CAN_SET_FOCUS;
    procedure MMLinesChanged(var Message: TDispatchMessage); message MM_MEMO_LINES_CHANGED;
    procedure MMMaxLengthChanged(var Message: TDispatchMessage); message MM_MEMO_MAXLENGTH_CHANGED;
    procedure MMGetCaretPositionByPoint(var Message: TDispatchMessageWithValue<TCustomMemoModel.TGetCaretPositionInfo>); message MM_MEMO_GET_CARET_POSITION_BY_POINT;

    { Messages from presented control }
    procedure PMInit(var Message: TDispatchMessage); message PM_INIT;
    procedure PMGotoLineBegin(var Message: TDispatchMessage); message PM_MEMO_GOTO_LINE_BEGIN;
    procedure PMGotoLineEnd(var Message: TDispatchMessage); message PM_MEMO_GOTO_LINE_END;
    procedure PMFragmentInserted(var Message: TDispatchMessageWithValue<TFragmentInserted>); message PM_MEMO_UNDO_MANAGER_INSERT_TEXT;
    procedure PMFragmentDeleted(var Message: TDispatchMessageWithValue<TFragmentDeleted>); message PM_MEMO_UNDO_MANAGER_DELETE_TEXT;
    procedure PMUndo(var Message: TDispatchMessage); message PM_MEMO_UNDO_MANAGER_UNDO;
    procedure PMSelectText(var Message: TDispatchMessage); message PM_MEMO_SELECT_TEXT;
    procedure PMRootChanged(var Message: TDispatchMessageWithValue<IRoot>); message PM_ROOT_CHANGED;

    { ITextInput }
    function GetTextService: TTextService;
    function GetTargetClausePointF: TPointF;
    procedure StartIMEInput;
    procedure EndIMEInput;
    procedure IMEStateUpdated;
    function GetSelection: string;
    function GetSelectionRect: TRectF;
    function GetSelectionBounds: TRect;
    function GetSelectionPointSize: TSizeF;
    function HasText: Boolean;

    { Rendering }
    procedure ContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
    procedure DrawSelection(const ACanvas: TCanvas); virtual;
    procedure DrawMarkedText(const ACanvas: TCanvas; const ARect: TRectF); virtual;

    { Selections }
    /// <summary>Returns selection region in the <c>Content</c> coordinate system.</summary>
    /// <remarks>It works slowly on a large number of lines of text. If you need to get a selection only within
    /// the visible area, use the method <c>GetVisibleSelectionRegion</c>.</remarks>
    function GetSelectionRegion: TRegion;
    /// <summary>Returns visible selection region in the <c>Content</c> coordinate system.</summary>
    function GetVisibleSelectionRegion: TRegion;
    function IsSelecting: Boolean;
    function NeedShowSelection: Boolean; virtual;

    { Autoscroll }
    procedure StartAutoScroll(const ALocalPoint: TPointF);

    { Caret }
    /// <summary>The carret position may become outdated and not fall within the text boundaries.
    /// This method allows to normalize the carriage position within the text.</summary>
    procedure NormalizeCaretPosition; overload;
    function NormalizeCaretPosition(const Value: TCaretPosition): TCaretPosition; overload;

    { ITextSpellCheck }
    function IsSpellCheckEnabled: Boolean;
    function IsCurrentWordWrong: Boolean;
    function GetListOfPrepositions: TArray<string>;
    procedure HighlightSpell;
    procedure HideHighlightSpell;

    { ITextSpellCheckActions }
    procedure Spell(const AWord: string);

    { Popup menu }
    procedure ExecuteContextAction(const AAction: TContextAction); virtual;
    procedure FillPopupMenu(const AMenu: TPopupMenu); virtual;
    ///<summary>Updates the current state of popup menu items.</summary>
    procedure UpdatePopupMenuItems(const AMenu: TPopupMenu); virtual;

    { Mouse events }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure AniMouseDown(const Touch: Boolean; const X, Y: Single); override;
    procedure AniMouseMove(const Touch: Boolean; const X, Y: Single); override;
    procedure AniMouseUp(const Touch: Boolean; const X, Y: Single); override;

    { Gestures }
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure LongTap(const ALocalPoint: TPointF; const AFlags: TInteractiveGestureFlags);
    procedure DblTap;

    { IME }
    function HasImeMarkedText: Boolean;
    procedure UpdateTextInTextService;
    /// <summary>Specifies whether to hide the caret during IME typing?</summary>
    function NeedHideCaretInIME: Boolean; virtual;

    { inherited }
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean; override;
    procedure DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure Resize; override;
    procedure DoChange; virtual;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecalcOpacity; override;

    /// <summary>Rollback the latest changes.</summary>
    procedure Undo;
    ///<summary>Repainting content in memo.</summary>
    procedure RepaintEdit;

    { Caret navigation }
    procedure GotoLineBegin;
    procedure GotoLineEnd;
    procedure MoveCaretVertical(const ALineDelta: Integer);
    procedure MoveCaretPageUp;
    procedure MoveCaretPageDown;
    /// <summary>Returns caret position in <c>Content</c> coordinate system.</summary>
    function GetCaretPositionPoint(const ACaretPos: TCaretPosition): TPointF;
    procedure PutCaretTo(const X, Y: Single; const APositionByWord: Boolean = False);

    { Viewport }
    procedure ScrollOnLine(const ADirection: TScrollDirection);
    function ViewportRect: TRectF;
  public
    property Model: TCustomMemoModel read GetModel;
    property Memo: TCustomMemo read GetMemo;
    property LinesLayout: TLinesLayout read FLinesLayout;
    property AutoscrollController: TAutoscrollController read FAutoscrollController;
    property SelectionController: TSelectionController read FSelectionController;
    ///<summary>Current caret position in text.</summary>
    property CaretPosition: TCaretPosition read FCaretPosition write SetCaretPosition;
    /// <summary>Average count of visible lines.</summary>
    property PageSize: Single read GetPageSize;
  end;

{ TFixedStyledMemo }

function TFixedStyledMemo.GetMemo: TCustomMemo;
begin
  Result := PresentedControl as TCustomMemo;
end;

function TFixedStyledMemo.GetModel: TCustomMemoModel;
begin
  Result := inherited GetModel<TCustomMemoModel>;
end;

constructor TFixedStyledMemo.Create(AOwner: TComponent);
var
  PlatformTextService: IFMXTextService;
begin
  inherited;
  EnableExecuteAction := False;
  CanFocus := False;
  AutoCapture := True;
  SetAcceptsControls(False);

  if TPlatformServices.Current.SupportsPlatformService(IFMXTextService, PlatformTextService) then
    FTextService := PlatformTextService.GetTextServiceClass.Create(Self, True);

  FLinesLayout := TLinesLayout.Create(Model, Self);
  FSelectionController := TSelectionController.Create(Model);
  FSelectionController.OnChanged := SelectionChangedHandler;

  FMemoPopupMenu := TPopupMenu.Create(Self);
  FMemoPopupMenu.Stored := False;
  FMemoPopupMenu.PopupComponent := Self;
  FillPopupMenu(FMemoPopupMenu);
  FMemoPopupMenu.AddFreeNotify(Self);

  FActionStack := TEditActionStack.Create(Model);
  FCaretPosition := TCaretPosition.Zero;

  if FTextService <> nil then
    FTextService.ImeMode := TImeMode.imDontCare;

  FAutoscrollController := TAutoscrollController.Create;
  FAutoscrollController.OnScroll := AutoScrollHandler;

  Touch.InteractiveGestures := Touch.InteractiveGestures + [TInteractiveGesture.DoubleTap, TInteractiveGesture.LongTap];

  FSpellingManager := TMemoSpellingManager.Create(Model);
  FSpellingManager.OnReplaceWord := ReplaceWordHandler;

  FImeLayout := TIMETextLayout.Create;
end;

function TFixedStyledMemo.DefineModelClass: TDataModelClass;
begin
  Result := TCustomMemoModel;
end;

destructor TFixedStyledMemo.Destroy;
begin
  Content.OnPainting := nil;
  Content.OnGetClipRect := nil;
  FreeAndNil(FAutoscrollController);
  FreeAndNil(FImeLayout);
  FreeAndNil(FSpellingManager);
  FreeAndNil(FActionStack);
  FreeAndNil(FMemoPopupMenu);
  FreeAndNil(FSelectionController);
  FreeAndNil(FLinesLayout);
  FreeAndNil(FTextService);
  inherited;
end;

procedure TFixedStyledMemo.HideHighlightSpell;
begin
  FSpellingManager.HideHighlightSpell;
  RepaintEdit;
end;

procedure TFixedStyledMemo.DoEndUpdate;

  function IsLoading: Boolean;
  begin
    Result := csLoading in PresentedControl.ComponentState;
  end;

  function IsDestroying: Boolean;
  begin
    Result := csDestroying in PresentedControl.ComponentState;
  end;

begin
  inherited;
  FLinesLayout.EndUpdate;
  if not (IsUpdating or IsLoading or IsDestroying) then
  begin
    UpdateCaretPosition;
    if FNeedUpdateContentOffset then
      UpdateContentOffset;
    RepaintEdit;
  end;
end;

procedure TFixedStyledMemo.DoEnter;
begin
  inherited;
  UpdateTextInTextService;
  UpdateCaretPosition;
  if Model.AutoSelect then
    Memo.SelectAll;
end;

procedure TFixedStyledMemo.DoExit;
begin
  DoChange;
  if Observers.IsObserving(TObserverMapping.EditLinkID) then
    TLinkObservers.EditLinkUpdate(Observers);
  if Observers.IsObserving(TObserverMapping.ControlValueID) then
    TLinkObservers.ControlValueUpdate(Observers);
  inherited;
end;

procedure TFixedStyledMemo.GotoLineBegin;
var
  Point: TPointF;
begin
  if HasText then
  begin
    Point := TPointF.Create(0, Model.Caret.Pos.Y + FLinesLayout.GetLineHeight / 2);
    CaretPosition := FLinesLayout.GetCaretPositionByPoint(Point);
  end;
end;

procedure TFixedStyledMemo.GotoLineEnd;
var
  Point: TPointF;
begin
  if HasText then
  begin
    Point := TPointF.Create(FLinesLayout[CaretPosition.Line].Rect.Right - 1,
                            Model.Caret.Pos.Y + FLinesLayout.GetLineHeight / 2);
    CaretPosition := FLinesLayout.GetCaretPositionByPoint(Point);
  end;
end;

function TFixedStyledMemo.GetCaretPositionPoint(const ACaretPos: TCaretPosition): TPointF;
begin
  Result := FLinesLayout.GetPointByCaretPosition(ACaretPos);
end;

procedure TFixedStyledMemo.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);

  function GetLeftWordBegin(const APosition: TCaretPosition): TCaretPosition;
  var
    CurrentLine: string;
    LLines: TStrings;
  begin
    LLines := Model.Lines;
    if LLines.Count = 0 then
      Exit(APosition);

    Result.Pos := APosition.Pos;
    Result.Line := APosition.Line;
    CurrentLine := LLines[Result.Line];

    if APosition.Pos > 0 then
    begin
      Result.Pos := GetLexemeBegin(CurrentLine, APosition.Pos);
      // If cursor is placed in the beginning of word, we have to take beginning pos of previous word.
      if Result.Pos = APosition.Pos then
        Result.Pos := GetPrevLexemeBegin(CurrentLine, APosition.Pos);
    end
    else if (APosition.Line - 1 >= 0) and (APosition.Line - 1 <= LLines.Count - 1) then
    begin
      Result.Line := APosition.Line - 1;
      Result.Pos := CurrentLine.Length;
    end;
  end;

  procedure DeleteTextByBackspace(const AIsCtrlOrCmd: Boolean);
  var
    DeleteLength: Integer;
    LCaret: TCaretPosition;
  begin
    if Model.ReadOnly then
      Exit;

    if Model.SelLength <> 0 then
      Memo.DeleteSelection
    else if AIsCtrlOrCmd then
    begin
      // Deleting whole word
      LCaret := GetLeftWordBegin(CaretPosition);
      if LCaret.IsInvalid then
        Exit;
      Model.DeleteFrom(LCaret, Model.PosToTextPos(CaretPosition) - Model.PosToTextPos(LCaret),
        [TDeleteOption.MoveCaret, TDeleteOption.CanUndo]);
    end
    // Deleting single character
    else if Model.PosToTextPos(CaretPosition) > 0 then
    begin
      if (Model.Lines[CaretPosition.Line].Length > 0) and
        Model.Lines[CaretPosition.Line].Chars[CaretPosition.Pos - 1].IsLowSurrogate then
        Model.DeleteFrom(Model.GetPositionShift(CaretPosition, -2), 2,
          [TDeleteOption.MoveCaret, TDeleteOption.CanUndo])
      else
      begin
        if CaretPosition.Pos = 0 then
          DeleteLength := Model.Lines.LineBreak.Length
        else
          DeleteLength := 1;

         Model.DeleteFrom(Model.GetPositionShift(CaretPosition, -1), DeleteLength, [TDeleteOption.MoveCaret, TDeleteOption.CanUndo]);
      end;
    end;
  end;

  procedure DeleteTextByDel(const AIsCtrlOrCmd: Boolean);
  begin
    if Model.ReadOnly then
      Exit;

    if Model.SelLength <> 0 then
    begin
      if ssShift in Shift then
        Memo.CutToClipboard
      else
        Memo.DeleteSelection;
    end
    else if AIsCtrlOrCmd then
      Model.DeleteFrom(CaretPosition, Min(FMX.Text.GetLexemeEnd(Model.Lines[CaretPosition.Line],
        CaretPosition.Pos), Model.Lines[CaretPosition.Line].Length) - CaretPosition.Pos + 1,
        [TDeleteOption.CanUndo])
    else if HasText then
    begin
      if (CaretPosition.Pos < Model.Lines[CaretPosition.Line].Length) and
        Model.Lines[CaretPosition.Line].Chars[CaretPosition.Pos].IsHighSurrogate then
        Model.DeleteFrom(CaretPosition, 2, [TDeleteOption.CanUndo])
      else if CaretPosition.Pos = Model.Lines[CaretPosition.Line].Length then
        // We are in the end of line, So we have to remove line break
        Model.DeleteFrom(CaretPosition, Model.Lines.LineBreak.Length, [TDeleteOption.CanUndo])
      else
        Model.DeleteFrom(CaretPosition, 1, [TDeleteOption.CanUndo]);
    end;
  end;

var
  TmpS: string;
  IsCtrlOrCmd: Boolean;
  LTmpOptions: TInsertOptions;
  KeyHandled: Boolean;
  IgnoreResetSelection: Boolean;
begin
  KeyHandled := False;
  IgnoreResetSelection := False;

  IsCtrlOrCmd := Shift * [ssCtrl, ssCommand] <> [];

  // Shift key can be used for pressing Uppercase characters, In this case we don't need to consider this case as selection.
  if (ssShift in Shift) and (KeyChar = #0) then
    Include(FSelectionMethods, TSelectionMethod.Keyboard);

  if IsCtrlOrCmd and (Key in [vkControl, vkUp, vkDown, vkC, vkLCommand, vkRCommand]) then
    IgnoreResetSelection := True;

  if Observers.IsObserving(TObserverMapping.EditLinkID) then
  begin
    if (Key in [vkReturn, vkBack, vkDelete]) or ((Key = vkInsert) and (ssShift in Shift)) then
      if TLinkObservers.EditLinkEdit(Observers) then
        TLinkObservers.EditLinkModified(Observers)
      else
      begin
        TLinkObservers.EditLinkReset(Observers);
        Exit;
      end;

    if (KeyChar >= #32) and not TLinkObservers.EditLinkIsValidChar(Observers, KeyChar) then
    begin
      KeyChar := #0;
      Exit;
    end;
    case KeyChar of
      ^H, ^V, ^X, #32 .. High(Char):
        if TLinkObservers.EditLinkEdit(Observers) then
          TLinkObservers.EditLinkModified(Observers)
        else
        begin
          KeyChar := #0;
          TLinkObservers.EditLinkReset(Observers);
          Exit;
        end;
      #27:
        begin
          TLinkObservers.EditLinkReset(Observers);
          Memo.SelectAll;
          KeyChar := #0;
          Exit;
        end;
    end;
  end;

  if Observers.IsObserving(TObserverMapping.ControlValueID) and (KeyChar <> #0) then
    TLinkObservers.ControlValueModified(Observers);

  inherited KeyDown(Key, KeyChar, Shift);

  // We don't process any combination with Alt key.
  if ssAlt in Shift then
    Exit;

  if (Key = vkReturn) and not (ssCommand in Shift) and not Model.ReadOnly then
  begin
    if Model.SelLength > 0 then
    begin
      Model.DeleteFrom(FSelectionController.SelBegin, Model.SelLength, [TDeleteOption.MoveCaret, TDeleteOption.CanUndo,
                       TDeleteOption.Selected]);
      LTmpOptions := [TInsertOption.UndoPairedWithPrev];
    end
    else
      LTmpOptions := [];
    TmpS := Model.Lines.LineBreak;
    Model.InsertAfter(CaretPosition, TmpS, LTmpOptions + [TInsertOption.MoveCaret, TInsertOption.CanUndo]);
    Model.SelLength := 0;
    Key := 0;
    DoChange;
  end;
  case Key of
    vkA:
      if IsCtrlOrCmd then
      begin
        Memo.SelectAll;
        IgnoreResetSelection := True;
        KeyHandled := True;
      end;
    vkC:
      if IsCtrlOrCmd then
      begin
        Memo.CopyToClipboard;
        KeyHandled := True;
      end;
    vkV:
      if IsCtrlOrCmd then
      begin
        Memo.PasteFromClipboard;
        KeyHandled := True;
      end;
    vkX:
      if IsCtrlOrCmd and not Model.ReadOnly then
      begin
        Memo.CutToClipboard;
        KeyHandled := True;
      end;
    vkZ:
      if IsCtrlOrCmd then
      begin
        Undo;
        KeyHandled := True;
      end;
    vkEnd:
      begin
        if IsCtrlOrCmd then
          TCustomMemo(PresentedControl).GoToTextEnd
        else
          GotoLineEnd;
        KeyHandled := True;
      end;
    vkHome:
      begin
        if IsCtrlOrCmd then
          TCustomMemo(PresentedControl).GoToTextBegin
        else
          GotoLineBegin;
        KeyHandled := True;
      end;
    vkLeft:
      begin
        if IsCtrlOrCmd then
          CaretPosition := GetLeftWordBegin(CaretPosition)
        else
          Model.MoveCaretLeft;
        KeyHandled := True;
      end;
    vkRight:
      begin
        if IsCtrlOrCmd then
          CaretPosition := Model.GetNextWordBegin(CaretPosition)
        else
          Model.MoveCaretRight;
        KeyHandled := True;
      end;
    vkUp:
      begin
        if IsCtrlOrCmd then
          ScrollOnLine(TScrollDirection.Up)
        else
          MoveCaretVertical(-1);
        KeyHandled := True;
      end;
    vkDown:
      begin
        if IsCtrlOrCmd then
          ScrollOnLine(TScrollDirection.Down)
        else
          MoveCaretVertical(1);
        KeyHandled := True;
      end;
    vkPrior:
      begin
        MoveCaretPageUp;
        KeyHandled := True;
      end;
    vkNext:
      begin
        MoveCaretPageDown;
        KeyHandled := True;
      end;
    vkDelete:
      begin
        DeleteTextByDel(IsCtrlOrCmd);
        KeyHandled := True;
      end;
    vkBack:
      begin
        DeleteTextByBackspace(IsCtrlOrCmd);
        KeyHandled := True;
      end;
    vkInsert:
      if IsCtrlOrCmd then
      begin
        Memo.CopyToClipboard;
        KeyHandled := True;
      end
      else if [ssShift] * Shift <> [] then
      begin
        Memo.PasteFromClipboard;
        KeyHandled := True;
      end;
    vkProcessKey:
      IgnoreResetSelection := True;
  end;

  if (KeyChar <> #0) and not Model.ReadOnly then
  begin
    FCharsBuffer := FCharsBuffer + KeyChar;
    if not KeyChar.IsHighSurrogate then
    begin
      if Model.SelLength > 0 then
      begin
        Model.DeleteFrom(FSelectionController.SelBegin, Model.SelLength, [TDeleteOption.MoveCaret, TDeleteOption.CanUndo]);
        LTmpOptions := [TInsertOption.UndoPairedWithPrev];
      end
      else
        LTmpOptions := [];
      Model.InsertAfter(CaretPosition, FCharsBuffer, LTmpOptions + [TInsertOption.MoveCaret, TInsertOption.CanUndo,
                        TInsertOption.Typed]);
      FCharsBuffer := string.Empty;
      Model.SelLength := 0;
    end;
    KeyHandled := True;
  end
  else
  begin
    FCharsBuffer := string.Empty;
    if Key in [vkEnd, vkHome, vkLeft, vkRight, vkUp, vkDown, vkPrior, vkNext] then
    begin
      if IsSelecting then
        FSelectionController.Finish := CaretPosition;

      RepaintEdit;
      KeyHandled := True;
    end;
  end;
  if not IsSelecting and not IgnoreResetSelection then
    FSelectionController.SetRange(CaretPosition, CaretPosition);

  if KeyHandled then
  begin
    Key := 0;
    KeyChar := #0;
  end;

  Exclude(FSelectionMethods, TSelectionMethod.Keyboard);
end;

procedure TFixedStyledMemo.KeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  Exclude(FSelectionMethods, TSelectionMethod.Keyboard);
end;

procedure TFixedStyledMemo.LongTap(const ALocalPoint: TPointF; const AFlags: TInteractiveGestureFlags);
begin
  SelectionController.UnholdSelection;
  Exclude(FSelectionMethods, TSelectionMethod.Mouse);

  if not (csDesigning in ComponentState) and not PresentedControl.IsFocused then
    PresentedControl.SetFocus;

  if TInteractiveGestureFlag.gfEnd in AFlags then
    ShowContextMenu(LocalToScreen(ALocalPoint));
end;

procedure TFixedStyledMemo.MMCanSetFocus(var Message: TDispatchMessageWithValue<Boolean>);
begin
  Message.Value := not FSetFocusOnUp;
end;

procedure TFixedStyledMemo.MMCharCaseChanged(var Message: TDispatchMessageWithValue<TEditCharCase>);
begin
  if FTextService <> nil then
  begin
    UpdateTextInTextService;
    FTextService.CharCase := Model.CharCase;
  end;
  RepaintEdit;
end;

procedure TFixedStyledMemo.MMCheckSpellingChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  if Message.Value then
    FSpellingManager.FindSpellingErrorsInLines
  else
    FSpellingManager.Reset;
  RepaintEdit;
end;

procedure TFixedStyledMemo.PMFragmentDeleted(var Message: TDispatchMessageWithValue<TFragmentDeleted>);
begin
  FActionStack.FragmentDeleted(Message.Value.StartPos, Message.Value.Fragment, Message.Value.Selected,
    Message.Value.CaretMoved);
end;

procedure TFixedStyledMemo.PMFragmentInserted(var Message: TDispatchMessageWithValue<TFragmentInserted>);
begin
  FActionStack.FragmentInserted(Message.Value.StartPos, Message.Value.FragmentLength, Message.Value.PairedWithPrev,
    Message.Value.Typed);
end;

procedure TFixedStyledMemo.MMGetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>);
begin
  Message.Value := CaretPosition;
end;

procedure TFixedStyledMemo.MMGetCaretPositionByPoint(var Message: TDispatchMessageWithValue<TCustomMemoModel.TGetCaretPositionInfo>);
begin
  Message.Value.CaretPosition := FLinesLayout.GetCaretPositionByPoint(Message.Value.HitPoint + ViewportPosition, Message.Value.RoundToWord);
end;

procedure TFixedStyledMemo.PMGotoLineBegin(var Message: TDispatchMessage);
begin
  GotoLineBegin;
end;

procedure TFixedStyledMemo.PMGotoLineEnd(var Message: TDispatchMessage);
begin
  GotoLineEnd;
end;

procedure TFixedStyledMemo.MMHideSelectionOnExitChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  RepaintEdit;
end;

procedure TFixedStyledMemo.MMImeModeChanged(var Message: TDispatchMessageWithValue<TImeMode>);
begin
  if (FTextService <> nil) and (FTextService.ImeMode <> Message.Value) then
    FTextService.ImeMode := Message.Value;
end;

procedure TFixedStyledMemo.MMLinesChanged(var Message: TDispatchMessage);
begin
end;

procedure TFixedStyledMemo.MMLinesClear(var Message: TDispatchMessage);
begin
  FLinesLayout.Clear;

  CaretPosition := TCaretPosition.Zero;
  FSelectionController.Reset;
  FSpellingManager.Reset;
  UpdateCaretPosition;
  UpdateContentOffset;
  RepaintEdit;
end;

procedure TFixedStyledMemo.MMLinesDeleteLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);

  procedure AdjustCaretPosition;
  var
    NewCaretPosition: TCaretPosition;
  begin
    if CaretPosition.Line >= Model.Lines.Count then
      // Old caret line is out of lines count.
      NewCaretPosition.Line := CaretPosition.Line - 1
    else
      // Trying to keep current line.
      NewCaretPosition.Line := CaretPosition.Line;
    NewCaretPosition.Pos := CaretPosition.Pos;
    CaretPosition := NormalizeCaretPosition(NewCaretPosition);
  end;

begin
  FLinesLayout.DeleteLine(Message.Value.Index);
  if CaretPosition.Line >= Message.Value.Index then
    // We are trying to save the position of the caret in the following lines.
    AdjustCaretPosition;
  SelectionController.SetRange(CaretPosition, CaretPosition);
  if Model.CheckSpelling then
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.Index);
end;

procedure TFixedStyledMemo.MMLinesExchangeLines(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);
begin
  if Model.CheckSpelling then
  begin
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.Index);
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.ExtraIndex);
  end;
  FLinesLayout.ExchangeLines(Message.Value.Index, Message.Value.ExtraIndex);
  if Model.CheckSpelling then
  begin
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.Index);
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.ExtraIndex);
  end;

  if not IsUpdating then
    RepaintEdit;
end;

procedure TFixedStyledMemo.MMLinesInsertLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);

  procedure AdjustCaretPosition;
  var
    NewCaretPosition: TCaretPosition;
  begin
    NewCaretPosition.Line := CaretPosition.Line + 1;
    NewCaretPosition.Pos := CaretPosition.Pos;
    CaretPosition := NormalizeCaretPosition(NewCaretPosition);
  end;

begin
  FLinesLayout.InsertLine(Message.Value.Index, Message.Value.Text);
  if CaretPosition.Line >= Message.Value.Index then
    // We are trying to save the position of the carret in the following lines.
    AdjustCaretPosition;
  if Model.CheckSpelling then
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.Index);
end;

procedure TFixedStyledMemo.MMLinesReplaceLine(var Message: TDispatchMessageWithValue<TCustomMemoModel.TLineInfo>);
begin
  FLinesLayout.ReplaceLine(Message.Value.Index, Message.Value.Text);
  NormalizeCaretPosition;
  SelectionController.SetRange(CaretPosition, CaretPosition);
  if Model.CheckSpelling then
  begin
    FSpellingManager.RemoveSpellingErrorsForLine(Message.Value.Index);
    FSpellingManager.FindSpellingErrorsInLine(Message.Value.Index);
  end;
end;

procedure TFixedStyledMemo.MMMaxLengthChanged(var Message: TDispatchMessage);
begin
  if FTextService <> nil then
    FTextService.MaxLength := Model.MaxLength;
end;

procedure TFixedStyledMemo.MMReadOnlyChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  Model.Caret.ReadOnly := Message.Value;
end;

procedure TFixedStyledMemo.MMSelLengthChanged(var Message: TDispatchMessageWithValue<Integer>);
begin
  FSelectionController.SetRange(Model.SelStart, Message.Value);
end;

procedure TFixedStyledMemo.MMSetCaretPosition(var Message: TDispatchMessageWithValue<TCaretPosition>);
begin
  CaretPosition := Message.Value;
end;

procedure TFixedStyledMemo.MMSetSelStart(var Message: TDispatchMessageWithValue<Integer>);
begin
  FSelectionController.SetRange(Message.Value, 0);
  CaretPosition := FSelectionController.SelBegin;
end;

procedure TFixedStyledMemo.MMTextSettingsChanged(var Message: TDispatchMessage);
var
  TextSettings: TTextSettings;
begin
  TextSettings := Model.TextSettingsInfo.ResultingTextSettings;
  FLinesLayout.TextSettings := TextSettings;

  FImeLayout.BeginUpdate;
  try
    FImeLayout.Font := TextSettings.Font;
    FImeLayout.Color := TextSettings.FontColor;
    FImeLayout.Opacity := Opacity;
    FImeLayout.RightToLeft := TFillTextFlag.RightToLeft in FillTextFlags;
  finally
    FImeLayout.EndUpdate;
  end;

  if not (csLoading in ComponentState) then
  begin
    if FLinesLayout.TextSettings.WordWrap then
      AniCalculations.TouchTracking := AniCalculations.TouchTracking - [ttHorizontal]
    else
      AniCalculations.TouchTracking := AniCalculations.TouchTracking + [ttHorizontal];

    UpdateCaretPosition;
  end;
  RepaintEdit;
end;

procedure TFixedStyledMemo.PMUndo(var Message: TDispatchMessage);
begin
  Undo;
end;

procedure TFixedStyledMemo.MMUpdateStateChanged(var Message: TDispatchMessageWithValue<Boolean>);
begin
  if Message.Value then
    BeginUpdate
  else
    EndUpdate;
end;

procedure TFixedStyledMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FSetFocusOnUp := ([ssDouble, ssTouch] * Shift) = [ssTouch];
  inherited;
  if Button <> TMouseButton.mbLeft then
    Exit;

  if ssDouble in Shift then
  begin
    Memo.SelectWord;
    // User uses double selection mode, we have to hold selection of one word and don't allow to reset
    // this selection word until selection is not finished.
    SelectionController.HoldSelection;
    Include(FSelectionOptions, TSelectionOption.SelectWords);
  end
  else
  begin
    PutCaretTo(X, Y);
    if IsSelecting then
      FSelectionController.Finish := FCaretPosition
    else
      FSelectionController.SetRange(FCaretPosition, FCaretPosition);
  end;
  Include(FSelectionMethods, TSelectionMethod.Mouse);
end;

procedure TFixedStyledMemo.ContentGetClipRectHandler(Sender: TObject; var AClipRect: TRectF);
begin
  AClipRect := LinesLayout.ViewportRect;
end;

procedure TFixedStyledMemo.ContentPaint(Sender: TObject; ACanvas: TCanvas; const ARect: TRectF);
begin
  FLinesLayout.RealignIfNeeded;
  FLinesLayout.Render(ACanvas);

  if NeedShowSelection then
    DrawSelection(ACanvas);

  if HasImeMarkedText then
    DrawMarkedText(ACanvas, ARect);

  if Model.CheckSpelling then
    FSpellingManager.DrawHightlightSpellingWords(FLinesLayout, Model.ViewportSize, ACanvas, AbsoluteOpacity);
end;

procedure TFixedStyledMemo.DoViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
  inherited;
  FLinesLayout.ViewportRect := TRectF.Create(NewViewportPosition, Model.ViewportSize.Width, Model.ViewportSize.Height);
  FImeLayout.MaxSize := Model.ViewportSize;
  if not (OldViewportPosition - NewViewportPosition).IsZero then
  begin
    if Model.CheckSpelling then
      FSpellingManager.UpdateHightlightRect(FLinesLayout, Model.ViewportSize, OldViewportPosition - NewViewportPosition);

    if HasImeMarkedText then
      FTextService.RefreshImePosition;
  end;
end;

procedure TFixedStyledMemo.DrawMarkedText(const ACanvas: TCanvas; const ARect: TRectF);

  procedure InitImeTextLayout(const ATextSettings: TTextSettings);
  begin
    FImeLayout.BeginUpdate;
    try
      FImeLayout.Color := ATextSettings.FontColor;
      FImeLayout.Opacity := Opacity;
      FImeLayout.Text := FTextService.MarkedText;
      FImeLayout.TextAttributes := FTextService.MarketTextAttributes;
    finally
      FImeLayout.EndUpdate;
    end;
  end;

var
  TextSettings: TTextSettings;
begin
  TextSettings := Model.TextSettingsInfo.ResultingTextSettings;

  InitImeTextLayout(TextSettings);
  FImeLayout.Render(ACanvas);
end;

procedure TFixedStyledMemo.DrawSelection(const ACanvas: TCanvas);
var
  I: Integer;
  Region: TRegion;
begin
  Region := GetVisibleSelectionRegion;
  for I := Low(Region) to High(Region) do
  begin
    if Region[I].Width = 0 then
      Region[I].Width := DefaultEmptySelectionWidth;

    ACanvas.FillRect(Region[I], 1, Model.SelectionFill);
  end;
end;

procedure TFixedStyledMemo.AniMouseDown(const Touch: Boolean; const X, Y: Single);
begin
  // Temporary disabling scroll animation until mobile platform are added.
end;

procedure TFixedStyledMemo.AniMouseMove(const Touch: Boolean; const X, Y: Single);
begin
  // Temporary disabling scroll animation until mobile platform are added.
end;

procedure TFixedStyledMemo.AniMouseUp(const Touch: Boolean; const X, Y: Single);
begin
  // Temporary disabling scroll animation until mobile platform are added.
end;

procedure TFixedStyledMemo.ApplyStyle;
var
  StyleResource: TFmxObject;
  BrushObject: TBrushObject;
  FontObject: IFontObject;
  TextSettingsInfo: TTextSettingsInfo;
  TextSettings: TTextSettings;
  DefaultTextSettings: TTextSettings;
begin
  TextSettingsInfo := Model.TextSettingsInfo;
  TextSettings := TextSettingsInfo.TextSettings;
  DefaultTextSettings := TextSettingsInfo.DefaultTextSettings;

  TextSettings.BeginUpdate;
  try
    TextSettingsInfo.Design := False;
    inherited;

    if FindStyleResource<TBrushObject>('selection', BrushObject) then
      Model.SelectionFill := BrushObject.Brush;

    { Default Text settings }
    if FindStyleResource<TBrushObject>('foreground', BrushObject) then
      DefaultTextSettings.FontColor := BrushObject.Brush.Color;
    StyleResource := FindStyleResource('font');
    if Supports(StyleResource, IFontObject, FontObject) and not TextSettings.Font.IsSizeStored then
      DefaultTextSettings.Font := FontObject.Font;
    DefaultTextSettings.HorzAlign := TTextAlign.Leading;

    { Caret Color }
    StyleResource := FindStyleResource('caretcolor');
    if StyleResource is TColorObject then
      Model.Caret.DefaultColor := TColorObject(StyleResource).Color
    else
      Model.Caret.DefaultColor := TAlphaColorRec.Null;
    TextSettings.Change;
  finally
    TextSettings.EndUpdate;
    TextSettingsInfo.Design := csDesigning in ComponentState;
  end;
  FLinesLayout.TextSettings := Model.TextSettingsInfo.ResultingTextSettings;
end;

procedure TFixedStyledMemo.RepaintEdit;
begin
  if Content <> nil then
    Content.Repaint;
end;

procedure TFixedStyledMemo.ReplaceWordHandler(const ASpellingWord: TSpellingWord; const ASuggestion: string);
begin
  Model.Replace(ASpellingWord.Position, ASpellingWord.Length, ASuggestion);
  RepaintEdit;
end;

procedure TFixedStyledMemo.Resize;
begin
  inherited;
  if Model.CheckSpelling then
    FSpellingManager.ResetBounds;
  UpdateCaretPosition;
  UpdateContentOffset;
end;

procedure TFixedStyledMemo.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if TSelectionMethod.Mouse in FSelectionMethods then
  begin
    PutCaretTo(X, Y, TSelectionOption.SelectWords in FSelectionOptions);
    FSelectionController.Finish := CaretPosition;
    StartAutoScroll(TPointF.Create(X, Y));
    UpdateContentOffset;
  end;
end;

procedure TFixedStyledMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FSetFocusOnUp and not AniCalculations.Moved then
  begin
    FSetFocusOnUp := False;
    if not (csDesigning in PresentedControl.ComponentState) and not PresentedControl.IsFocused then
      PresentedControl.SetFocus;
  end;
  if TSelectionMethod.Mouse in FSelectionMethods then
  begin
    PutCaretTo(X, Y, TSelectionOption.SelectWords in FSelectionOptions);
    FSelectionController.Finish := CaretPosition;
  end;
  UpdateCaretPosition;
  SelectionController.UnholdSelection;
  Exclude(FSelectionMethods, TSelectionMethod.Mouse);
  Exclude(FSelectionOptions, TSelectionOption.SelectWords);
  FAutoscrollController.Stop;
end;

procedure TFixedStyledMemo.FillPopupMenu(const AMenu: TPopupMenu);
var
  LMenuItem: TContextMenuItem;
begin
  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditUndo);
  LMenuItem.StyleName := UndoStyleName;
  LMenuItem.ContextAction := TContextAction.Undo;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := SMenuSeparator;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditCut);
  LMenuItem.StyleName := CutStyleName;
  LMenuItem.ContextAction := TContextAction.Cut;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditCopy);
  LMenuItem.StyleName := CopyStyleName;
  LMenuItem.ContextAction := TContextAction.Copy;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditPaste);
  LMenuItem.StyleName := PasteStyleName;
  LMenuItem.ContextAction := TContextAction.Paste;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditDelete);
  LMenuItem.StyleName := DeleteStyleName;
  LMenuItem.ContextAction := TContextAction.Delete;
  LMenuItem.OnClick := ContextMenuItemClick;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := SMenuSeparator;

  LMenuItem := TContextMenuItem.Create(AMenu);
  LMenuItem.Parent := AMenu;
  LMenuItem.Text := Translate(SEditSelectAll);
  LMenuItem.StyleName := SelectAllStyleName;
  LMenuItem.ContextAction := TContextAction.SelectAll;
  LMenuItem.OnClick := ContextMenuItemClick;
end;

procedure TFixedStyledMemo.UpdatePopupMenuItems(const AMenu: TPopupMenu);
var
  ClipService: IFMXClipboardService;

  procedure SetEnabled(const AParamName: string; const AValue : Boolean);
  var
    MenuItem : TMenuItem;
  begin
    MenuItem := TMenuItem(AMenu.FindStyleResource(AParamName));
    if MenuItem <> nil then
      MenuItem.Enabled := AValue;
  end;

begin
  SetEnabled(UndoStyleName, not Model.ReadOnly and (FActionStack.Count > 0));
  SetEnabled(CutStyleName, FSelectionController.IsSelected and not Model.ReadOnly);
  SetEnabled(CopyStyleName, FSelectionController.IsSelected);
  SetEnabled(PasteStyleName, TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipService) and
    not ClipService.GetClipboard.IsEmpty and not Model.ReadOnly);
  SetEnabled(DeleteStyleName, FSelectionController.IsSelected and not Model.ReadOnly);
  SetEnabled(SelectAllStyleName, Model.SelLength <> Model.Lines.Text.Length);
end;

procedure TFixedStyledMemo.UpdateContentOffset;
type
  TRelativeLocation = (Above, Below, &In, OnTheLeftSide, OnTheRightSide);

  function RectLocation(const ARect: TRectF): TRelativeLocation;
  begin
    if ARect.Bottom > Model.ViewportSize.Height then
      Result := TRelativeLocation.Below
    else if ARect.Top < 0 then
      Result := TRelativeLocation.Above
    else if ARect.Left < 0 then
      Result := TRelativeLocation.OnTheLeftSide
    else if ARect.Left > Model.ViewportSize.Width then
      Result := TRelativeLocation.OnTheRightSide
    else
      Result := TRelativeLocation.&In;
  end;

  function CalculateViewportOffset(const ACaretRect: TRectF): TPointF;
  const
    DefaultOffset = 50;
  begin
    case RectLocation(ACaretRect) of
      TRelativeLocation.Above:
        Result := TPointF.Create(0, ACaretRect.Top);
      TRelativeLocation.Below:
        Result := TPointF.Create(0, ACaretRect.Bottom - Model.ViewportSize.Height);
      TRelativeLocation.OnTheLeftSide:
        Result := TPointF.Create(ACaretRect.Left - DefaultOffset, 0);
      TRelativeLocation.OnTheRightSide:
        Result := TPointF.Create(ACaretRect.Left - Model.ViewportSize.Width + DefaultOffset, 0);
    else
      Result := TPointF.Zero;
    end;
  end;

var
  CaretRegion: TRegion;
  CaretRect: TRectF;
begin
  if csLoading in ComponentState then
    Exit;

  CaretRegion := FLinesLayout.GetRegionForRange(CaretPosition, 1);
  if Length(CaretRegion) = 0 then
    Exit;

  // Convert rect to viewport coordinate system
  CaretRect := CaretRegion[0];
  CaretRect.Offset(-ViewportPosition);

  ViewportPosition := ViewportPosition + CalculateViewportOffset(CaretRect);
  FNeedUpdateContentOffset := False;
end;

procedure TFixedStyledMemo.UpdateTextInTextService;
begin
  if HasText then
    FTextService.Text := Model.Lines[FCaretPosition.Line]
  else
    FTextService.Text := string.Empty;
end;

function TFixedStyledMemo.ViewportRect: TRectF;
begin
  if ContentLayout = nil then
    Result := TRectF.Create(0, 0, Model.ViewportSize.Width, Model.ViewportSize.Height)
  else
    Result := ContentLayout.BoundsRect;
end;

procedure TFixedStyledMemo.ScrollOnLine(const ADirection: TScrollDirection);
var
  LLineHeight : Single;
  NewViewportPosition: TPointF;
  Delta: Single;
begin
  if HasText and VScrollBar.Visible then
  begin
    LLineHeight := Model.ContentBounds.Height / Model.Lines.Count;
    NewViewportPosition := ViewportPosition;
    case ADirection of
      TFixedStyledMemo.TScrollDirection.Up: Delta := -LLineHeight;
      TFixedStyledMemo.TScrollDirection.Down: Delta := LLineHeight;
    else
      Delta := LLineHeight;
    end;
    NewViewportPosition.Offset(0, Delta);
    ViewportPosition := NewViewportPosition;
  end;
end;

procedure TFixedStyledMemo.EndIMEInput;
begin
  // Some platforms have their own behavior in displaying the cursor. For example, macOS displays marked IME text
  // via underlining
  Model.Caret.TemporarilyHidden := False;
  if HasText and (CaretPosition.Line < Model.Lines.Count) then
    CaretPosition := TCaretPosition.Create(CaretPosition.Line, Min(CaretPosition.Pos, Model.Lines[CaretPosition.Line].Length));
end;

procedure TFixedStyledMemo.ExecuteContextAction(const AAction: TContextAction);
begin
  case AAction of
    TContextAction.Cut:
      Memo.CutToClipboard;
    TContextAction.Copy:
      Memo.CopyToClipboard;
    TContextAction.Paste:
      Memo.PasteFromClipboard;
    TContextAction.Delete:
      Memo.DeleteSelection;
    TContextAction.Undo:
      Undo;
    TContextAction.SelectAll:
      Memo.SelectAll;
  end;
end;

procedure TFixedStyledMemo.AutoScrollHandler(const ADirection: TAutoscrollDirection; var AStop: Boolean);
var
  PreviousCaretPosition: TCaretPosition;
begin
  PreviousCaretPosition := CaretPosition;
  case ADirection of
    TAutoscrollDirection.LeftToRight:
      CaretPosition := TCaretPosition.Create(CaretPosition.Line, CaretPosition.Pos + 1);

    TAutoscrollDirection.RightToLeft:
      CaretPosition := TCaretPosition.Create(CaretPosition.Line, Max(0, CaretPosition.Pos - 1));

    TAutoscrollDirection.BottomToTop,
    TAutoscrollDirection.LeftBottomToRightTop,
    TAutoscrollDirection.RightBottomToLeftTop:
      MoveCaretVertical(-1);

    TAutoscrollDirection.TopToBottom,
    TAutoscrollDirection.RightTopToLeftBottom:
      MoveCaretVertical(1);

    TAutoscrollDirection.LeftTopToRightBottom:
    begin
      MoveCaretVertical(1);
      if CaretPosition.Line = LinesLayout.Count - 1 then
        GotoLineEnd;
    end;
  end;

  AStop := PreviousCaretPosition = CaretPosition;
  if AStop then
    Exit;

  FSelectionController.Finish := CaretPosition;
end;

procedure TFixedStyledMemo.DoBeginUpdate;
begin
  inherited;
  FLinesLayout.BeginUpdate;
end;

procedure TFixedStyledMemo.DoChange;
begin
  if not (csLoading in ComponentState) then
    Model.Change;
end;

procedure TFixedStyledMemo.CMGesture(var EventInfo: TGestureEventInfo);
begin
  case EventInfo.GestureID of
    igiLongTap:
      LongTap(AbsoluteToLocal(EventInfo.Location), EventInfo.Flags);

    igiDoubleTap:
      DblTap;
  else
    inherited;
  end;
end;

procedure TFixedStyledMemo.ContextMenuItemClick(Sender: TObject);
var
  Action: TContextAction;
begin
  if Sender is TContextMenuItem then
  begin
    Action := TContextMenuItem(Sender).ContextAction;
    ExecuteContextAction(Action);
  end;
end;

function TFixedStyledMemo.ShowContextMenu(const ScreenPosition: TPointF): Boolean;
var
  LCaretPosition: TCaretPosition;
begin
  Result := inherited;
  FSelectionMethods := [];
  if not Result and not (csDesigning in ComponentState) then
  begin
    UpdatePopupMenuItems(FMemoPopupMenu);
    if Model.CheckSpelling then
    begin
      LCaretPosition := FLinesLayout.GetCaretPositionByPoint(ScreenToLocal(ScreenPosition));
      FSpellingManager.AddSuggestionsToPopupMenu(FMemoPopupMenu, LCaretPosition);
    end;

    if Root <> nil then
      FMemoPopupMenu.Parent := Root.GetObject;
    try
      FMemoPopupMenu.Popup(Round(ScreenPosition.X), Round(ScreenPosition.Y));
    finally
      FMemoPopupMenu.Parent := nil;
    end;
    Result := True;
  end;
end;

procedure TFixedStyledMemo.FreeNotification(AObject: TObject);
begin
  inherited;
  if AObject = FMemoPopupMenu then
    FMemoPopupMenu := nil;
end;

procedure TFixedStyledMemo.FreeStyle;
begin
  inherited;
end;

procedure TFixedStyledMemo.SetCaretPosition(const Value: TCaretPosition);
var
  NewCaretPosition: TCaretPosition;
  IsLineChanged: Boolean;
begin
  NewCaretPosition := NormalizeCaretPosition(Value);
  IsLineChanged := FCaretPosition.Line <> NewCaretPosition.Line;
  FCaretPosition := NewCaretPosition;

  if FTextService <> nil then
  begin
    if IsLineChanged then
      UpdateTextInTextService;
    FTextService.MarkedTextPosition := FCaretPosition;
    FTextService.CaretPosition := FCaretPosition;
  end;

  if IsUpdating then
    FNeedUpdateContentOffset := True
  else
  begin
    UpdateCaretPosition;
    UpdateContentOffset;
    RepaintEdit;
  end;
end;

procedure TFixedStyledMemo.RecalcOpacity;
begin
  inherited;
  FLinesLayout.Opacity := AbsoluteOpacity;
  FImeLayout.Opacity := AbsoluteOpacity;
end;

procedure TFixedStyledMemo.DblTap;
begin
end;

function TFixedStyledMemo.GetTextService: TTextService;
begin
  Result := FTextService;
end;

function TFixedStyledMemo.GetVisibleSelectionRegion: TRegion;

  function Max(const AValue1, AValue2: TCaretPosition): TCaretPosition;
  begin
    if AValue1 >= AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  function Min(const AValue1, AValue2: TCaretPosition): TCaretPosition;
  begin
    if AValue1 <= AValue2 then
      Result := AValue1
    else
      Result := AValue2;
  end;

  function IntersectSegments(const A1, A2, B1, B2: TCaretPosition; var C1, C2: TCaretPosition): Boolean;
  var
    IsABIntersection: Boolean;
    IsBAIntersection: Boolean;
  begin
    Assert(A1 <= A2);
    Assert(B1 <= B2);

    IsABIntersection := (B1 <= A2) and (A2 <= B2);
    IsBAIntersection := (A1 <= B2) and (B2 <= A2);
    Result := IsABIntersection or IsBAIntersection;
    if Result then
    begin
      C1 := Max(A1, B1);
      C2 := Min(A2, B2);
    end;
  end;

var
  VisibleSelBegin: TCaretPosition;
  VisibleSelEnd: TCaretPosition;
  VisibleSelLength: Integer;
  FirstVisibleLine: TCaretPosition;
  LastVisibleLine: TCaretPosition;
begin
  // FirstVisibleLineIndex and LastVisibleLineIndex can have invalid values in case of updating process.
  if (FLinesLayout.FirstVisibleLineIndex = -1) or (FLinesLayout.LastVisibleLineIndex = -1) or FLinesLayout.IsUpdating then
    Exit(nil);

  FirstVisibleLine := TCaretPosition.Create(FLinesLayout.FirstVisibleLineIndex, 0);
  LastVisibleLine := TCaretPosition.Create(FLinesLayout.LastVisibleLineIndex, Model.Lines[FLinesLayout.LastVisibleLineIndex].Length);

  if IntersectSegments(FSelectionController.SelBegin, FSelectionController.SelEnd,
                       FirstVisibleLine, LastVisibleLine,
                       VisibleSelBegin, VisibleSelEnd) then
  begin
    VisibleSelLength := Model.PosToTextPos(VisibleSelEnd) - Model.PosToTextPos(VisibleSelBegin);
    Result := FLinesLayout.GetRegionForRange(VisibleSelBegin, VisibleSelLength);
  end
  else
    Result := [];
end;

procedure TFixedStyledMemo.IMEStateUpdated;
var
  LCaret: TCaretPosition;
begin
  if FTextService <> nil then
  begin
    LCaret := FTextService.TargetClausePosition;
    if Model.Lines.Count > 0 then
      LCaret.Line := EnsureRange(LCaret.Line, 0, Model.Lines.Count - 1)
    else
       // This situation can occur when text is empty and user begins to input text from IME text input.
      LCaret.Line := 0;

    FImeLayout.Text := FTextService.MarkedText;
    UpdateCaretPosition;
    RepaintEdit;
  end;
end;

function TFixedStyledMemo.GetTargetClausePointF: TPointF;
var
  TmpPt: TPointF;
begin
  if FTextService = nil then
    Result := TPointF.Zero
  else
  begin
    TmpPt := GetCaretPositionPoint(FTextService.MarkedTextPosition);
    TmpPt.Offset(0, FImeLayout.BoundsRect.Height + IMEWindowGap);
    Result := Content.LocalToAbsolute(TmpPt);
  end;
end;

procedure TFixedStyledMemo.Undo;
begin
  if not Model.ReadOnly then
    FActionStack.RollBackAction;
end;

procedure TFixedStyledMemo.UpdateCaretPosition;

  function GetCaretSize: TSizeF;
  begin
    if (CaretPosition.Line >= 0) and (CaretPosition.Line < FLinesLayout.Count) and not LinesLayout.IsWordWrap then
      Result := TPointF.Create(Model.Caret.Size.cx, FLinesLayout[CaretPosition.Line].Size.Height)
    else
      Result := TPointF.Create(Model.Caret.Size.cx, FLinesLayout.GetLineHeight);
  end;

  function GetImeCaretPosition: TPointF;
  var
    MarkedTextPosition: Integer;
    OffsetPosition: Integer;
    TextRange: TTextRange;
    CaretPoint: TPointF;
  begin
    MarkedTextPosition := Model.PosToTextPos(FTextService.MarkedTextPosition);
    OffsetPosition := Model.PosToTextPos(FTextService.TargetClausePosition) - MarkedTextPosition;
    TextRange := TTextRange.Create(OffsetPosition, 1);
    CaretPoint := GetCaretPositionPoint(CaretPosition);
    FImeLayout.TopLeft := TPointF.Create(ViewportPosition.X, CaretPoint.Y);
    FImeLayout.FirstLineOffset := CaretPoint.X - ViewportPosition.X;
    Result := FImeLayout.GetPositionPoint(OffsetPosition);
  end;

begin
  Model.Caret.BeginUpdate;
  try
    Model.Caret.Size := GetCaretSize;
    if HasImeMarkedText then
      Model.Caret.Pos := GetImeCaretPosition
    else
      Model.Caret.Pos := GetCaretPositionPoint(CaretPosition);
  finally
     Model.Caret.EndUpdate;
  end;
end;

function TFixedStyledMemo.IsCurrentWordWrong: Boolean;
begin
  Result := FSpellingManager.IsWordWrong(CaretPosition);
end;

function TFixedStyledMemo.IsSelecting: Boolean;
begin
  Result := FSelectionMethods <> [];
end;

function TFixedStyledMemo.IsSpellCheckEnabled: Boolean;
begin
  Result := Model.CheckSpelling;
end;

function TFixedStyledMemo.GetListOfPrepositions: TArray<string>;
begin
  Result := FSpellingManager.GetListOfPrepositions(CaretPosition);
end;

procedure TFixedStyledMemo.Spell(const AWord: string);
var
  LCaretPosition: TCaretPosition;
begin
  LCaretPosition := CaretPosition;
  FSpellingManager.Spell(LCaretPosition, AWord);
  CaretPosition := LCaretPosition;
end;

procedure TFixedStyledMemo.StartAutoScroll(const ALocalPoint: TPointF);
var
  Rect: TRectF;
  Direction: TAutoscrollDirection;
begin
  if not HasText then
    Exit;

  Rect := ViewportRect;
  if (ALocalPoint.X <= Rect.Left) and (ALocalPoint.Y <= Rect.Top) then
    Direction := TAutoscrollDirection.RightBottomToLeftTop
  else if (ALocalPoint.X <= Rect.Left) and (ALocalPoint.Y >= Rect.Bottom) then
    Direction := TAutoscrollDirection.RightTopToLeftBottom
  else if (ALocalPoint.X >= Rect.Right) and (ALocalPoint.Y <= Rect.Top) then
    Direction := TAutoscrollDirection.LeftBottomToRightTop
  else if (ALocalPoint.X >= Rect.Right) and (ALocalPoint.Y >= Rect.Bottom) then
    Direction := TAutoscrollDirection.LeftTopToRightBottom
  else if ALocalPoint.X <= Rect.Left then
    Direction := TAutoscrollDirection.RightToLeft
  else if ALocalPoint.X >= Rect.Right then
    Direction := TAutoscrollDirection.LeftToRight
  else if ALocalPoint.Y <= Rect.Top then
    Direction := TAutoscrollDirection.BottomToTop
  else if ALocalPoint.Y >= Rect.Bottom then
    Direction := TAutoscrollDirection.TopToBottom
  else
  begin
    FAutoscrollController.Stop;
    Exit;
  end;

  FAutoscrollController.Start(Direction);
end;

procedure TFixedStyledMemo.StartIMEInput;
begin
  if FTextService = nil then
    Exit;

  Model.Caret.TemporarilyHidden := NeedHideCaretInIME;

  UpdateTextInTextService;
  FTextService.CaretPosition := CaretPosition;
  FTextService.MarkedTextPosition := CaretPosition;
  UpdateCaretPosition;
end;

function TFixedStyledMemo.GetPageSize: Single;
begin
  Result := Model.ViewportSize.Height / FLinesLayout.GetLineHeight;
end;

procedure TFixedStyledMemo.PMInit(var Message: TDispatchMessage);
var
  I: Integer;
begin
  inherited;
  if FTextService <> nil then
    FTextService.MaxLength := Model.MaxLength;
  if HasText then
  begin
    BeginUpdate;
    try
      for I := 0 to Model.Lines.Count - 1 do
        FLinesLayout.InsertLine(I, Model.Lines[I]);
    finally
      EndUpdate;
    end;
  end;
  if Model.Caret.Flasher <> nil then
    FLinesLayout.CaretWidth := Model.Caret.Flasher.Size.Width;
  Content.OnPainting := ContentPaint;
  Content.OnGetClipRect := ContentGetClipRectHandler;
end;

procedure TFixedStyledMemo.PMRootChanged(var Message: TDispatchMessageWithValue<IRoot>);
begin
  inherited;
end;

procedure TFixedStyledMemo.PMSelectText(var Message: TDispatchMessage);
begin
  FSelectionController.SetRange(Model.SelStart, Model.SelLength);
  CaretPosition := FSelectionController.SelEnd;
end;

function TFixedStyledMemo.ConvertLocalPointFrom(const AControl: TControl; const AControlLocalPoint: TPointF): TPointF;
begin
  Assert(AControl <> nil);

  Result := AControl.LocalToAbsolute(AControlLocalPoint);
  Result := AbsoluteToLocal(Result);
end;

function TFixedStyledMemo.ConvertLocalPointTo(const AControl: TControl; const ALocalPoint: TPointF): TPointF;
begin
  Assert(AControl <> nil);

  Result := LocalToAbsolute(ALocalPoint);
  Result := AControl.AbsoluteToLocal(Result);
end;

procedure TFixedStyledMemo.PutCaretTo(const X, Y: Single; const APositionByWord: Boolean);
type
  TRelativePositionToInitialSelection = (Before, &In, After);

  function DefineRelativePosition(const ACaretPosition: TCaretPosition): TRelativePositionToInitialSelection;
  begin
    if ACaretPosition < SelectionController.HoldSelBegin then
      Result := TRelativePositionToInitialSelection.Before
    else if SelectionController.HoldSelEnd < ACaretPosition then
      Result := TRelativePositionToInitialSelection.After
    else
      Result := TRelativePositionToInitialSelection.&In;
  end;

  function GetPrevWordEnd(const ACaretPosition: TCaretPosition): TCaretPosition;
  var
    WordStartIndex: Integer;
    WordEndIndex: Integer;
  begin
    Result := ACaretPosition;
    if FindWordBound(Model.Lines[ACaretPosition.Line], ACaretPosition.Pos, WordStartIndex, WordEndIndex) then
      if InRange(ACaretPosition.Pos, WordStartIndex, WordEndIndex) then
        Result.Pos := WordStartIndex
      else
        Result.Pos := WordEndIndex + 1
    else
    begin
      Result := Model.GetPrevWordBegin(ACaretPosition);
      if FindWordBound(Model.Lines[Result.Line], Result.Pos, WordStartIndex, WordEndIndex) then
        Result.Pos := WordEndIndex + 1;
    end;
  end;

  function FindNextWordBegin(const ACaretPosition: TCaretPosition): TCaretPosition;
  var
    WordStartIndex: Integer;
    WordEndIndex: Integer;
    NextWordCaretPosition: TCaretPosition;
  begin
    Result := ACaretPosition;
    if FindWordBound(Model.Lines[Result.Line], Result.Pos, WordStartIndex, WordEndIndex) then
    begin
      if Result.Pos = WordStartIndex then
        // We are at the word beginning
        Result.Pos := WordStartIndex
      else
        // Caret is in the bounds of new word, so move caret to the word end
        Result.Pos := WordEndIndex + 1
    end
    else
    begin
      // Caret is in words separators (spaces, coma, column)
      NextWordCaretPosition := Model.GetNextWordBegin(Result);
      if NextWordCaretPosition = Result then
      begin
        Result := NextWordCaretPosition;
        Result.Pos := Result.Pos +1;
      end
      else
        Result := NextWordCaretPosition;
    end;
  end;

  function FindPreviousWordEnd(const ACaretPosition: TCaretPosition): TCaretPosition;
  var
    WordStartIndex: Integer;
    WordEndIndex: Integer;
  begin
    Result := ACaretPosition;

    if FindWordBound(Model.Lines[Result.Line], Result.Pos, WordStartIndex, WordEndIndex) and InRange(Result.Pos, WordStartIndex, WordEndIndex) then
      // ¬нутри слова. сдвигаем курсор на начало слова
      Result.Pos := WordStartIndex
    else
      //  аретка попала на разделитель между словами, должны округлить позицию до конца предыдущего слова
      Result := GetPrevWordEnd(Result);
  end;

var
  ContentPoint: TPointF;
  NewCaretPosition: TCaretPosition;
  RelativePosition: TRelativePositionToInitialSelection;
begin
  // X, Y in the presentation coordinate system
  ContentPoint := ConvertLocalPointTo(Content, TPointF.Create(X, Y));

  NewCaretPosition := FLinesLayout.GetCaretPositionByPoint(ContentPoint, False);
  if APositionByWord then
    if SelectionController.IsSelected then
    begin
      // When user uses selection by words, we have to keep initial selected word independently of selection direction.
      RelativePosition := DefineRelativePosition(NewCaretPosition);
      case RelativePosition of
        TRelativePositionToInitialSelection.Before:
        begin
          SelectionController.SetRange(SelectionController.HoldSelEnd, SelectionController.SelBegin);
          NewCaretPosition := FindPreviousWordEnd(NewCaretPosition);
        end;
        TRelativePositionToInitialSelection.In:
        begin
          SelectionController.SetRange(SelectionController.HoldSelBegin, SelectionController.HoldSelEnd);
          NewCaretPosition := SelectionController.HoldSelEnd;
        end;
        TRelativePositionToInitialSelection.After:
        begin
          SelectionController.SetRange(SelectionController.HoldSelBegin, SelectionController.SelEnd);
          NewCaretPosition := FindNextWordBegin(NewCaretPosition);
        end;
      end;
    end
    else
      NewCaretPosition := Model.GetNextWordBegin(NewCaretPosition);

  CaretPosition := NewCaretPosition;
end;

function TFixedStyledMemo.HasImeMarkedText: Boolean;
begin
  Result := (FTextService <> nil) and FTextService.HasMarkedText;
end;

function TFixedStyledMemo.HasText: Boolean;
begin
  Result := Model.Lines.Count > 0;
end;

procedure TFixedStyledMemo.HighlightSpell;
begin
  FSpellingManager.HighlightSpell(FLinesLayout, CaretPosition);
  RepaintEdit;
end;

function TFixedStyledMemo.GetSelection: string;
begin
  Result := Model.SelectedText;
end;

function TFixedStyledMemo.GetSelectionBounds: TRect;
begin
  if FSelectionController.IsSelected then
    Result := TRect.Create(FSelectionController.SelBegin, FSelectionController.SelEnd)
  else
    Result := TRect.Create(FCaretPosition, FCaretPosition);
end;

function TFixedStyledMemo.GetSelectionPointSize: TSizeF;
begin
  Result := TSizeF.Create(0, 0);
end;

function TFixedStyledMemo.GetSelectionRect: TRectF;
var
  TmpRect, SelRect: TRectF;
  Region: TRegion;
  I: Integer;
  TmpPt: TPointF;
begin
  Region := FLinesLayout.GetRegionForRange(CaretPosition, 1);
  if Length(Region) = 0 then
    TmpPt := TPointF.Zero
  else
    TmpPt := Region[0].TopLeft;

  Result := TRectF.Create(TmpPt, 1, FLinesLayout.GetLineHeight);
  if NeedShowSelection and (Model.SelLength > 0) then
  begin
    Region := GetVisibleSelectionRegion;
    if Length(Region) > 0 then
      Result := Region[0];
    SelRect := ViewportRect;
    for I := Low(Region) to High(Region) do
    begin
      IntersectRect(TmpRect, Region[I], SelRect);
      Result := TRectF.Union(Result, TmpRect);
    end;
  end;
  Result.Location := ConvertLocalPointFrom(Content, Result.TopLeft);
end;

function TFixedStyledMemo.GetSelectionRegion: TRegion;
var
  LCaret: TCaretPosition;
begin
  LCaret := FSelectionController.SelBegin;
  Result := FLinesLayout.GetRegionForRange(LCaret, Model.SelLength);
end;

procedure TFixedStyledMemo.SelectionChangedHandler(Sender: TObject; const ASelStart, ALength: Integer);
begin
  Model.DisableNotify;
  try
    Model.SelStart := ASelStart;
    Model.SelLength := ALength;
  finally
    Model.EnableNotify;
  end;
  RepaintEdit;
end;

procedure TFixedStyledMemo.MoveCaretVertical(const ALineDelta: Integer);
var
  Pt: TPointF;
  NewCaretPosition: TCaretPosition;
begin
  Pt := Model.Caret.Pos;
  Pt.Offset(0, FLinesLayout.GetLineHeight / 2 + ALineDelta * FLinesLayout.GetLineHeight);

  NewCaretPosition := FLinesLayout.GetCaretPositionByPoint(Pt);
  if not NewCaretPosition.IsInvalid then
  begin
    FCaretPosition := NewCaretPosition;
    UpdateCaretPosition;
    UpdateContentOffset;
  end;
end;

procedure TFixedStyledMemo.MoveCaretPageDown;
var
  ScrollLineNumber: Integer;
begin
  ScrollLineNumber := Max(1, Trunc(GetPageSize));
  MoveCaretVertical(ScrollLineNumber);
end;

procedure TFixedStyledMemo.MoveCaretPageUp;
var
  ScrollLineNumber: Integer;
begin
  ScrollLineNumber := Max(1, Trunc(GetPageSize));
  MoveCaretVertical(-ScrollLineNumber);
end;

function TFixedStyledMemo.NeedHideCaretInIME: Boolean;
begin
{$IFDEF MACOS}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

function TFixedStyledMemo.NeedShowSelection: Boolean;
begin
  Result := IsFocused or not Model.HideSelectionOnExit and FSelectionController.IsSelected;
end;

function TFixedStyledMemo.NormalizeCaretPosition(const Value: TCaretPosition): TCaretPosition;
begin
  if Value.IsInvalid or (Model.Lines.Count = 0) then
    Result := TCaretPosition.Zero
  else
  begin
    Result.Line := EnsureRange(Value.Line, 0, Model.Lines.Count - 1);
    Result.Pos := EnsureRange(Value.Pos, 0, Model.Lines[Result.Line].Length);
  end;
end;

procedure TFixedStyledMemo.NormalizeCaretPosition;
begin
  CaretPosition := NormalizeCaretPosition(CaretPosition);
end;

initialization
  TPresentationProxyFactory.Current.Replace(TMemo, TControlType.Styled, TStyledPresentationProxy<TFixedStyledMemo>);
finalization
  TPresentationProxyFactory.Current.Unregister(TMemo, TControlType.Styled, TStyledPresentationProxy<TFixedStyledMemo>);
end.
