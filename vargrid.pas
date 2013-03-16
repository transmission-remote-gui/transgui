{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2012 by Yury Sidorov.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*************************************************************************************}

unit VarGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, VarList, Graphics, Controls, LMessages, Forms, StdCtrls, LCLType, ExtCtrls;

type
  TVarGrid = class;

  TCellOption = (coDrawCheckBox, coDrawTreeButton);
  TCellOptions = set of TCellOption;

  TCellAttributes = record
    Text: string;
    ImageIndex: integer;
    Indent: integer;
    Options: TCellOptions;
    State: TCheckBoxState;
    Expanded: boolean;
  end;

  TOnCellAttributes = procedure (Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes) of object;
  TOnDrawCellEvent = procedure (Sender: TVarGrid; ACol, ARow, ADataCol: integer; AState: TGridDrawState; const R: TRect; var ADefaultDrawing: boolean) of object;
  TOnSortColumnEvent = procedure (Sender: TVarGrid; var ASortCol: integer) of object;
  TCellNotifyEvent = procedure (Sender: TVarGrid; ACol, ARow, ADataCol: integer) of object;
  TOnQuickSearch = procedure (Sender: TVarGrid; var SearchText: string; var ARow: integer) of object;

  { TVarGrid }

  TVarGrid = class(TCustomDrawGrid)
  private
    FFirstVisibleColumn: integer;
    FHideSelection: boolean;
    FImages: TImageList;
    FItems: TVarList;
    FItemsChanging: boolean;
    FColumnsMap: array of integer;
    FMultiSelect: boolean;
    FOnCellAttributes: TOnCellAttributes;
    FOnCheckBoxClick: TCellNotifyEvent;
    FOnDrawCell: TOnDrawCellEvent;
    FOnQuickSearch: TOnQuickSearch;
    FOnTreeButtonClick: TCellNotifyEvent;
    FSelCount: integer;
    FAnchor: integer;
    FSortColumn: integer;
    FOnSortColumn: TOnSortColumnEvent;
    FRow: integer;
    FHintCell: TPoint;
    FCurSearch: string;
    FSearchTimer: TTimer;

    function GetRow: integer;
    function GetRowSelected(RowIndex: integer): boolean;
    function GetRowVisible(RowIndex: integer): boolean;
    function GetSortOrder: TSortOrder;
    procedure ItemsChanged(Sender: TObject);
    procedure SetHideSelection(const AValue: boolean);
    procedure SetRow(const AValue: integer);
    procedure SetRowSelected(RowIndex: integer; const AValue: boolean);
    procedure SetRowVisible(RowIndex: integer; const AValue: boolean);
    procedure SetSortColumn(const AValue: integer);
    procedure SetSortOrder(const AValue: TSortOrder);
    procedure UpdateColumnsMap;
    procedure UpdateSelCount;
    procedure SelectRange(OldRow, NewRow: integer);
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    function CellNeedsCheckboxBitmaps(const aCol,aRow: Integer): boolean;
    procedure DrawCellCheckboxBitmaps(const aCol,aRow: Integer; const aRect: TRect);
    function FindRow(const SearchStr: string; StartRow: integer): integer;
    procedure DoSearchTimer(Sender: TObject);

  protected
    procedure SizeChanged(OldColCount, OldRowCount: Integer); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure ColRowMoved(IsColumn: Boolean; FromIndex,ToIndex: Integer); override;
    procedure PrepareCanvas(aCol,aRow: Integer; aState:TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure DoOnCellAttributes(ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
    procedure HeaderClick(IsColumn: Boolean; index: Integer); override;
    procedure AutoAdjustColumn(aCol: Integer); override;
    procedure VisualChange; override;
    procedure DrawColumnText(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;
    procedure DblClick; override;
    procedure Click; override;
    procedure GetCheckBoxState(const aCol, aRow:Integer; var aState:TCheckboxState); override;
    procedure SetCheckboxState(const aCol, aRow:Integer; const aState: TCheckboxState); override;
    procedure SetupCell(ACol, ARow: integer; AState: TGridDrawState; out CellAttribs: TCellAttributes);
    procedure DoOnCheckBoxClick(ACol, ARow: integer);
    procedure DoOnTreeButtonClick(ACol, ARow: integer);
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveSelection;
    procedure SelectAll;
    procedure Sort; reintroduce;
    function ColToDataCol(ACol: integer): integer;
    procedure EnsureSelectionVisible;
    procedure BeginUpdate; reintroduce;
    procedure EndUpdate(aRefresh: boolean = true); reintroduce;

    property Items: TVarList read FItems;
    property RowSelected[RowIndex: integer]: boolean read GetRowSelected write SetRowSelected;
    property RowVisible[RowIndex: integer]: boolean read GetRowVisible write SetRowVisible;
    property SelCount: integer read FSelCount;
    property Row: integer read GetRow write SetRow;
    property FirstVisibleColumn: integer read FFirstVisibleColumn;
  published
    property Align;
    property AlternateColor;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FixedCols;
    property FixedRows;
    property Font;
    property GridLineWidth;
    property Options;
    property ParentColor default false;
    property ParentFont;
    property ParentShowHint default false;
    property PopupMenu;
    property RowCount;
    property ScrollBars;
    property ShowHint default True;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle default tsNative;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnResize;

    property Images: TImageList read FImages write FImages;
    property MultiSelect: boolean read FMultiSelect write FMultiSelect default False;
    property SortColumn: integer read FSortColumn write SetSortColumn default -1;
    property SortOrder: TSortOrder read GetSortOrder write SetSortOrder default soAscending;
    property HideSelection: boolean read FHideSelection write SetHideSelection default False;

    property OnCellAttributes: TOnCellAttributes read FOnCellAttributes write FOnCellAttributes;
    property OnDrawCell: TOnDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnSortColumn: TOnSortColumnEvent read FOnSortColumn write FOnSortColumn;
    property OnCheckBoxClick: TCellNotifyEvent read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnTreeButtonClick: TCellNotifyEvent read FOnTreeButtonClick write FOnTreeButtonClick;
    property OnQuickSearch: TOnQuickSearch read FOnQuickSearch write FOnQuickSearch;
  end;

procedure Register;

implementation

uses Variants, Math, GraphType, lclintf, Themes, types, lclproc
     {$ifdef LCLcarbon} , carbonproc {$endif LCLcarbon};

const
  roSelected = 1;
  roCurRow   = 2;

procedure Register;
begin
  RegisterComponents('TransGUI', [TVarGrid]);
end;

{ TVarGrid }

procedure TVarGrid.ItemsChanged(Sender: TObject);
var
  i, OldRows, OldCols: integer;
  pt: TPoint;
begin
  FItemsChanging:=True;
  try
    Perform(CM_MouseLeave, 0, 0);  // Hack to call ResetHotCell to workaround a bug
    OldRows:=RowCount;
    OldCols:=Columns.Count;
    i:=FItems.RowCnt + FixedRows;
    if (FRow = -1) and (inherited Row >= i) and (i > FixedRows) then
      inherited Row:=i - 1;
    RowCount:=i;
    if FRow <> -1 then begin
      Row:=FRow;
      FRow:=-1;
    end;
    UpdateSelCount;
    while Columns.Count > FItems.ColCnt do
      Columns.Delete(Columns.Count - 1);
    if Columns.Count <> FItems.ColCnt then begin
      Columns.BeginUpdate;
      try
        for i:=Columns.Count to FItems.ColCnt - 1 do
          Columns.Add;
      finally
        Columns.EndUpdate;
      end;
    end;
    if (OldRows <> RowCount) or (OldCols <> Columns.Count) then begin
      if Parent <> nil then
        HandleNeeded;
      ResetSizes;
    end
    else
      Invalidate;
    pt:=ScreenToClient(Mouse.CursorPos);
    if PtInRect(ClientRect, pt) then
      MouseMove([], pt.x, pt.y);
  finally
    FItemsChanging:=False;
  end;
end;

procedure TVarGrid.SetHideSelection(const AValue: boolean);
begin
  if FHideSelection=AValue then exit;
  FHideSelection:=AValue;
  Invalidate;
end;

procedure TVarGrid.SetRow(const AValue: integer);
var
  i, r: integer;
begin
  if FItems.IsUpdating then
    FRow:=AValue
  else begin
    r:=AValue + FixedRows;
    if r <> inherited Row then begin
      i:=LeftCol;
      inherited Row:=r;
      LeftCol:=i;
    end;
  end;
end;

function TVarGrid.GetRowSelected(RowIndex: integer): boolean;
begin
  Result:=LongBool(FItems.RowOptions[RowIndex] and roSelected);
end;

function TVarGrid.GetRowVisible(RowIndex: integer): boolean;
begin
  Result:=RowHeights[RowIndex + FixedRows] > 0;
end;

function TVarGrid.GetSortOrder: TSortOrder;
begin
  Result:=inherited SortOrder;
end;

function TVarGrid.GetRow: integer;
begin
  if FItems.IsUpdating and (FRow <> -1) then
    Result:=FRow
  else begin
    Result:=inherited Row - FixedRows;
  end;
end;

procedure TVarGrid.SetRowSelected(RowIndex: integer; const AValue: boolean);
var
  i, j: integer;
begin
  i:=FItems.RowOptions[RowIndex];
  if AValue then begin
    j:=i or roSelected;
    if j <> i then
      Inc(FSelCount);
  end
  else begin
    j:=i and not roSelected;
    if j <> i then
      Dec(FSelCount);
  end;
  FItems.RowOptions[RowIndex]:=j;
  InvalidateRow(RowIndex + FixedRows);
  if FSelCount <= 1 then
    InvalidateRow(inherited Row);
end;

procedure TVarGrid.SetRowVisible(RowIndex: integer; const AValue: boolean);
begin
  if AValue then
    RowHeights[RowIndex + FixedRows]:=DefaultRowHeight
  else
    RowHeights[RowIndex + FixedRows]:=0;
end;

procedure TVarGrid.SetSortColumn(const AValue: integer);
begin
  if FSortColumn=AValue then exit;
  FSortColumn:=AValue;
  if FSortColumn >= 0 then
    Options:=Options + [goHeaderPushedLook, goHeaderHotTracking]
  else
    Options:=Options - [goHeaderPushedLook, goHeaderHotTracking];
  Sort;
end;

procedure TVarGrid.SetSortOrder(const AValue: TSortOrder);
begin
  if SortOrder = AValue then exit;
  inherited SortOrder:=AValue;
  Sort;
end;

procedure TVarGrid.UpdateColumnsMap;
var
  i, j: integer;
begin
  FFirstVisibleColumn:=-1;
  SetLength(FColumnsMap, Columns.Count);
  j:=0;
  for i:=0 to Columns.Count - 1 do
    with Columns[i] do begin
      if (FFirstVisibleColumn < 0) and Visible then
        FFirstVisibleColumn:=i;
      FColumnsMap[j]:=ID - 1;
      Inc(j);
    end;
  SetLength(FColumnsMap, j);
end;

procedure TVarGrid.UpdateSelCount;
var
  i: integer;
begin
  FSelCount:=0;
  for i:=0 to FItems.Count - 1 do
    if RowSelected[i] then
      Inc(FSelCount);
end;

procedure TVarGrid.SelectRange(OldRow, NewRow: integer);
var
  dir: integer;
  sel: boolean;
begin
  if OldRow = NewRow then
    exit;
  if FAnchor = -1 then
    FAnchor:=OldRow;
  dir:=Sign(NewRow - OldRow);
  if Sign(FAnchor - OldRow) <> Sign(FAnchor - NewRow) then
    while OldRow <> FAnchor do begin
      RowSelected[OldRow]:=False;
      Inc(OldRow, dir);
    end;
  sel:=Abs(FAnchor - OldRow) < Abs(FAnchor - NewRow);
  while OldRow <> NewRow do begin
    RowSelected[OldRow]:=sel;
    Inc(OldRow, dir);
  end;
  RowSelected[NewRow]:=True;
end;

procedure TVarGrid.CMHintShow(var Message: TCMHintShow);
var
  ca: TCellAttributes;
  pt: TPoint;
  wd: integer;
  R: TRect;
begin
  with Message.HintInfo^ do begin
    pt:=MouseToCell(CursorPos);
    if (pt.x >= FixedCols) and (pt.y >= 0) then begin
      R:=CellRect(pt.x, pt.y);
      if PtInRect(R, CursorPos) then begin
        SetupCell(pt.x, pt.y, [], ca);
        if ca.Text <> '' then begin
          wd:=Canvas.TextWidth(ca.Text);
          Inc(R.Left, ca.Indent);
          if coDrawTreeButton in ca.Options then
            Inc(R.Left, R.Bottom - R.Top);
          if coDrawCheckBox in ca.Options then
            Inc(R.Left, R.Bottom - R.Top);
          if (ca.ImageIndex <> -1) and Assigned(FImages) then
            Inc(R.Left, FImages.Width + 2);
          if (R.Right <= R.Left) or (R.Right - R.Left < wd + 5) then begin
            HintStr:=ca.Text;
            R.Top:=(R.Top + R.Bottom - Canvas.TextHeight(ca.Text)) div 2 - 4;
            Dec(R.Left);
            HintPos:=ClientToScreen(R.TopLeft);
          end;
          FHintCell:=pt;
        end
        else
          Message.Result:=1;
      end
      else
        Message.Result:=1;
    end;
  end;
end;

function TVarGrid.CellNeedsCheckboxBitmaps(const aCol, aRow: Integer): boolean;
var
  C: TGridColumn;
begin
  Result := false;
  if (aRow>=FixedRows) and Columns.Enabled then begin
    C := ColumnFromGridColumn(aCol);
    result := (C<>nil) and (C.ButtonStyle=cbsCheckboxColumn)
  end;
end;

procedure TVarGrid.DrawCellCheckboxBitmaps(const aCol, aRow: Integer; const aRect: TRect);
var
  AState: TCheckboxState;
begin
  AState := cbUnchecked;
  GetCheckBoxState(aCol, aRow, aState);
  DrawGridCheckboxBitmaps(aCol, aRow, aRect, aState);
end;

function TVarGrid.FindRow(const SearchStr: string; StartRow: integer): integer;
var
  i, c: integer;
  s, ss: string;
  v: variant;
begin
  Result:=-1;
  if Columns.Count = 0 then
    exit;
  c:=SortColumn;
  if (c < 0) or (c >= Items.ColCnt) then
    c:=0;
  ss:=UTF8UpperCase(SearchStr);
  for i:=StartRow to Items.Count - 1 do begin
    v:=Items[c, i];
    if VarIsNull(v) or VarIsEmpty(v) then
      s:=''
    else
      s:=UTF8UpperCase(UTF8Encode(widestring(v)));
    if Copy(s, 1, Length(ss)) = ss then begin
      Result:=i;
      break;
    end;
  end;
end;

procedure TVarGrid.DoSearchTimer(Sender: TObject);
begin
  FSearchTimer.Enabled:=False;
  FCurSearch:='';
end;

procedure TVarGrid.SizeChanged(OldColCount, OldRowCount: Integer);
begin
  if not FItemsChanging and (FItems <> nil) then begin
    FItems.ColCnt:=Columns.Count;
    FItems.RowCnt:=RowCount - FixedRows;
    UpdateColumnsMap;
  end;
  inherited;
end;

procedure TVarGrid.DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  ca: TCellAttributes;
//  ts: TTextStyle;
  dd, IsHeader: boolean;
  R: TRect;
  det: TThemedElementDetails;
  sz: TSize;
  i: integer;
begin
  if RowHeights[aRow] = 0 then
    exit;
  IsHeader:=(gdFixed in aState) and (aRow=0) and (aCol>=FirstGridColumn);
  if not IsHeader and MultiSelect and (FSelCount > 0) then
    if (aRow >= FixedRows) and (aCol >= FixedCols) and RowSelected[aRow - FixedRows] then
      Include(aState, gdSelected)
    else
      Exclude(aState, gdSelected);

  PrepareCanvas(aCol, aRow, aState);
  if DefaultDrawing then
    SetupCell(aCol, aRow, aState, ca);
  if not IsHeader or (TitleStyle<>tsNative) then
    Canvas.FillRect(aRect);

  if not IsHeader then begin
    dd:=True;
    if Assigned(FOnDrawCell) then begin
      R:=CellRect(aCol, aRow);
      if goVertLine in Options then
        Dec(R.Right, 1);
      if goHorzLine in Options then
        Dec(R.Bottom, 1);
      FOnDrawCell(Self, aCol, aRow - FixedRows, ColToDataCol(aCol), aState, R, dd);
    end;

    if DefaultDrawing and dd then begin
      if CellNeedsCheckboxBitmaps(aCol,aRow) then
        DrawCellCheckboxBitmaps(aCol,aRow,aRect)
      else begin
        Inc(aRect.Left, ca.Indent);
        if coDrawTreeButton in ca.Options then begin
          R:=aRect;
          R.Right:=R.Left + (R.Bottom - R.Top);
          aRect.Left:=R.Right;
          if ThemeServices.ThemesEnabled then begin
            if ca.Expanded then
              det:=ThemeServices.GetElementDetails(ttGlyphOpened)
            else
              det:=ThemeServices.GetElementDetails(ttGlyphClosed);
            sz:=ThemeServices.GetDetailSize(det);
            with R do begin
              Left:=(Left + Right - sz.cx) div 2;
              Top:=(Top + Bottom - sz.cy) div 2;
              R:=Bounds(Left, Top, sz.cx, sz.cy);
            end;
            ThemeServices.DrawElement(Canvas.Handle, det, R, nil);
          end
          else
            with Canvas do begin
              i:=(R.Bottom - R.Top) div 4;
              InflateRect(R, -i, -i);
              if (R.Right - R.Left) and 1 = 0 then
                Dec(R.Right);
              if (R.Bottom - R.Top) and 1 = 0 then
                Dec(R.Bottom);
              Pen.Color:=clWindowText;
              Rectangle(R);
              InflateRect(R, -1, -1);
              Brush.Color:=clWindow;
              FillRect(R);
              InflateRect(R, -1, -1);
              i:=(R.Top + R.Bottom) div 2;
              MoveTo(R.Left, i);
              LineTo(R.Right, i);
              if not ca.Expanded then begin
                i:=(R.Left + R.Right) div 2;
                MoveTo(i, R.Top);
                LineTo(i, R.Bottom);
              end;
            end;
        end;
        if coDrawCheckBox in ca.Options then begin
          R:=aRect;
          R.Right:=R.Left + (R.Bottom - R.Top);
          aRect.Left:=R.Right;
          DrawGridCheckboxBitmaps(aCol, aRow, R, ca.State);
        end;
        if (ca.ImageIndex <> -1) and Assigned(FImages) then begin
          FImages.Draw(Canvas, aRect.Left + 2, (aRect.Bottom + aRect.Top - FImages.Height) div 2, ca.ImageIndex, gdeNormal);
          Inc(aRect.Left, FImages.Width + 2);
        end;
        if ca.Text <> '' then begin
{
          if Canvas.TextStyle.Alignment <> taLeftJustify then
            if (aRect.Right <= aRect.Left) or (aRect.Right - aRect.Left < Canvas.TextWidth(ca.Text) + 9) then begin
              ts:=Canvas.TextStyle;
              ts.Alignment:=taLeftJustify;
              Canvas.TextStyle:=ts;
            end;
          DrawCellText(aCol, aRow, aRect, aState, ca.Text);
}
          with aRect do begin
            Inc(Top, 2);
            Inc(Left, constCellPadding);
            Dec(Right, constCellPadding);
            if Right<Left then
              Right:=Left;
            if Left>Right then
              Left:=Right;
            if Bottom<Top then
              Bottom:=Top;
            if Top>Bottom then
              Top:=Bottom;

            if (Left <> Right) and (Top <> Bottom) then begin
              if Canvas.TextStyle.Alignment <> taLeftJustify then begin
                i:=Canvas.TextWidth(ca.Text);
                if i < Right - Left then
                  case Canvas.TextStyle.Alignment of
                    taRightJustify:
                      Left:=Right - i;
                    taCenter:
                      Left:=(Left + Right - i) div 2;
                  end;
              end;
              ExtUTF8Out(Canvas.Handle, Left, Top, ETO_OPAQUE or ETO_CLIPPED, @aRect, PChar(ca.Text), Length(ca.Text), nil);
            end;
          end;

        end;
      end;
    end;
  end;
  if gdFixed in aState then
    DefaultDrawCell(aCol, aRow, aRect, aState)
  else
    DrawCellGrid(aCol,aRow,aRect,aState);
end;

procedure TVarGrid.ColRowMoved(IsColumn: Boolean; FromIndex, ToIndex: Integer);
begin
  inherited ColRowMoved(IsColumn, FromIndex, ToIndex);
  UpdateColumnsMap;
end;

procedure TVarGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
var
  F: TCustomForm;
begin
  F:=GetParentForm(Self);
  if FHideSelection and (FSelCount = 0) and (F <> nil) and (F.ActiveControl <> Self) then
    aState:=aState - [gdSelected];
  inherited PrepareCanvas(aCol, aRow, aState);
  with Canvas do
    if (Font.Color = clWindow) and (Brush.Color = clHighlight) then begin
      Font.Color:=clHighlightText;
{$ifdef LCLgtk2}
      Brush.Color:=ColorToRGB(Brush.Color); // Workaround for LCL bug
{$endif LCLgtk2}
    end;
end;

procedure TVarGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  IsCtrl, CheckBoxClicked: boolean;
  ca: TCellAttributes;
  R, RR: TRect;
begin
{$ifdef LCLcarbon}
  IsCtrl:=ssMeta in GetCarbonShiftState;
{$else}
  IsCtrl:=ssCtrl in Shift;
{$endif LCLcarbon}
  CheckBoxClicked:=False;
  pt:=MouseToCell(Point(X,Y));
  if ssLeft in Shift then begin
    SetupCell(pt.x, pt.y, [], ca);
    RR:=CellRect(pt.x, pt.y);
    Inc(RR.Left, ca.Indent);
    if (RR.Left <= RR.Right) and (coDrawTreeButton in ca.Options) then begin
      R:=RR;
      R.Right:=R.Left + (R.Bottom - R.Top);
      if R.Right > RR.Right then
        R.Right:=RR.Right;
      if PtInRect(R, Point(X,Y)) then begin
        DoOnTreeButtonClick(pt.x, pt.y);
        InvalidateCell(pt.x, pt.y);
      end;
      Inc(RR.Left, RR.Bottom - RR.Top);
    end;
    if (RR.Left <= RR.Right) and (coDrawCheckBox in ca.Options) then begin
      R:=RR;
      R.Right:=R.Left + (R.Bottom - R.Top);
      if R.Right > RR.Right then
        R.Right:=RR.Right;
      if PtInRect(R, Point(X,Y)) then begin
        DoOnCheckBoxClick(pt.x, pt.y);
        InvalidateCell(pt.x, pt.y);
        CheckBoxClicked:=True;
      end;
    end;
  end;
  if (ssRight in Shift) {$ifdef darwin} or (Shift*[ssLeft, ssCtrl] = [ssLeft, ssCtrl]) {$endif} then begin
    SetFocus;
    if (pt.x >= FixedCols) and (pt.y >= FixedRows) then begin
      if MultiSelect and (SelCount > 0) and not RowSelected[pt.y - FixedRows] then
        RemoveSelection;
      Row:=pt.y - FixedRows;
    end;
  end
  else
    if MultiSelect and (ssLeft in Shift) and (pt.x >= FixedCols) and (pt.y >= FixedRows) then begin
      if IsCtrl then begin
        if SelCount = 0 then
          RowSelected[Row]:=True;
        RowSelected[pt.y - FixedRows]:=not RowSelected[pt.y - FixedRows];
        FAnchor:=-1;
      end
      else
        if ssShift in Shift then
          SelectRange(Row, pt.y - FixedRows)
        else begin
          if (SelCount > 0) and not CheckBoxClicked then
            RemoveSelection;
          FAnchor:=-1;
        end;
    end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TVarGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  pt:=MouseToCell(Point(x, y));
  if (FHintCell.x <> -1) and ((FHintCell.x <> pt.x) or (FHintCell.y <> pt.y)) then begin
    Application.CancelHint;
    FHintCell.x:=-1;
  end;
end;

procedure TVarGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  r, k: integer;
  ca: TCellAttributes;
begin
  r:=Row;
  k:=Key;

  if (Shift = []) and ( (k = VK_SPACE) or (k = VK_LEFT) or (k = VK_RIGHT) or (k = VK_ADD) or (k = VK_SUBTRACT) ) then begin
    SetupCell(FixedCols, inherited Row, [], ca);
    case k of
      VK_SPACE:
        if coDrawCheckBox in ca.Options then begin
          DoOnCheckBoxClick(FixedCols, inherited Row);
          exit;
        end;
      VK_LEFT, VK_SUBTRACT:
        if (coDrawTreeButton in ca.Options) and ca.Expanded then begin
          DoOnTreeButtonClick(FixedCols, inherited Row);
          exit;
        end;
      VK_RIGHT, VK_ADD:
        if (coDrawTreeButton in ca.Options) and not ca.Expanded then begin
          DoOnTreeButtonClick(FixedCols, inherited Row);
          exit;
        end;
    end;
  end;

  inherited KeyDown(Key, Shift);

  if MultiSelect then begin
    if ssCtrl in Shift then begin
      if k = VK_SPACE then
        RowSelected[Row]:=not RowSelected[Row];
      FAnchor:=-1;
    end
    else
      if ssShift in Shift then begin
        SelectRange(r, Row);
      end
      else
        if k in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_NEXT, VK_PRIOR] then begin
          if SelCount > 0 then
            RemoveSelection;
          FAnchor:=-1;
        end;
  end;
  if (Key = VK_RETURN) and Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TVarGrid.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  i, r: integer;
begin
  inherited UTF8KeyPress(UTF8Key);
  if UTF8Key = #0 then
    exit;
  FSearchTimer.Enabled:=False;
  FSearchTimer.Enabled:=True;
  if FCurSearch = '' then
    i:=0
  else
    i:=Row;
  FCurSearch:=FCurSearch + UTF8Key;
  if Assigned(FOnQuickSearch) then begin
    r:=i;
    FOnQuickSearch(Self, FCurSearch, r);
    if r <> i then
      Row:=r;
  end
  else begin
    i:=FindRow(FCurSearch, i);
    if i >= 0 then
      Row:=i;
  end;
end;

procedure TVarGrid.DoOnCellAttributes(ACol, ARow, ADataCol: integer; AState: TGridDrawState; var CellAttribs: TCellAttributes);
begin
  if Assigned(FOnCellAttributes) then
    FOnCellAttributes(Self, ACol, ARow, ADataCol, AState, CellAttribs);
end;

procedure TVarGrid.HeaderClick(IsColumn: Boolean; index: Integer);
var
  i: integer;
begin
  inherited HeaderClick(IsColumn, index);
  if IsColumn and (FSortColumn >= 0) then begin
    i:=ColToDataCol(index);
    if FSortColumn = i then begin
      if SortOrder = soAscending then
        SortOrder:=soDescending
      else
        SortOrder:=soAscending;
    end
    else begin
      SortOrder:=soAscending;
      SortColumn:=i;
    end;
  end;
end;

procedure TVarGrid.AutoAdjustColumn(aCol: Integer);
var
  i, j, wd, h: integer;
  ca: TCellAttributes;
begin
  wd:=4;
  for i:=0 to FItems.Count - 1 do begin
    h:=RowHeights[i + FixedRows];
    if h > 0 then begin
      SetupCell(aCol, i, [], ca);
      j:=Canvas.TextWidth(ca.Text) + 6;
      Inc(j, ca.Indent);
      if coDrawTreeButton in ca.Options then
        Inc(j, h);
      if coDrawCheckBox in ca.Options then
        Inc(j, h);
      if (ca.ImageIndex <> -1) and Assigned(FImages) then
        Inc(j, FImages.Width + 2);
      if j > wd then
        wd:=j;
    end;
  end;
  ColumnFromGridColumn(aCol).Width:=wd;
end;

procedure TVarGrid.VisualChange;
begin
  inherited VisualChange;
  if HandleAllocated then
    DefaultRowHeight:=Canvas.TextHeight('Xy') + 5;
  UpdateColumnsMap;
end;

procedure TVarGrid.DrawColumnText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  R: TRect;
  i: integer;
begin
  if (gdFixed in aState) and (aRow=0) and (aCol>=FirstGridColumn) then begin
    R:=aRect;
    if FSortColumn = ColToDataCol(aCol) then begin
      R.Right:=R.Left + R.Bottom - R.Top;
      InflateRect(R, -5, -5);
      OffsetRect(R, -3, 0);
      Dec(R.Bottom, 2);
      aRect.Left:=R.Right + 2;
    end;
    inherited DrawColumnText(aCol, aRow, aRect, aState);
    if FSortColumn = ColToDataCol(aCol) then
      with Canvas do begin
        Pen.Color:=clGrayText;
        i:=(R.Left + R.Right) div 2;
        if SortOrder = soAscending then begin
          MoveTo(i, R.Top);
          LineTo(R.Right, R.Bottom);
          LineTo(R.Left, R.Bottom);
          LineTo(i, R.Top);
        end
        else begin
          MoveTo(R.TopLeft);
          LineTo(R.Right, R.Top);
          LineTo(i, R.Bottom);
          LineTo(R.TopLeft);
        end;
      end;
  end;
end;

procedure TVarGrid.DblClick;
var
  pt: TPoint;
begin
  pt:=MouseToCell(ScreenToClient(Mouse.CursorPos));
  if (pt.y < FixedRows) and (pt.y = 0) and (Cursor <> crHSplit) then
    exit;
  inherited DblClick;
end;

procedure TVarGrid.Click;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TVarGrid.GetCheckBoxState(const aCol, aRow: Integer; var aState: TCheckboxState);
var
  s: string;
begin
  if (aCol >= FixedCols) and (aRow >= FixedRows) then begin
    s:=Items[ColToDataCol(aCol), aRow - FixedRows];
    with Columns[GridColumnFromColumnIndex(aCol)] do
      if s = ValueChecked then
        aState:=cbChecked
      else
        if s = ValueUnchecked then
          aState:=cbUnchecked
        else
          aState:=cbGrayed;
  end;
  inherited GetCheckBoxState(aCol, aRow, aState);
end;

procedure TVarGrid.SetCheckboxState(const aCol, aRow: Integer; const aState: TCheckboxState);
var
  s: string;
begin
  if (aCol >= FixedCols) and (aRow >= FixedRows) then begin
    with Columns[GridColumnFromColumnIndex(aCol)] do
      case aState of
        cbUnchecked:
          s:=ValueUnchecked;
        cbChecked:
          s:=ValueChecked;
        else
          s:='?';
      end;
    Items[ColToDataCol(aCol), aRow - FixedRows]:=s;
  end;
  inherited SetCheckboxState(aCol, aRow, aState);
end;

procedure TVarGrid.SetupCell(ACol, ARow: integer; AState: TGridDrawState; out CellAttribs: TCellAttributes);
var
  v: variant;
  dc: integer;
begin
  if (ACol < 0) or (ARow < 0) then
    exit;
  CellAttribs.ImageIndex:=-1;
  CellAttribs.Indent:=0;
  CellAttribs.Options:=[];
  CellAttribs.State:=cbUnchecked;
  CellAttribs.Expanded:=True;
  if ACol >= FixedCols then begin
    dc:=ColToDataCol(ACol);
    if ARow >= FixedRows then begin
      v:=Items[dc, ARow - FixedRows];
      if not VarIsNull(v) and not VarIsEmpty(v) then
        CellAttribs.Text:=UTF8Encode(WideString(v))
      else
        CellAttribs.Text:='';
    end
    else
      CellAttribs.Text:=ColumnFromGridColumn(ACol).Title.Caption;
  end
  else
    dc:=-1;
  DoOnCellAttributes(ACol - FixedCols, ARow - FixedRows, dc, AState, CellAttribs);
end;

procedure TVarGrid.DoOnCheckBoxClick(ACol, ARow: integer);
var
  i, dc, c: integer;
  ca: TCellAttributes;
  st: TCheckBoxState;
begin
  if Assigned(FOnCheckBoxClick) then begin
    dc:=ColToDataCol(ACol);
    c:=ACol - FixedCols;
    FOnCheckBoxClick(Self, c, ARow - FixedRows, dc);
    if (SelCount > 0) and RowSelected[ARow - FixedRows] then begin
      SetupCell(ACol, ARow, [], ca);
      st:=ca.State;
      for i:=0 to Items.Count - 1 do
        if RowSelected[i] then begin
          SetupCell(ACol, i + FixedRows, [], ca);
          if (coDrawCheckBox in ca.Options) and (ca.State <> st) then
            FOnCheckBoxClick(Self, c, i, dc);
        end;
    end;
  end;
end;

procedure TVarGrid.DoOnTreeButtonClick(ACol, ARow: integer);
begin
  if Assigned(FOnTreeButtonClick) then
    FOnTreeButtonClick(Self, ACol - FixedCols, ARow - FixedRows, ColToDataCol(ACol));
end;

function TVarGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelDown) then
    OnMouseWheelDown(Self, Shift, MousePos, Result);
end;

function TVarGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(OnMouseWheelUp) then
    OnMouseWheelUp(Self, Shift, MousePos, Result);
end;

function TVarGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then begin
    if Mouse.WheelScrollLines = -1 then
      GridMouseWheel(Shift, -WheelDelta*VisibleRowCount div 120)
    else
      GridMouseWheel(Shift, -WheelDelta*Mouse.WheelScrollLines div 120);
    Result := True;
  end;
end;

constructor TVarGrid.Create(AOwner: TComponent);
begin
  FRow:=-1;
  FHintCell.x:=-1;
  inherited Create(AOwner);
  FixedRows:=1;
  FixedCols:=0;
  Options:=[goRowSelect, goThumbTracking, goVertLine, goHorzLine, goColSizing, goColMoving, goDblClickAutoSize, goFixedHorzLine, goFixedVertLine];
  MouseWheelOption:=mwGrid;
  FItems:=TVarList.Create(1, 0);
  FItems.OnDataChanged:=@ItemsChanged;
  ItemsChanged(nil);
  TitleStyle:=tsNative;
  FAnchor:=-1;
  FSortColumn:=-1;
  ShowHint:=True;
  SetLength(FColumnsMap, 1);
  FColumnsMap[0]:=0;
  FSearchTimer:=TTimer.Create(Self);
  with FSearchTimer do begin
    Enabled:=False;
    Interval:=1500;
    OnTimer:=@DoSearchTimer;
  end;
end;

destructor TVarGrid.Destroy;
begin
  inherited Destroy;
  FItems.Free;
end;

procedure TVarGrid.RemoveSelection;
var
  i: integer;
begin
  for i:=0 to FItems.Count - 1 do
    RowSelected[i]:=False;
  FSelCount:=0;
end;

procedure TVarGrid.SelectAll;
var
  i: integer;
begin
  for i:=0 to FItems.Count - 1 do
    RowSelected[i]:=True;
end;

procedure TVarGrid.Sort;
var
  i, c: integer;
begin
  if (FSortColumn >= 0) and (FItems.Count > 0) then begin
    c:=FSortColumn;
    if Assigned(FOnSortColumn) then
      FOnSortColumn(Self, c);
    if not FItems.IsUpdating and (Row >= 0) and (Row < FItems.Count) then
      FItems.RowOptions[Row]:=FItems.RowOptions[Row] or roCurRow;
    FItems.Sort(c, SortOrder = soDescending);
    if not FItems.IsUpdating then begin
      for i:=0 to FItems.Count - 1 do
        if LongBool(FItems.RowOptions[i] and roCurRow) then begin
          FItems.RowOptions[i]:=FItems.RowOptions[i] and not roCurRow;
          Row:=i;
          break;
        end;
      Invalidate;
    end;
  end;
end;

function TVarGrid.ColToDataCol(ACol: integer): integer;
begin
  if (ACol >= FixedCols) and (ACol <= High(FColumnsMap)) then
    Result:=FColumnsMap[ACol]
  else
    Result:=-1;
end;

procedure TVarGrid.EnsureSelectionVisible;
var
  i: integer;
begin
  if FSelCount > 0 then
    for i:=0 to FItems.Count - 1 do
      if RowSelected[i] then begin
        Row:=i;
        break;
      end;

  if inherited Row < TopRow then
    TopRow:=inherited Row
  else
    if inherited Row > GCache.FullVisibleGrid.Bottom then
      TopRow:=inherited Row - (GCache.FullVisibleGrid.Bottom - GCache.FullVisibleGrid.Top);
end;

procedure TVarGrid.BeginUpdate;
begin
  inherited BeginUpdate;
  Items.BeginUpdate;
end;

procedure TVarGrid.EndUpdate(aRefresh: boolean);
begin
  inherited EndUpdate(aRefresh);
  Items.EndUpdate;
end;

end.

