unit uPageControlWithCloseButtons;

interface

uses
  Classes, Controls, ComCtrls, Types, UITypes;

type
  TOnCloseTabEvent = procedure (const ATabIndex: Integer) of object;

  TPageControlWithCloseButtons = class (TPageControl)
  private
    FCloseButtonsRect: array of TRect;
    FCloseButtonMouseDownIndex: Integer;
    FCloseButtonShowPushed: Boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCloseTab: TOnCloseTabEvent;
    procedure PageControlCloseButtonDrawTab(Control: TCustomTabControl;
      TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure PageControlCloseButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonMouseLeave(Sender: TObject);
    procedure PageControlCloseButtonMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControlCloseButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PageCountChanged;   //should be done on every change of the page count

    property OnMouseDown2: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseLeave2: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove2: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp2: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnCloseTab: TOnCloseTabEvent read FOnCloseTab write FOnCloseTab;
  end;

implementation

uses
  Windows, UxTheme, Themes, Math;

{ TPageControlWithCloseButtons }

constructor TPageControlWithCloseButtons.Create(AOwner: TComponent);
begin
  inherited;
  OwnerDraw := True;
  OnDrawTab := PageControlCloseButtonDrawTab;
  OnMouseDown := PageControlCloseButtonMouseDown;
  OnMouseUp := PageControlCloseButtonMouseUp;
  OnMouseLeave := PageControlCloseButtonMouseLeave;
  OnMouseMove := PageControlCloseButtonMouseMove;
end;

destructor TPageControlWithCloseButtons.Destroy;
begin

  inherited;
end;

procedure TPageControlWithCloseButtons.PageCountChanged;
var
  i: Integer;
begin
  SetLength(FCloseButtonsRect, PageCount);
  FCloseButtonMouseDownIndex := -1;

  for i := 0 to Length(FCloseButtonsRect) - 1 do
  begin
    FCloseButtonsRect[I] := Rect(0, 0, 0, 0);
  end;
end;

procedure TPageControlWithCloseButtons.PageControlCloseButtonDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  CloseBtnSize: Integer;
  PageControl: TPageControl;
  TabCaption: TPoint;
  CloseBtnRect: TRect;
  CloseBtnDrawState: Cardinal;
  CloseBtnDrawDetails: TThemedElementDetails;
begin
  PageControl := Control as TPageControl;

  if InRange(TabIndex, 0, Length(FCloseButtonsRect) - 1) then
  begin
    CloseBtnSize := 14;
    TabCaption.Y := Rect.Top + 3;

    if Active then
    begin
      CloseBtnRect.Top := Rect.Top + 4;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 6;
    end
    else
    begin
      CloseBtnRect.Top := Rect.Top + 3;
      CloseBtnRect.Right := Rect.Right - 5;
      TabCaption.X := Rect.Left + 3;
    end;

    CloseBtnRect.Bottom := CloseBtnRect.Top + CloseBtnSize;
    CloseBtnRect.Left := CloseBtnRect.Right - CloseBtnSize;
    FCloseButtonsRect[TabIndex] := CloseBtnRect;

    PageControl.Canvas.FillRect(Rect);
    PageControl.Canvas.TextOut(TabCaption.X, TabCaption.Y, PageControl.Pages[TabIndex].Caption);

    if not UseThemes then
    begin
      if (FCloseButtonMouseDownIndex = TabIndex) and FCloseButtonShowPushed then
        CloseBtnDrawState := DFCS_CAPTIONCLOSE + DFCS_PUSHED
      else
        CloseBtnDrawState := DFCS_CAPTIONCLOSE;

      Windows.DrawFrameControl(PageControl.Canvas.Handle,
        FCloseButtonsRect[TabIndex], DFC_CAPTION, CloseBtnDrawState);
    end
    else
    begin
      Dec(FCloseButtonsRect[TabIndex].Left);

      if (FCloseButtonMouseDownIndex = TabIndex) and FCloseButtonShowPushed then
        CloseBtnDrawDetails := ThemeServices.GetElementDetails(twCloseButtonPushed)
      else
        CloseBtnDrawDetails := ThemeServices.GetElementDetails(twCloseButtonNormal);

      ThemeServices.DrawElement(PageControl.Canvas.Handle, CloseBtnDrawDetails,
        FCloseButtonsRect[TabIndex]);
    end;
  end;
end;

procedure TPageControlWithCloseButtons.PageControlCloseButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if Button = mbLeft then
  begin
    for I := 0 to Length(FCloseButtonsRect) - 1 do
    begin
      if PtInRect(FCloseButtonsRect[I], Point(X, Y)) then
      begin
        FCloseButtonMouseDownIndex := I;
        FCloseButtonShowPushed := True;
        PageControl.Repaint;
      end;
    end;
  end;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TPageControlWithCloseButtons.PageControlCloseButtonMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
  Inside: Boolean;
begin
  PageControl := Sender as TPageControl;

  if (ssLeft in Shift) and (FCloseButtonMouseDownIndex >= 0) then
  begin
    Inside := PtInRect(FCloseButtonsRect[FCloseButtonMouseDownIndex], Point(X, Y));

    if FCloseButtonShowPushed <> Inside then
    begin
      FCloseButtonShowPushed := Inside;
      PageControl.Repaint;
    end;
  end;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Sender, Shift, X, Y);
end;

procedure TPageControlWithCloseButtons.PageControlCloseButtonMouseLeave(Sender: TObject);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;
  FCloseButtonShowPushed := False;
  PageControl.Repaint;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Sender);
end;

procedure TPageControlWithCloseButtons.PageControlCloseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PageControl: TPageControl;
begin
  PageControl := Sender as TPageControl;

  if (Button = mbLeft) and (FCloseButtonMouseDownIndex >= 0) then
  begin
    if PtInRect(FCloseButtonsRect[FCloseButtonMouseDownIndex], Point(X, Y)) then
    begin
      if Assigned(FOnCloseTab) then
        FOnCloseTab(FCloseButtonMouseDownIndex);

      FCloseButtonMouseDownIndex := -1;
      PageControl.Repaint;
    end;
  end;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Sender, Button, Shift, X, Y);
end;

end.
