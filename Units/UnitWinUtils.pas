unit UnitWinUtils;

interface uses Windows, Messages, Classes, WinApi.ShellApi, Vcl.ExtCtrls, SysUtils;

var
  InternalFont: THandle;
type
  TBallonIcon = (biNone, biInfo, biWarning, biError);
  TStringEvent = procedure(Text: string) of object;

  TTimedSearch = class
  private
    FText: string;
    FTimer: TTimer;
    FCallback: TStringEvent;
    procedure FOnTick(Sender: TObject);
  public
    constructor Create();
    destructor Destroy(); override;
    function KeyPressed(const Key: Char): string;
    function GetMatched(const Criteria: string; const Items: TStrings): integer;
    procedure Handler(Sender: TObject; var Key: Char);
    property OnSearch: TStringEvent read FCallback write FCallback;
    property Text: string read FText;
  end;

  procedure wuShowBalloon(Edit: HWND; Title: string; Text: string;  Icon: TBallonIcon);
  procedure wuLoadInternalFont();
  procedure wuUnloadInternalFont();
  procedure wuOpenUrl(const Handle: HWND; const URL: string);
implementation
const
  EM_SHOWBALLOONTIP = $1503;
type
  PEditBalloonTip = ^EditBalloonTip;
  EDITBALLOONTIP = record
    cbStruct: DWORD;
    pszTitle: PWChar;
    pszText: PWChar;
    ttiIcon: Integer;
  end;


constructor TTimedSearch.Create();
begin
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 1500;
  FTimer.OnTimer := FOnTick;
end;

destructor TTimedSearch.Destroy();
begin
  FTimer.Free();
end;

procedure TTimedSearch.FOnTick(Sender: TObject);
begin
  FTimer.Enabled := false;
end;

function TTimedSearch.KeyPressed(const Key: Char): string;
var
  sOrigText: string;
begin
  sOrigText := FText;
  if not FTimer.Enabled then
  begin
    FText := '';
  end;

  if Key = #8 then
  begin
    //if Length(FText) == 0 then

    FText := Copy(FText, 0, Length(FText) - 1);
  end;

  if (Ord(Key) >= 65) and (Ord(Key) <= 122) then
  begin
    FText := FText + Key;
  end;
  FTimer.Enabled := false;
  FTimer.Enabled := true;

  if (sOrigText <> FText) and (Assigned(FCallback)) then
  begin
    FCallback(FText);
  end;

  Result := FText;
end;

function TTimedSearch.GetMatched(const Criteria: string; const Items: TStrings): integer;
  (**
   * Idea: calculate cost of string.
   *  Every matched char (ignore case) = 1
   *  Every matched first word char = 2
   *  Every missed char = -1
   *  Exact match (ignore case) = 1000
   **)
  function GetTextCost(Pattern: string; Text: string): integer;
  const
    CostExact = 1000;
    CostChar =  3;
    CostWord =  15;
    CostMiss = -1;
    CostWordMissPenalty = -5;
  var
    p, i: integer;
    ps, ts: string;
  begin
    Result := 0;
    // force check exact match
    if LowerCase(Pattern) = LowerCase(Text) then
    begin
      Result := CostExact;
      exit;
    end;

    p := 1;
    for i := 1 to Length(Text) do
    begin
      if p > Length(Pattern) then
      begin
        exit;
      end;

      ps := Copy(Pattern, p, 1);
      ts := Copy(Text, i, 1);

      if UpperCase(ps) = ts then
      begin
        Inc(Result, CostWord);
        Inc(p);
      end else if ps = ts then
      begin
        Inc(Result, CostChar);
        Inc(p);
      end else begin
        if UpperCase(ts) = ts then
        begin
          Inc(Result, CostWordMissPenalty);
        end;

        Inc(Result, CostMiss);
      end;
    end;
  end;
var
  i, iTopCost, iNewCost: integer;
begin
  Result := -1;
  iTopCost := 0;

  for i := 0 to Items.Count - 1 do
  begin
    iNewCost := GetTextCost(Criteria, Items[i]);
    {$IFDEF DEBUG}
    OutputDebugString(PChar(Format('Text cost `%s` to `%s` is `%d`.', [Items[i], Criteria, iNewCost])));
    {$ENDIF}
    if iNewCost > iTopCost then
    begin
      iTopCost := iNewCost;
      Result := i;
    end;
  end;
end;

procedure TTimedSearch.Handler(Sender: TObject; var Key: Char);
begin
  KeyPressed(Key);

  Key := #0;
end;

procedure wuShowBalloon(Edit: HWND; Title: string; Text: string;  Icon: TBallonIcon);
var
 ebt: EDITBALLOONTIP;
begin
  ebt.ttiIcon := Ord(Icon);
  ebt.pszTitle := PWideChar(Title);
  ebt.pszText := PWideChar(Text);
  ebt.cbStruct := sizeof(ebt);
  SendMessage(Edit, EM_SHOWBALLOONTIP, 0, LongInt(@ebt));
 end;

procedure wuLoadInternalFont();
var
  res: TResourceStream;
  FontsCount : DWORD;
begin
  res := TResourceStream.CreateFromID(HInstance, 1, RT_FONT);
  InternalFont := AddFontMemResourceEx(res.Memory, res.Size, nil, @FontsCount);
  res.Free();
end;

procedure wuOpenUrl(const Handle: HWND; const URL: string);
begin
  ShellExecute(Handle, 'open', PChar(URL), nil, nil, SW_SHOWDEFAULT);
end;

procedure wuUnloadInternalFont();
begin
  RemoveFontMemResourceEx(InternalFont);
end;
end.

