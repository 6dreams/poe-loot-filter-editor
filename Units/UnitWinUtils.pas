unit UnitWinUtils;

interface uses Windows, Messages, Classes, WinApi.ShellApi;

var
  InternalFont: THandle;
type
  TBallonIcon = (biNone, biInfo, biWarning, biError);
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

