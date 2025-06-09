unit UnitMain;

interface uses Winapi.Windows, Winapi.Messages, System.SysUtils, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Menus,
  System.ImageList, Vcl.ImgList, System.Classes, Vcl.StdCtrls,
 Math, UnitFilter, UnitSectionEditor, UnitFilterSelector, UnitBlockEditor, UnitAbout;

type
TFilterRef = class
  SectionIndex: integer;
  BlockIndex: integer;

  Section: TFilterSection;
  Block: TFilterBlock;
  constructor Create(const Section: TFilterSection; const Block: TFilterBlock; const SectionId, BlockId: integer); overload;
  constructor Create(const Section: TFilterSection; const SectionId: integer); overload;
  function IsSection(): boolean;
  function IsBlock(): boolean;
end;

TfrmMain = class(TForm)
  mmMain: TMainMenu;
  miFile: TMenuItem;
  miFileLoad: TMenuItem;
  lbFilter: TListBox;
  ilIcons: TImageList;
  pmFilterEditor: TPopupMenu;
  miEdit: TMenuItem;
  pmEditElement: TMenuItem;
  pmEdit: TMenuItem;
  pmAddSection: TMenuItem;
  N1: TMenuItem;
  N2: TMenuItem;
  N3: TMenuItem;
  N4: TMenuItem;
  miAddSection: TMenuItem;
  pmDelete: TMenuItem;
  miDelete: TMenuItem;
  pmMoveUp: TMenuItem;
  pmMoveDown: TMenuItem;
  miMoveUp: TMenuItem;
  miMoveDown: TMenuItem;
  pmCreateBlock: TMenuItem;
  miHelp: TMenuItem;
  miAbout: TMenuItem;
    miFileSave: TMenuItem;
  procedure miFileLoadClick(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure lbFilterMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
  procedure lbFilterDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  procedure pmEditClick(Sender: TObject);
  procedure lbFilterClick(Sender: TObject);
  procedure pmAddSectionClick(Sender: TObject);
  procedure lbFilterDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  procedure lbFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
  procedure pmDeleteClick(Sender: TObject);
  procedure pmMoveUpClick(Sender: TObject);
  procedure pmMoveDownClick(Sender: TObject);
  procedure lbFilterDblClick(Sender: TObject);
  procedure pmCreateBlockClick(Sender: TObject);
  procedure lbFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  procedure lbFilterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  procedure miAboutClick(Sender: TObject);
  procedure miFileSaveClick(Sender: TObject);
private
  DebugEnabled: boolean;
  FilterIsDoubleClicked: boolean;
  Filter: TFilter;
  Actions: TActions;
  procedure LoadConfigs();
  function GetRefByIndex(const Index: integer): TFilterRef;
  procedure FilterUpdateItemSize(const Index: integer);
  procedure FilterExchange(const Source, Destination: integer);
  procedure FilterRebuildIndexes();
  procedure RebuildAllRefList();
  function StrPtr(const ref: TFilterRef): string;
end;

var
  frmMain: TfrmMain;

implementation
{$R *.dfm}
const
  cAppTitle = '%s%s — Path of Exile Loot Filter Editor';
  FilterItemPadding = 4;
  FilterTextPadding = 2;
  dfHeaderFont = 16;
  dfBaseFont = 10;

constructor TFilterRef.Create(const Section: TFilterSection; const SectionId: integer);
begin
  Create(Section, nil, SectionId, -1);
end;

function TFilterRef.IsSection(): boolean;
begin
  Result := Block = nil;
end;

function TFilterRef.IsBlock(): boolean;
begin
  Result := Block <> nil;
end;

constructor TFilterRef.Create(const Section: TFilterSection; const Block: TFilterBlock; const SectionId, BlockId: integer);
begin
  SectionIndex := SectionId;
  BlockIndex := BlockId;

  self.Section := Section;
  self.Block := Block;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  DebugEnabled := FindCmdLineSwitch('debug');
  LoadConfigs();
  lbFilterClick(nil);
end;

{*
  Из-за сломанной механики ListBox, мы храним указатель на структуру в тексте элемента списка. объект добавляется к элементу отдельно, а не сразу из-за чего часть сообщений от системы
   приходит раньше, и не получается получить объект ссылку на фильтр.
*}
function TfrmMain.GetRefByIndex(const Index: integer): TFilterRef;
begin
  Result := TFilterRef(lbFilter.Items.Objects[Index]);
  if Result <> nil then
  begin
    exit;
  end;

  Result := TFilterRef(Pointer(NativeInt(StrToIntDef(lbFilter.Items[Index], 0))));
end;

function TfrmMain.StrPtr(const ref: TFilterRef): string;
begin
  Result := IntToStr(NativeInt(ref));
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
var
  frmAbout: TfrmAbout;
begin
  frmAbout := TfrmAbout.Create(self);
  frmAbout.ShowModal();
  frmAbout.Free();
end;

procedure TfrmMain.miFileLoadClick(Sender: TObject);
var
  selector: TfrmFilterSelector;
  filterName: string;
  filterContent: TStringList;
  section: TFilterSection;
  iSection, iItem: integer;
  ref: TFilterRef;
begin
  selector := TfrmFilterSelector.Create(frmMain);
  filterName := selector.GetFilter();
  selector.Free();

  if filterName = '' then
  begin
    exit;
  end;

  Caption := Format(cAppTitle, [TFilterUtils.GetSimpleName(filterName), '']);

  filterContent := TStringList.Create();
  filterContent.LoadFromFile(filterName);

  if Assigned(Filter) then
  begin
    Filter.Free();
  end;
  Filter := TFilter.Create(filterName, filterContent, Actions);
  filterContent.Free();

  lbFilter.Items.Clear();
  lbFilter.Items.BeginUpdate();
  for iSection := Low(Filter.Sections) to High(Filter.Sections) do
  begin
    section := Filter.Sections[iSection];
    ref := TFilterRef.Create(section, iSection);
    lbFilter.AddItem(StrPtr(ref), ref);

    for iItem := Low(section.Blocks) to High(section.Blocks) do
    begin
      ref := TFilterRef.Create(section, section.Blocks[iItem], iSection, iItem);
      lbFilter.AddItem(StrPtr(ref), ref);
    end;
  end;
  lbFilter.Items.EndUpdate();
end;

procedure TfrmMain.miFileSaveClick(Sender: TObject);
var
  slTemp: TStrings;
begin
  slTemp := Filter.Content();
  slTemp.SaveToFile(Filter.FileName);
  slTemp.Free();
end;

procedure TfrmMain.pmAddSectionClick(Sender: TObject);
var
  frmAdd: TfrmSectionEditor;
  slSection: TStrings;
  i, targetPosition, prevSectionIndex: integer;
  ref: TFilterRef;
  section: TFilterSection;
begin
  frmAdd := TfrmSectionEditor.Create(self);
  slSection := frmAdd.NewSection();
  frmAdd.Free();

  if slSection = nil then
  begin
    exit;
  end;

  targetPosition := -1;
  prevSectionIndex := 0;
  if lbFilter.ItemIndex > -1 then
  begin
    for i := lbFilter.ItemIndex to lbFilter.Items.Count - 1 do
    begin
      ref := GetRefByIndex(i);
      if ref.IsSection() then
      begin
        if i >= lbFilter.ItemIndex then
        begin
          targetPosition := i;
          prevSectionIndex := ref.SectionIndex - 1;
          break;
        end;
      end;
    end;
  end;

  if targetPosition = -1 then
  begin
    targetPosition := lbFilter.Items.Count;
    prevSectionIndex := High(Filter.Sections);
  end;

  OutputDebugString(PChar(Format('List: %d; Selected: %d; SectionInsertIndex: %d', [targetPosition, lbFilter.ItemIndex, prevSectionIndex + 1])));

  section := TFilterSection.Create();
  section.Comment.AddStrings(slSection);
  slSection.Free();

  Filter.Insert(prevSectionIndex + 1, section);

  ref := TFilterRef.Create(section, targetPosition);

  lbFilter.Items.InsertObject(targetPosition, StrPtr(ref), ref);
  FilterRebuildIndexes();
end;

procedure TfrmMain.pmCreateBlockClick(Sender: TObject);
var
  frmAdd: TfrmBlockEditor;
  block: TFilterBlock;
  targetPosition: integer;
  ref, newRef: TFilterRef;
begin
  if lbFilter.ItemIndex = -1 then
  begin
    exit;
  end;

  frmAdd := TfrmBlockEditor.Create(self);
  block := frmAdd.Add(Actions);
  frmAdd.Free();

  if block = nil then
  begin
    exit;
  end;

  targetPosition := lbFilter.ItemIndex;
  ref := GetRefByIndex(targetPosition);

  Filter.Sections[ref.SectionIndex].Insert(ref.BlockIndex + 1, block);
  newRef := TFilterRef.Create(Filter.Sections[ref.SectionIndex], block, ref.SectionIndex, ref.BlockIndex + 1);
  lbFilter.Items.InsertObject(targetPosition + 1, StrPtr(newRef), newRef);
  FilterRebuildIndexes();
end;

procedure TfrmMain.pmDeleteClick(Sender: TObject);
var
  ref: TFilterRef;
  i, iCount, Index: integer;
begin
  Index := lbFilter.ItemIndex;

  if Index = -1 then
  begin
    exit;
  end;

  ref := GetRefByIndex(lbFilter.ItemIndex);

  if ref.IsBlock() then
  begin
    Filter.Sections[ref.SectionIndex].Delete(ref.BlockIndex);
    lbFilter.Items.Delete(lbFilter.ItemIndex);
  end;

  if ref.IsSection() then
  begin
    iCount := Length(ref.Section.Blocks);
    Filter.Delete(ref.SectionIndex);
    // we need remove items after, because ref.Section.Blocks are allready dead.
    lbFilter.Items.BeginUpdate();
    for i := Index + iCount downto Index + 1 do
    begin
      GetRefByIndex(i).Free();
      lbFilter.Items.Delete(i);
    end;
    lbFilter.Items.Delete(Index);
    lbFilter.Items.EndUpdate();
  end;

  ref.Free();
  FilterRebuildIndexes();
end;

procedure TfrmMain.pmEditClick(Sender: TObject);
var
  ref: TFilterRef;
  frmSection: TfrmSectionEditor;
  frmBlock: TfrmBlockEditor;
  newBlock: TFilterBlock;
  newValue: TStrings;
  iPrevCount: integer;
begin
  if lbFilter.ItemIndex = -1 then
  begin
    exit;
  end;

  ref := GetRefByIndex(lbFilter.ItemIndex);
  if ref = nil then
  begin
    exit;
  end;

  if ref.IsBlock() then
  begin
    frmBlock := TfrmBlockEditor.Create(self);
    newBlock := frmBlock.Edit(Actions, ref.Block);
    if newBlock <> nil then
    begin
      ref.Block.Free();
      ref.Block := newBlock;
      OutputDebugString(PChar(newBlock.Comment.Text));
      FilterUpdateItemSize(lbFilter.ItemIndex);
    end;
    frmBlock.Free();
    exit;
  end;

  frmSection := TfrmSectionEditor.Create(self);
  newValue := frmSection.EditSection(ref.Section.Comment);
  if newValue <> nil then
  begin
    iPrevCount := ref.Section.Comment.Count;
    ref.Section.Comment.Assign(newValue);
    if iPrevCount = newValue.Count then
    begin
      lbFilter.Repaint();
    end else begin
      FilterUpdateItemSize(lbFilter.ItemIndex);
    end;
  end;
  frmSection.Free();
end;

procedure TfrmMain.pmMoveDownClick(Sender: TObject);
var
  src: TFilterRef;
begin
  if lbFilter.ItemIndex = -1 then
  begin
    exit;
  end;

  src := GetRefByIndex(lbFilter.ItemIndex);
  if src.IsSection() then
  begin
    FilterExchange(lbFilter.ItemIndex, lbFilter.ItemIndex + Length(src.Section.Blocks) + 1);
  end else begin
    FilterExchange(lbFilter.ItemIndex, lbFilter.ItemIndex + 1);
  end;
end;

procedure TfrmMain.pmMoveUpClick(Sender: TObject);
var
  src, dst: TFilterRef;
begin
  if lbFilter.ItemIndex= -1 then
  begin
    exit;
  end;

  src := GetRefByIndex(lbFilter.ItemIndex);
  if src.IsSection() then
  begin
    dst := GetRefByIndex(lbFilter.ItemIndex - 1);

    FilterExchange(lbFilter.ItemIndex, lbFilter.ItemIndex - 1 - Length(dst.Section.Blocks));
  end else begin
    FilterExchange(lbFilter.ItemIndex, lbFilter.ItemIndex - 1);
  end;
end;

procedure TfrmMain.FilterUpdateItemSize(const Index: integer);
var
  newHeight: integer;
begin
  lbFilterMeasureItem(lbFilter, Index, newHeight);
  SendMessage(lbFilter.Handle, LB_SETITEMHEIGHT, Index, newHeight);
end;

procedure TfrmMain.FilterExchange(const Source, Destination: integer);
var
  src, dst: TFilterRef;
begin
  src := GetRefByIndex(Source);
  if src.IsSection() then
  begin
    dst := GetRefByIndex(Destination);
    if not dst.IsSection() then
    begin
      dst := GetRefByIndex(Destination - (dst.BlockIndex + 1));
    end;

    Filter.MoveSection(src.SectionIndex, dst.SectionIndex);

    RebuildAllRefList();
    exit;
  end;

  if src.IsBlock() then
  begin
    dst := GetRefByIndex(Destination);

    Filter.MoveBlock(src.SectionIndex, src.BlockIndex, dst.SectionIndex, Max(dst.BlockIndex, 0));

    lbFilter.Items.Delete(Source);
    lbFilter.Items.InsertObject(Destination + Ord(dst.IsSection()), StrPtr(src), src);
  end;

  FilterRebuildIndexes();
end;

procedure TfrmMain.RebuildAllRefList();
var
  b, i, s: integer;
  ref: TFilterRef;
begin
  i := 0;
  lbFilter.Items.BeginUpdate();
  for s := Low(Filter.Sections) to High(Filter.Sections) do
  begin
    ref := GetRefByIndex(i);
    ref.Free();

    ref := TFilterRef.Create(Filter.Sections[s], s);
    lbFilter.Items.Objects[i] := ref;
    lbFilter.Items[i] := StrPtr(ref);

    Inc(i);
    for b := Low(Filter.Sections[s].Blocks) to High(Filter.Sections[s].Blocks) do
    begin
      ref := GetRefByIndex(i);
      ref.Free();

      ref := TFilterRef.Create(Filter.Sections[s], Filter.Sections[s].Blocks[b], s, b);
      lbFilter.Items.Objects[i] := ref;
      lbFilter.Items[i] := StrPtr(ref);

      Inc(i);
    end;
  end;
  lbFilter.Items.EndUpdate();
end;

procedure TfrmMain.FilterRebuildIndexes();
var
  Index, iSection, iBlock: integer;
  ref: TFilterRef;
begin
  Index := 0;
  for iSection := Low(Filter.Sections) to High(Filter.Sections) do
  begin
    ref := GetRefByIndex(Index);
    ref.SectionIndex := iSection;
    ref.BlockIndex := -1;

    Inc(Index);
    for iBlock := Low(Filter.Sections[iSection].Blocks) to High(Filter.Sections[iSection].Blocks) do
    begin
      ref := GetRefByIndex(Index);

      ref.SectionIndex := iSection;
      ref.BlockIndex := iBlock;

      Inc(Index);
    end;
  end;
end;

procedure TfrmMain.lbFilterClick(Sender: TObject);
var
  hasSelection, hasEnoughElems: boolean;
  ref: TFilterRef;
begin
  hasSelection := lbFilter.ItemIndex <> -1;

  pmEdit.Enabled := hasSelection;
  pmEditElement.Enabled := hasSelection;

  pmDelete.Enabled := hasSelection;
  miDelete.Enabled := hasSelection;

  pmCreateBlock.Enabled := hasSelection;

  if hasSelection then
  begin
    ref := GetRefByIndex(lbFilter.ItemIndex);

    if ref.IsSection() then
    begin
      hasEnoughElems := Length(Filter.Sections) > 1;
      pmMoveUp.Enabled := hasEnoughElems and (ref.SectionIndex > 0);
      pmMoveDown.Enabled := hasEnoughElems and (ref.SectionIndex < High(Filter.Sections));
    end else begin
      hasEnoughElems := Length(ref.Section.Blocks) > 1;
      pmMoveUp.Enabled := hasEnoughElems and (ref.BlockIndex > 0);
      pmMoveDown.Enabled := hasEnoughElems and (ref.BlockIndex < High(ref.Section.Blocks));
    end;
  end else begin
    pmMoveUp.Enabled := false;
    pmMoveDown.Enabled := false;
  end;

  miMoveUp.Enabled := pmMoveUp.Enabled;
  miMoveDown.Enabled := pmMoveDown.Enabled;
end;

procedure TfrmMain.lbFilterDblClick(Sender: TObject);
begin
  FilterIsDoubleClicked := true;
  pmEditClick(Sender);

end;

procedure TfrmMain.lbFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  FilterExchange(lbFilter.ItemIndex, lbFilter.ItemAtPos(TPoint.Create(X, Y), False));
end;

procedure TfrmMain.lbFilterDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Index: integer;
  src, dst: TFilterRef;
begin
  if Source <> lbFilter then
  begin
    exit;
  end;

  Index := lbFilter.ItemAtPos(TPoint.Create(X, Y), False);
  // сам в себ¤ нельзя
  Accept := (Index < lbFilter.Items.Count) and (Index > -1) and (Index <> lbFilter.ItemIndex);

  if Accept then
  begin
    src := GetRefByIndex(lbFilter.ItemIndex);
    dst := GetRefByIndex(Index);

    // не принимаем секцию в саму секцию и её блоки, и блок в секцию в которой он и находится
    if src.IsSection() or (src.IsBlock() and dst.IsSection()) then
    begin
      Accept := src.SectionIndex <> dst.SectionIndex;
    end;
  end;
end;

procedure TfrmMain.lbFilterDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  dots = '...';
var
  ref: TFilterRef;
  c: TCanvas;
  x, y, i, l, a: integer;
  originalX, dotsX: integer;
  icon: TIcon;
  s: string;
  item: TFilterItem;
  defColor: TColor;
begin
  ref := GetRefByIndex(Index);

  c := lbFilter.Canvas;
  c.FillRect(Rect);

  if ref = nil then
  begin
    c.Font.Size := 8;
    c.TextOut(Rect.Left + FilterItemPadding, Rect.Top + FilterItemPadding, Format('no ref or invalid item: %s', [lbFilter.Items[Index]]));
    exit;
  end;

  if DebugEnabled then
  begin
    c.Font.Size := 7;
    s := '';
    if ref.BlockIndex >= 0 then
    begin
      s := Format('; b: %d', [ref.BlockIndex]);
    end;
    s := Format('id: %d; s: %d%s', [Index, ref.SectionIndex, s]);
    c.TextOut(Rect.Right - c.TextWidth(s) - FilterTextPadding, Rect.Top + FilterTextPadding, s);
  end;

  if ref.IsSection() then
  begin
    c.Font.Size := dfHeaderFont;
    x := Rect.Left + FilterItemPadding + FilterTextPadding;
    y := Rect.Top + FilterItemPadding;
    c.TextOut(x, y, ref.Section.Comment[0]);

    y := y + FilterTextPadding + c.TextExtent(ref.Section.Comment[0]).Height;
    c.Font.Size := dfBaseFont;
    for i := 1 to ref.Section.Comment.Count - 1 do
    begin
      c.TextOut(x, y, ref.Section.Comment[i]);
      Inc(y, c.TextExtent(ref.Section.Comment[i]).Height);
    end;
    exit;
  end;

  icon := TIcon.Create();
  ilIcons.GetIcon(Ord(ref.Block.Kind = 'Show'), icon);
  c.Draw(Rect.Left + FilterItemPadding, Rect.Top + FilterItemPadding, icon);
  icon.Free();

  defColor := c.Font.Color;
  if ref.Block.HasMarker(TFilterBlock.mDisabled) then
  begin
    c.Font.Color := clGrayText;
  end;

  c.Font.Size := dfBaseFont;
  c.Font.Style := [fsBold];
  x := Rect.Left + ilIcons.Width + FilterItemPadding * 2 + FilterTextPadding;
  y := Rect.Top + FilterItemPadding;
  for i := 0 to ref.Block.Comment.Count - 1 do
  begin
    c.TextOut(x, y, ref.Block.Comment[i]);
    Inc(y, c.TextExtent(ref.Block.Comment[i]).Height);
  end;
  c.Font.Style := [];

  if ref.Block.Comment.Count > 0 then
  begin
    Inc(y, FilterTextPadding);
  end;

  dotsX := c.TextExtent(dots).Width;

  for i := Low(ref.Block.Items) to High(ref.Block.Items) do
  begin
    item := ref.Block.Items[i];
    s := item.Name + ' ';

    c.TextOut(x, y, s);
    originalX := x;
    Inc(x, c.TextExtent(s).Width);
    for a := 0 to item.Arguments.Count - 1 do
    begin
      s := item.Arguments[a];
      if a < item.Arguments.Count - 1 then
      begin
        if TFilterUtils.IsOperator(s, a) then
        begin
          s := s + ' ';
        end else begin
          s := s + ', ';
        end;
      end;

      l := c.TextExtent(s).Width;
      if x + l + dotsX >= lbFilter.ClientWidth - FilterItemPadding * 2 then
      begin
        c.TextOut(x, y, dots);
        break;
      end;

      c.TextOut(x, y, s);
      Inc(x, l);
    end;
    x := originalX;

    Inc(y, c.TextExtent(s).Height);
  end;

  c.Font.Color := defColor;
end;

procedure TfrmMain.lbFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    pmDeleteClick(Sender);
  end;
end;

procedure TfrmMain.lbFilterMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
const
  testString = 'BigString 1xvj';
var
  ref: TFilterRef;
  lines: integer;
  headerSize, baseSize, boldSize: integer;
begin
  ref := GetRefByIndex(Index);
  if ref = nil then
  begin
    OutputDebugString(PChar(Format('lbFilterMeasureItem(%d | pointer: %s) = skip, no reference', [Index, lbFilter.Items[Index]])));

    lbFilter.Canvas.Font.Size := 8;
    Height :=
      FilterItemPadding * 2 +
      lbFilter.Canvas.TextHeight(testString)
    ;

    exit;
  end;

  lbFilter.Canvas.Font.Size := dfBaseFont;
  baseSize := lbFilter.Canvas.TextExtent(testString).Height;

  lbFilter.Canvas.Font.Style := [fsBold];
  boldSize := lbFilter.Canvas.TextExtent(testString).Height;
  lbFilter.Canvas.Font.Style := [];

  if ref.IsBlock() then
  begin
    lines := ref.Block.Comment.Count;
    Height :=
      FilterItemPadding * 2 +             // top & bottom padding
      lines * boldSize +                  // line per item
      Length(ref.Block.Items) * baseSize  // padding between lines
    ;

    if lines > 0 then
    begin
      Height := Height + FilterTextPadding; // padding between comment & content
    end;

    Height := Max(ilIcons.Height + FilterItemPadding * 2, Height);
    exit;
  end;

  lines := ref.Section.Comment.Count;

  lbFilter.Canvas.Font.Size := dfHeaderFont;
  headerSize := lbFilter.Canvas.TextExtent(testString).Height;

  Height :=
    FilterItemPadding * 2 +                 // top & bottom padding
    headerSize +                            // first line
    (lines - 1) * baseSize +                // other lines except first
    Math.Min(lines, 1) * FilterTextPadding  // padding between lines
  ;
end;

procedure TfrmMain.lbFilterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  i := lbFilter.ItemAtPos(TPoint.Create(X, Y), true);

  if i <> -1 then
  begin
    lbFilter.ItemIndex := i;
  end;
end;

procedure TfrmMain.LoadConfigs();
var
  actionStream: TResourceStream;
  actionFs: TFileStream;
  actionFile: string;
  resActions, fsActions: TActions;
begin
  actionFile := ChangeFileExt(Application.ExeName, '.actions');
  actionStream := TResourceStream.Create(HInstance, 'ACTIONS', RT_RCDATA);

  if not DebugEnabled then
  begin
    Actions := TActions.Create(actionStream);
    actionStream.Free();
    exit;
  end;

  if not FileExists(actionFile) then
  begin
    actionStream.SaveToFile(actionFile);
  end;

  actionFs := TFileStream.Create(actionFile, fmOpenRead);
  resActions := TActions.Create(actionStream);
  fsActions := TActions.Create(actionFs);
  actionFs.Free();
  if resActions.Version > fsActions.Version then
  begin
    Actions := resActions;
    fsActions.Free();
    actionStream.SaveToFile(actionFile);
  end else begin
    Actions := fsActions;
    resActions.Free();
  end;
  actionStream.Free();
end;
end.

