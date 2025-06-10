unit UnitFilter;

interface uses System.IOUtils, System.Classes, System.SysUtils, IniFiles, Math, StrUtils, UnitUtils;

{***
  Ожидаемая структура фильтра:
   #$## // configuration opener
   # configuration // application config
   #$## // configuration closer
   #### // section opener
   # section // section name, and content
   #### // section closer
   # comment // comment on following block
   #comment // still comment on following block
   #* inherits:ShowItemGood // some app marker loaded from config
   Show // filter content
       BaseType "Item" // filter content

   # comment // comment on following block
   Hide // filter content
       BaseType "BadItem"

  основные правила:
   * конфигурация бывает только в одном экземпляре
   * маркеров можно быть много на одной строке, их формат: marker_name:marker_value; marker2_name;
   * маркеры применяемы только к блоку
   * в случае, если фильтр не был создан в программе, секция будет одна

***}

const
  caFormat: array[0..1] of string = ('.filter', '.ruthlessfilter');

type
TFilterConfig = class
  // пока пусто
end;

TFilterMarker = class
private
  FName: string;
  FValue: string;
public
  constructor Create(); overload;
  constructor Create(const Name, Value: string); overload;
  property Name: string read FName;
  property Value: string read FValue write FValue;
end;

TArgumentKind = (akInt, akList, akVariadic, akColor, akSockets);
TActionArgumentDefault = record
  Number: integer;
  Text: string;
  HasDefault: boolean;
end;
TActionArgument = class
private
  FName: string;
  FInternalName: string;
  FKind: TArgumentKind;
  FMinimum: integer;
  FMaximum: integer;
  FDefault: TActionArgumentDefault;
  FValues, FValueNames, FDisplayNameCache: TStrings;
public
  constructor Create();
  property Name: string read FName;
  property InternalName: string read FInternalName;
  property Kind: TArgumentKind read FKind;
  property Minimum: integer read FMinimum;
  property Maximum: integer read FMaximum;
  property DefaultValue: TActionArgumentDefault read FDefault;
  property Values: TStrings read FValues;
  property ValueNames: TStrings read FValueNames;
  function GetDisplayNames(): TStrings;
  function GetValueByName(const Name: string): string;
  function GetNameByValue(const Value: string): string;
  function GetDefaultValue(): string;
end;

TActionAguments = array of TActionArgument;
TAction = class
private
  FName: string;
  FText: string;
  FKind: string;
  FRequired: integer;
  FArguments: TActionAguments;
public
  property Name: string read FName;
  property Text: string read FText;
  property Kind: string read FKind;
  property Required: integer read FRequired;
  property Arguments: TActionAguments read FArguments;
end;
TActionArray = array of TAction;
TActions = class(TObject)
private
  FVersion: integer;
  FActions: TActionArray;
public
  constructor Create(Content: TStream);
  property Version: integer read FVersion;
  property Actions: TActionArray read FActions;
  function GetAction(Name: string): TAction;
end;

TFilterItem = class
private
  FAction: TAction;
  FName: string;
  FArguments: TStrings;
public
  constructor Create(Action: TAction; Arguments: TStrings); overload;
  destructor Destroy(); override;
  property Action: TAction read FAction;
  property Name: string read FName;
  property Arguments: TStrings read FArguments;
end;

TFilterItems = array of TFilterItem;
TFilterBlock = class
private
  FType: string;
  FComment: TStrings;
  FMarkers: array of TFilterMarker;
  FItems: TFilterItems;
public
  constructor Create();
  destructor Destroy(); override;
  procedure Delete(const Index: integer);
  procedure Insert(const Index: integer; const Item: TFilterItem);
  function Clone(): TFilterBlock;
  function HasMarker(Name: string): boolean;
  procedure UpdateMarker(const Name, Value: string);
  procedure DeleteMarker(const Name: string);
  property Kind: string read FType write FType;
  property Comment: TStrings read FComment;
  property Items: TFilterItems read FItems;
const
  mDisabled: string = 'disabled';
end;

TFilterBlocks = array of TFilterBlock;
TFilterSection = class
private
  FComment: TStrings;
  FBlocks: TFilterBlocks;
public
  constructor Create();
  property Comment: TStrings read FComment;
  property Blocks: TFilterBlocks read FBlocks;
  procedure Add(Block: TFilterBlock);
  procedure Delete(const Index: integer);
  procedure Remove(const Index: integer);
  procedure Insert(const Index: integer; const Block: TFilterBlock);
end;

TFilterSections = array of TFilterSection;
TFilter = class(TObject)
private
  FFileName: string;
  //FConfig: TFilterConfig;
  FSections: TFilterSections;
  FActions: TActions;
public
  constructor Create(FileName: string; Content: TStrings; Actions: TActions);
  destructor Destroy(); override;
  procedure Parse(Data: TStrings);
  function Content(): TStrings;
  property FileName: string read FFileName;
  property Actions: TActions read FActions;
  property Sections: TFilterSections read FSections;
  procedure Add(Section: TFilterSection);
  procedure Insert(const Index: integer; const Section: TFilterSection);
  procedure Delete(const Index: integer);
  procedure MoveBlock(const SourceSection, Block, DstSection, DstBlock: integer);
  procedure MoveSection(const Source, Destination: integer);
end;

type TFilterUtils = class
public
  class function IsFilter(Name: string): boolean; stdcall; static;
  class function GetSimpleName(Name: string): string; stdcall; static;
  class function GetFileName(SimpleName: string): string; stdcall; static;
  class function IsOperator(const S: string; const Index: integer): boolean; stdcall; static;
end;

implementation
type
TParsedItem = record
  Base: string;
  Arguments: TStrings;
end;

const
  cfPrefConfig: string = '#$##';
  cfPrefSection: string = '####';
  cfPrefComment: string = '#';
  cfPrefMarker: string = '#* ';
  cfBlockShow: string  = 'Show';
  cfBlockHide: string = 'Hide';
  cVersion: string = ':version';

{$REGION 'TFilter class functions'}
destructor TFilterItem.Destroy();
begin
  FArguments.Free();
end;

constructor TFilterItem.Create(Action: TAction; Arguments: TStrings);
var
  p, i, iArgIndex: integer;
  val: string;
begin
  FAction := Action;
  FName := Action.Name;
  FArguments := TStringList.Create();

  iArgIndex := 0;
  for i := Low(Action.Arguments) to High(Action.Arguments) do
  begin
    if iArgIndex > Arguments.Count - 1 then
    begin
      exit;
    end;

    val := Arguments[iArgIndex];
    case Action.Arguments[i].Kind of
      akVariadic, akColor: begin
        for p := iArgIndex to Arguments.Count - 1 do
        begin
          FArguments.Add(Arguments[p]);
        end;
        break;
      end;
      akInt: begin
        if (-2 = StrToIntDef(val, -2)) and Action.Arguments[i].DefaultValue.HasDefault then
        begin
          // we count value as invalid, ex. wrong option, so we skip it for next arg
          val := IntToStr(Action.Arguments[i].FDefault.Number);
        end else begin
          Inc(iArgIndex);
        end;

        FArguments.Add(val);
      end;
      akList: begin
        if Action.Arguments[i].DefaultValue.HasDefault and (Action.Arguments[i].FValues.IndexOf(val) = -1) then
        begin
          val := Action.Arguments[i].DefaultValue.Text;
        end else begin
          Inc(iArgIndex);
        end;

        FArguments.Add(val);
      end;
      else begin
        FArguments.Add(val);
        Inc(iArgIndex);
      end;
    end;
  end;
  Arguments.Free();
end;


constructor TFilter.Create(FileName: string; Content: TStrings; Actions: TActions);
begin
  FFileName := FileName;
  FActions := Actions;
  Parse(Content);
end;

destructor TFilter.Destroy();
var
  section: TFilterSection;
begin
  for section in FSections do
  begin
    section.Free();
  end;

  inherited Destroy();
end;

procedure TFilter.Parse(Data: TStrings);
  function GetActionInfo(const Text: string): TParsedItem;
  var
    items: TArray<string>;
    i: Integer;
  begin
    items := Text.Split([' '], '"', '"', ExcludeEmpty);
    Result.Base := items[0];
    Result.Arguments := TStringList.Create();
    for i := Low(items) + 1 to High(items) do
    begin
      Result.Arguments.Add(TrimString(items[i], '"'));
    end;
  end;

  function ParseMarker(const Text: string): TFilterMarker;
  var
    data: TArray<string>;
  begin
    data := Text.Split([':'], 2);

    if Length(data) = 1 then
    begin
      Result := TFilterMarker.Create(data[0], '');
      exit;
    end;

    Result := TFilterMarker.Create(data[0], data[1]);
  end;

  function GetCommentText(Text: string): string;
  begin
    Result := Trim(TrimString(Text, '#'));
  end;
var
  currentSection: TFilterSection;
  currentBlock: TFilterBlock;

  procedure EnsureBlockExists();
  begin
    if currentSection = nil then
    begin
      currentSection := TFilterSection.Create();
      currentSection.FComment.Add('General Section');
      Add(currentSection);
    end;

    if currentBlock = nil then
    begin
      currentBlock := TFilterBlock.Create();
      currentSection.Add(currentBlock);
    end;
  end;
var
  line, lineTrim, temp: string;
  inSection, inConfig, inDisabledBlock: boolean;
  collectedComments: TStringList;
  collectedMarkers: TStringList;
  actionData: TParsedItem;
  i: integer;
  action: TAction;
begin
  inSection := false;
  inConfig := false;
  inDisabledBlock := false;
  currentSection := nil;
  currentBlock := nil;
  collectedComments := TStringList.Create();
  collectedMarkers := TStringList.Create();
  for line in Data do
  begin
    lineTrim := Trim(line);

    // пропускаем пустую строку
    if lineTrim = '' then
    begin
      if inDisabledBlock then
      begin
        inDisabledBlock := false;
      end;
      continue;
    end;

    if inConfig then
    begin
      if lineTrim.StartsWith(cfPrefConfig) then
      begin
        inConfig := false;
        continue;
      end;

      // todo: add parsing config.
      continue;
    end;

    if inSection then
    begin
      if lineTrim.StartsWith(cfPrefSection) then
      begin
        inSection := false;
        continue;
      end;

      temp := GetCommentText(lineTrim);
      if temp = '' then
      begin
        continue;
      end;

      currentSection.FComment.Add(temp);

      continue;
    end;

    if lineTrim.StartsWith(cfPrefConfig) then
    begin
      inConfig := true;
      continue;
    end;

    if lineTrim.StartsWith(cfPrefSection) then
    begin
      inSection := true;
      currentSection := TFilterSection.Create();
      Add(currentSection);

      continue;
    end;

    if lineTrim.StartsWith(cfPrefMarker) then
    begin
      temp := Trim(Copy(lineTrim, Length(cfPrefMarker)));
      collectedMarkers.Add(temp);

      if temp = TFilterBlock.mDisabled then
      begin
        inDisabledBlock := true;
      end;

      continue;
    end;

    if inDisabledBlock then
    begin
      if lineTrim.StartsWith(cfPrefComment) then
      begin
        lineTrim := Copy(lineTrim, Length(cfPrefComment) + 1);
      end else begin
        inDisabledBlock := false;
      end;
    end;

    if lineTrim.StartsWith(cfPrefComment) then
    begin
      temp := GetCommentText(lineTrim);
      if temp = '' then
      begin
        continue;
      end;

      collectedComments.Add(temp);

      continue;
    end;

    if lineTrim.StartsWith(cfBlockShow) or lineTrim.StartsWith(cfBlockHide) then
    begin
      EnsureBlockExists();

      if currentBlock.FType <> '' then
      begin
        currentBlock := TFilterBlock.Create();
        currentSection.Add(currentBlock);
      end;

      SetLength(currentBlock.FMarkers, collectedMarkers.Count);
      for i := 0 to collectedMarkers.Count - 1 do
      begin
        currentBlock.FMarkers[i] := ParseMarker(Trim(collectedMarkers[i]));
      end;
      collectedMarkers.Clear();

      currentBlock.FType := Copy(lineTrim, 0, 4);
      currentBlock.FComment.AddStrings(collectedComments);
      collectedComments.Clear();

      continue;
    end;

    actionData := GetActionInfo(lineTrim);
    action := FActions.GetAction(actionData.Base);
    if action <> nil then
    begin
      SetLength(currentBlock.FItems, Length(currentBlock.FItems) + 1);
      currentBlock.FItems[High(currentBlock.FItems)] := TFilterItem.Create(action, actionData.Arguments);
    end;
  end;

  collectedComments.Free();
  collectedMarkers.Free();

  lineTrim := '';
end;

function TFilter.Content(): TStrings;
  function GetMaxLength(const Items: TStrings): integer;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to Items.Count - 1 do
    begin
      Result := Math.Max(Result, Length(Items[i]));
    end;
  end;

  function BuildArguments(const item: TFilterItem): string;
  var
    i, argRefId: integer;
    sDefault, sFormat: string;
  begin
    Result := '';
    for i := 0 to item.Arguments.Count - 1 do
    begin
      argRefId := i;
      if argRefId > High(item.Action.Arguments) then
      begin
        argRefId := High(item.Action.Arguments);
      end;

      sDefault := item.Action.Arguments[argRefId].GetDefaultValue();
      if sDefault <> item.Arguments[i] then
      begin
        sFormat := '%s %s';
        if item.Action.Arguments[argRefId].Kind = akVariadic then
        begin
          sFormat := '%s "%s"';
        end;

        Result := Format(sFormat, [Result, item.Arguments[i]]);
      end;
    end;

    Result := Trim(Result);
  end;
var
  p, i, z: Integer;
  block: TFilterBlock;
  item: TFilterItem;
  prefix: string;
begin
  Result := TStringList.Create();

  for i := Low(FSections) to High(FSections) do
  begin
    z := GetMaxLength(FSections[i].Comment);
    Result.Add(DupeString(cfPrefComment, z + 4));
    for p := 0 to FSections[i].Comment.Count - 1 do
    begin
      Result.Add(Format(
        '%s %s%s %s',
        [
          cfPrefComment,
          FSections[i].Comment[p],
          DupeString(' ', z - Length(FSections[i].Comment[p])),
          cfPrefComment
        ]
      ));
    end;
    Result.Add(DupeString(cfPrefComment, z + 4));

    for p := Low(FSections[i].Blocks) to High(FSections[i].Blocks) do
    begin
      block := FSections[i].Blocks[p];

      // comment
      for z := 0 to block.Comment.Count - 1 do
      begin
        Result.Add(Format('%s %s', [cfPrefComment, block.Comment[z]]));
      end;

      // markers.
      for z := Low(block.FMarkers) to High(block.FMarkers) do
      begin
        if block.FMarkers[z].Value = '' then
        begin
          Result.Add(Format('%s%s', [cfPrefMarker, block.FMarkers[z].Name]));
        end else begin
          Result.Add(Format('%s%s:%s', [cfPrefMarker, block.FMarkers[z].Name, block.FMarkers[z].Value]));
        end;
      end;

      // disabled item prefix (comment)
      prefix := '';
      if block.HasMarker(TFilterBlock.mDisabled) then
      begin
        prefix := cfPrefComment;
      end;

      Result.Add(Format('%s%s', [prefix, block.Kind]));

      for z := Low(block.Items) to High(block.Items) do
      begin
        item := block.Items[z];

        Result.Add(TrimRight(Format('%s    %s %s', [prefix, item.Name, BuildArguments(item)])));
      end;

      // finisher.
      Result.Add('');
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TActions class functions'}
constructor TActionArgument.Create();
begin
  FMinimum := -1;
  FMaximum := -1;
  FDefault.Number := -1;
end;

function TActionArgument.GetDefaultValue(): string;
begin
  // if argument have no default value, default contains empty string anyway.
  Result := FDefault.Text;

  if FDefault.HasDefault and (FKind = akInt) then
  begin
    Result := IntToStr(FDefault.Number);
  end;
end;

function TActionArgument.GetDisplayNames(): TStrings;
var
  i: integer;
begin
  if FDisplayNameCache <> nil then
  begin
    Result := FDisplayNameCache;
    exit;
  end;

  FDisplayNameCache := TStringList.Create();

  if FValueNames = nil then
  begin
    FDisplayNameCache.AddStrings(FValues);
  end else begin
    // we can't use duplicates prop, it works only with sorted. we dont want sort.
    for i := 0 to FValueNames.Count - 1 do
    begin
      if FDisplayNameCache.IndexOf(FValueNames[i]) = -1 then
      begin
        FDisplayNameCache.Add(FValueNames[i]);
      end;
    end;
  end;

  Result := FDisplayNameCache;
end;

function TActionArgument.GetValueByName(const Name: string): string;
var
  i: integer;
begin
  if (FValueNames = nil) or (FValues.IndexOf(Name) <> -1) then
  begin
    Result := Name;
    exit;
  end;

  i := FValueNames.IndexOf(Name);
  if i <> -1 then
  begin
    Result := FValues[i];
  end else begin
    Result := FValues[0];
  end;
end;

function TActionArgument.GetNameByValue(const Value: string): string;
var
  i: integer;
begin
  if FValueNames = nil then
  begin
    Result := Value;
    exit;
  end;

  i := FValues.IndexOf(Value);
  if i = -1 then
  begin
    Result := FValueNames[0];
  end else begin
    Result := FValueNames[i];
  end;
end;

constructor TActions.Create(Content: TStream);
var
  argRefs: array of TActionArgument;
  config: TMemIniFile;

  function FindArgument(const Name: string): TActionArgument;
  begin
    for Result in argRefs do
    begin
      if Result.FInternalName = Name then
      begin
        exit;
      end;
    end;

    Result := nil;
  end;

  function LoadArgument(const Name: string): TActionArgument;
    function ParseKind(const Kind: string): TArgumentKind;
    begin
      Result := akInt;
      if Kind = 'color' then Result := akColor;
      if Kind = 'list' then Result := akList;
      if Kind = 'sockets' then Result := akSockets;
      if Kind = 'variadic' then Result := akVariadic;
    end;
  begin
    Result := TActionArgument.Create();

    Result.FInternalName := Name;
    Result.FName := config.ReadString(Name, 'name', '');
    Result.FKind := ParseKind(config.ReadString(Name, 'type', ''));

    if Result.Kind = akInt then
    begin
      Result.FMinimum := config.ReadInteger(Name, 'minimum', -1);
      Result.FMaximum := config.ReadInteger(Name, 'maximum', -1);
      if config.ValueExists(Name, 'default') then
      begin
        Result.FDefault.Number := config.ReadInteger(Name, 'default', -1);
        Result.FDefault.HasDefault := true;
      end;
    end;

    if Result.Kind = akList then
    begin
      Result.FValues := SplitString(config.ReadString(Name, 'values', ''), ',', true);
      Result.FValueNames := SplitString(config.ReadString(Name, 'valueNames', ''), ',', true, true);
      if (Result.FValueNames <> nil) and (Result.FValues.Count <> Result.FValueNames.Count) then
      begin
        Result.FValueNames.Free();
        Result.FValueNames := nil;
      end;
      if config.ValueExists(Name, 'default') then
      begin
        Result.FDefault.HasDefault := true;
        Result.FDefault.Text := Trim(config.ReadString(Name, 'default', ''));
      end;
    end;
  end;
const
  cActions = 'Actions';
var
  slTemp, slActions, slTypeRefs: TStrings;
  actionName, argName: string;
  action: TAction;
  i: integer;
  actionArgument: TActionArgument;
begin
  slTemp := TStringList.Create();
  slTemp.LoadFromStream(Content);
  config := TMemIniFile.Create('');
  config.SetStrings(slTemp);

  FVersion := config.ReadInteger(cActions, cVersion, 0);
  slActions := SplitString(config.ReadString(cActions, 'order', ''), ' ');

  for actionName in slActions do
  begin
    if not config.SectionExists(actionName) then
    begin
      continue;
    end;

    action := TAction.Create();
    action.FName := actionName;
    slTypeRefs := SplitString(config.ReadString(actionName, 'arguments', ''), ' ', false);
    for argName in slTypeRefs do
    begin
      if not config.SectionExists(argName) then
      begin
        action.Free();
        slTypeRefs.Free();
        action := nil;
        break;
      end;

      actionArgument := FindArgument(argName);
      if actionArgument = nil then
      begin
        actionArgument := LoadArgument(argName);
        SetLength(argRefs, Length(argRefs) + 1);
        argRefs[High(argRefs)] := actionArgument;
      end;

      SetLength(action.FArguments, Length(action.FArguments) + 1);
      action.FArguments[High(action.FArguments)] := actionArgument;
    end;

    if action = nil then
    begin
      slTypeRefs.Free();
      continue;
    end;

    action.FRequired := config.ReadInteger(actionName, 'required', 0);
    action.FKind := config.ReadString(actionName, 'kind', '');
    action.FText := config.ReadString(actionName, 'text', '');
    if action.FText = '' then
    begin
      action.FText := Format('Configure "%s" .text field.', [actionName]);

      for i := 0 to slTypeRefs.Count - 1 do
      begin
        action.FText := action.FText + Format(' %s=<a id="%s">{%d|%s}</a>', [slTypeRefs[i], slTypeRefs[i], i, slTypeRefs[i]]);
      end;
    end;

    SetLength(FActions, Length(FActions) + 1);
    FActions[High(FActions)] := action;
    slTypeRefs.Free();
  end;

  slActions.Free();
  slTemp.Free();
  config.Free();
end;

function TActions.GetAction(Name: string): TAction;
begin
  for Result in FActions do
  begin
    if Result.Name = Name then
    begin
      exit;
    end;
  end;

  Result := nil;
end;
{$ENDREGION}

{$REGION 'TFilter TFilterSection TFilterBlock DTOs'}
procedure TFilter.Add(Section: TFilterSection);
begin
  SetLength(FSections, Length(FSections) + 1);
  FSections[High(FSections)] := Section;
end;

procedure TFilter.Insert(const Index: integer; const Section: TFilterSection);
var
  i: integer;
begin
  SetLength(FSections, Length(FSections) + 1);
  for i := Length(FSections) - 2 downto Index do
  begin
    FSections[i + 1] := FSections[i];
  end;

  FSections[Index] := Section;
end;

procedure TFilter.Delete(const Index: integer);
var
  i: integer;
begin
  for i := Low(FSections[Index].Blocks) to High(FSections[Index].Blocks) do
  begin
    FSections[Index].Blocks[i].Free();
  end;
  FSections[Index].Free();

  for i := Index to High(FSections) - 1 do
  begin
    FSections[i] := FSections[i + 1];
  end;

  SetLength(FSections, Length(FSections) - 1);
end;

procedure TFilter.MoveBlock(const SourceSection, Block, DstSection, DstBlock: integer);
var
  src: TFilterBlock;
begin
  src := FSections[SourceSection].FBlocks[Block];
  if SourceSection <> DstSection then
  begin
    FSections[SourceSection].Remove(Block);
    FSections[DstSection].Insert(DstBlock, src);
    exit;
  end;

  // мен¤ем объекты местами
  FSections[DstSection].FBlocks[Block] := FSections[DstSection].FBlocks[DstBlock];
  FSections[DstSection].FBlocks[DstBlock] := src;
end;

procedure TFilter.MoveSection(const Source, Destination: integer);
var
  tmp: TFilterSection;
begin
  tmp := FSections[Source];
  FSections[Source] := FSections[Destination];
  FSections[Destination] := tmp;
end;

procedure TFilterSection.Add(Block: TFilterBlock);
begin
  SetLength(FBlocks, Length(FBlocks) + 1);
  FBlocks[High(FBlocks)] := Block;
end;

procedure TFilterSection.Delete(const Index: integer);
begin
  FBlocks[Index].Free();
  Remove(Index);
end;

procedure TFilterSection.Remove(const Index: integer);
var
  i: integer;
begin
  for i := Index to High(FBlocks) - 1 do
  begin
    FBlocks[i] := FBlocks[i + 1];
  end;

  SetLength(FBlocks, Length(FBlocks) - 1);
end;

procedure TFilterSection.Insert(const Index: integer; const Block: TFilterBlock);
var
  i: integer;
begin
  SetLength(FBlocks, Length(FBlocks) + 1);
  for i := Length(FBlocks) - 2 downto Index do
  begin
    FBlocks[i + 1] := FBlocks[i];
  end;

  FBlocks[Index] := Block;
end;

constructor TFilterBlock.Create();
begin
  FComment := TStringList.Create();
end;

destructor TFilterBlock.Destroy();
var
  i: integer;
begin
  FComment.Free();
  for i := Low(FMarkers) to High(FMarkers) do
  begin
    FMarkers[i].Free();
  end;

  for i := Low(FItems) to High(FItems) do
  begin
    FItems[i].Free();
  end;
end;

procedure TFilterBlock.Delete(const Index: integer);
var
  i: integer;
begin
  FItems[Index].Free();
  for i := Index to High(FItems) - 1 do
  begin
    FItems[i] := FItems[i + 1];
  end;

  SetLength(FItems, Length(FItems) - 1);
end;

procedure TFilterBlock.Insert(const Index: integer; const Item: TFilterItem);
var
  i: integer;
begin
  SetLength(FItems, Length(FItems) + 1);
  for i := Length(FItems) - 2 downto Index do
  begin
    FItems[i + 1] := FItems[i];
  end;
  FItems[Index] := Item;
end;

function TFilterBlock.Clone(): TFilterBlock;
var
  i: integer;
  slTemp: TStringList;
begin
  Result := TFilterBlock.Create();
  Result.FType := FType;
  Result.FComment.AddStrings(FComment);

  SetLength(Result.FMarkers, Length(FMarkers));
  for i := Low(FMarkers) to High(FMarkers) do
  begin
    Result.FMarkers[i] := TFilterMarker.Create(FMarkers[i].Name, FMarkers[i].Value);
  end;

  SetLength(Result.FItems, Length(FItems));
  for i := Low(FItems) to High(FItems) do
  begin
    slTemp := TStringList.Create();
    slTemp.AddStrings(FItems[i].Arguments);
    Result.FItems[i] := TFilterItem.Create(FItems[i].Action, slTemp);
  end;
end;

constructor TFilterSection.Create();
begin
  FComment := TStringList.Create();
end;

function TFilterBlock.HasMarker(Name: string): boolean;
var
  i: integer;
begin
  for i := Low(FMarkers) to High(FMarkers) do
  begin
    if FMarkers[i].FName = Name then
    begin
      Result := True;
      exit;
    end;
  end;

  Result := False;
end;

procedure TFilterBlock.UpdateMarker(const Name, Value: string);
var
  i: Integer;
begin
  for i := Low(FMarkers) to High(FMarkers) do
  begin
    if FMarkers[i].Name = Name then
    begin
      FMarkers[i].Value := Value;
      exit;
    end;
  end;

  i := Length(FMarkers);
  SetLength(FMarkers, i + 1);
  FMarkers[i] := TFilterMarker.Create(Name, Value);
end;

procedure TFilterBlock.DeleteMarker(const Name: string);
var
  delete: boolean;
  i: integer;
begin
  delete := false;
  for i := Low(FMarkers) to High(FMarkers) do
  begin
    if delete then
    begin
      FMarkers[i - 1] := FMarkers[i];
    end;

    if FMarkers[i].FName = Name then
    begin
      delete := true;
      FMarkers[i].Free();
    end;
  end;

  if not delete then
  begin
    exit;
  end;

  SetLength(FMarkers, Length(FMarkers) - 1);
end;

{$ENDREGION}

{$REGION 'TFilter* DTOs'}
constructor TFilterMarker.Create();
begin
end;

constructor TFilterMarker.Create(const Name, Value: string);
begin
  FName := Name;
  FValue := Value;
end;
{$ENDREGION}

{$REGION 'TFilterUtils class functions'}
class function TFilterUtils.IsFilter(Name: string): boolean;
var
  ext: string;
  i: integer;
begin
  ext := TPath.GetExtension(Name);
  Result := false;
  for i := Low(caFormat) to High(caFormat) do
  begin
    if caFormat[i] = ext then
    begin
      Result := true;
      exit;
    end;
  end;
end;

class function TFilterUtils.GetSimpleName(Name: string): string;
begin
  Result := TPath.GetFileName(Name);
end;

class function TFilterUtils.GetFileName(SimpleName: string): string;
begin
  Result := SimpleName;
end;

class function TFilterUtils.IsOperator(const S: string; const Index: integer): boolean;
  const
    aSkip: array[0..3] of string = ('=', '<', '>', '!');
  var
    i: integer;
  begin
    Result := false;
    if Index > 0 then
    begin
      exit;
    end;

    for i := Low(aSkip) to High(aSkip) do
    begin
      if S.Contains(aSkip[i]) then
      begin
        Result := true;
        exit;
      end;
    end;
  end;
{$ENDREGION}
end.

