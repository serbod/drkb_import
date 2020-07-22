unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FastHTMLParser, RFUtils, lconvencoding, LazUTF8;

type
  TOutputMode = (omText, omMarkdown, omHtml);
  TBlockType = (btUndef, btCode, btIgnore);
  TParaType = (ptUndef, ptCode, ptHeader, ptAuthor, ptSource);
  TSpanType = (stUndef, stMono, stBold, stEmpty);

  { TConverter }

  TConverter = class(TObject)
  private
    FSrcFiles: TStringList;
    FOutFile: TStringList;

    FOutputMode: TOutputMode;
    FFileName: string;
    FCurTag: string;
    FBlockType: TBlockType;
    FSpanType: TSpanType;
    FSpanTypePrev: TSpanType;
    FParaType: TParaType;
    FParaText: string;
    FSpanText: string;
    FCodeText: string;
    FFileTitle: string;
    FFileAuthor: string;
    FFileSource: string;
    FFileDrkb: string;
    FEmptyParaCount: Integer;

    procedure SpanStart(ASpanType: TSpanType);
    procedure SpanEnd(IsParaEnd: Boolean = False);

    procedure OnFoundTagHandler(NoCaseTag, ActualTag: string);
    procedure OnFoundTextHandler(AText: string);

    function ExtractTagParamValue(ATagStr, AParamName: string): string;
    procedure PutCredentials(AClearAfter: Boolean = True);

  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure FillSrcFilesList();
    procedure Start(AOutputMode: TOutputMode);

    procedure ReadIndex();
    function FindFilenameByTitle(AStr: string): string;
    function FindFilenameByDrkbID(AStr: string): string;

    procedure StripFile(AFileName: string);

    property OutputMode: TOutputMode read FOutputMode write FOutputMode;
  end;

  { TWorker }

  TWorker = class(TThread)
  protected
    procedure Execute; override;
  public
    Converter: TConverter;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    btnToHtml: TButton;
    btnToMarkdown: TButton;
    btnToText: TButton;
    btnPasteTitle: TButton;
    edFileName: TEdit;
    edDrkbID: TEdit;
    edTitle: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    memoPageText: TMemo;
    procedure btnIndexMDClick(Sender: TObject);
    procedure btnPasteTitleClick(Sender: TObject);
    procedure btnToHtmlClick(Sender: TObject);
    procedure btnToMarkdownClick(Sender: TObject);
    procedure btnToTextClick(Sender: TObject);
    procedure edDrkbIDEditingDone(Sender: TObject);
    procedure edFileNameEditingDone(Sender: TObject);
    procedure edTitleEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConverter: TConverter;
    FWorker: TWorker;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

procedure HtmlUnicodeToUtf8(var AText: string);
var
  n, m, iLen, iChar: Integer;
  sCode, sChar: string;
  wc: WideChar;
begin
  n := Pos('&#', AText);
  while n > 0 do
  begin
    m := PosEx(';', AText, n);

    iLen := (m-n)-2;
    if iLen > 5 then
    begin
      Assert(False);
      Exit;
    end;
    iChar := StrToIntDef(Copy(AText, n+2, iLen), 0);
    if iChar = 0 then
    begin
      Assert(False);
      Exit;
    end;

    wc := WideChar(iChar);
    sChar := UTF8Encode(wc);
    sCode := Copy(AText, n, iLen + 3);

    AText := StringReplace(AText, sCode, sChar, [rfReplaceAll]);
    n := Pos('&#', AText);
  end;

end;

procedure FindFiles(AFiles: TStrings; ADir, AFileMask: string);
var
  SearchRec: TSearchRec;
  FileName: string;
begin
  // WriteLn('Adding Dir: ', ADir);
  if FindFirst(ADir + AFileMask, faAnyFile or faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) <> 0 then
      begin
        if Pos('.', SearchRec.Name) = 0 then
        begin
          FindFiles(AFiles, IncludeTrailingPathDelimiter(ADir + SearchRec.Name), AFileMask);
        end;
      end
      else
      begin
        FileName := ADir + SearchRec.Name;
        //FileName := ExtractRelativepath(Project.ProjectDir, FileName);
        if AFiles.IndexOf(FileName) = -1 then
          AFiles.Add(FileName);
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

function TrimSpacesRight(s: string): string;
var
  l: SizeInt;
begin
  l := Length(s);
  while (l > 0) and (s[l] = ' ') do
    Dec(l);
  Result := Copy(s, 1, l);
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConverter := TConverter.Create();
  FConverter.ReadIndex();
end;

procedure TFormMain.btnToHtmlClick(Sender: TObject);
begin
  btnToHtml.Enabled := False;
  FConverter.Start(omHtml);
  btnToHtml.Enabled := True;
end;

procedure TFormMain.btnIndexMDClick(Sender: TObject);
begin
  FConverter.ReadIndex();
end;

procedure TFormMain.btnPasteTitleClick(Sender: TObject);
begin
  edTitle.Clear;
  edTitle.PasteFromClipboard();
  edTitleEditingDone(nil);
end;

procedure TFormMain.btnToMarkdownClick(Sender: TObject);
begin
  if Trim(edFileName.Text) <> '' then
  begin
    FConverter.OutputMode := omMarkdown;
    FConverter.StripFile('drkb3/' + Trim(edFileName.Text) + '.htm');
    Exit;
  end;
  FConverter.Start(omMarkdown);
end;

procedure TFormMain.btnToTextClick(Sender: TObject);
begin
  FConverter.Start(omText);
end;

procedure TFormMain.edDrkbIDEditingDone(Sender: TObject);
var
  sFileName: string;
begin
  sFileName := FConverter.FindFilenameByDrkbID(edDrkbID.Text);

  if sFileName = '' then Exit;

  edFileName.Text := Trim(sFileName);
  edFileNameEditingDone(nil);
end;

procedure TFormMain.edFileNameEditingDone(Sender: TObject);
var
  sFileName: string;
begin
  sFileName := 'out/' + Trim(edFileName.Text) + '.md';
  memoPageText.Lines.LoadFromFile(sFileName);
  memoPageText.SelectAll();
  memoPageText.CopyToClipboard();
  memoPageText.SelLength := 0;
end;

procedure TFormMain.edTitleEditingDone(Sender: TObject);
var
  sFileName: string;
begin
  sFileName := FConverter.FindFilenameByTitle(edTitle.Text);
  edDrkbID.Text := '';
  edFileName.Text := '';

  if sFileName = '' then Exit;

  edFileName.Text := Trim(sFileName);
  edFileNameEditingDone(nil);

  {sFileName := 'out/' + Trim(FSrcFiles.Names[n]) + '.md';
  memoPageText.Lines.LoadFromFile(sFileName);
  memoPageText.SelectAll();
  memoPageText.CopyToClipboard();
  memoPageText.SelLength := 0;}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FWorker) then
    FreeAndNil(FWorker);
  FreeAndNil(FConverter);
end;

{ TConverter }

procedure TConverter.SpanStart(ASpanType: TSpanType);
begin
  if (FSpanTypePrev = stMono) and (ASpanType <> stMono) then
  begin
    // Mono -> other
    FParaText := FParaText + sLineBreak + ' ' + FSpanText;
    FSpanText := '';

    // new line
    if (Trim(FParaText) <> '') or (FEmptyParaCount < 2) then
      FOutFile.Add(FParaText);
    if Trim(FParaText) = '' then
      Inc(FEmptyParaCount)
    else
      FEmptyParaCount := 0;
    FParaText := '';
  end
  else
  if (FSpanTypePrev = stBold) and (ASpanType <> stBold) then
  begin
    // Bold -> other
    if (FSpanText <> '') and (FSpanTypePrev = stBold) then
    begin
      if FOutputMode = omText then
        FParaText := FParaText + '**' + FSpanText + '**'
      else
      if FOutputMode = omMarkdown then
        FParaText := FParaText + '''''''' + FSpanText + ''''''''
      else
      if FOutputMode = omHtml then
        FParaText := FParaText + FSpanText;
      FSpanText := '';
    end;
  end;
  FSpanType := ASpanType;
end;

procedure TConverter.SpanEnd(IsParaEnd: Boolean);
begin
  if (FSpanText <> '') and IsParaEnd then
  begin
    case FSpanTypePrev of
      {stBold:
      begin
        if FOutputMode = omText then
          FParaText := FParaText + '**' + FSpanText + '**'
        else
        if FOutputMode = omMarkdown then
          FParaText := FParaText + '''''''' + FSpanText + '''''''';
      end; }

      stMono:
      begin
        FParaText := FParaText + sLineBreak + ' ' + FSpanText;
      end;
    end;
    FSpanText := '';
  end;

  if (FSpanText <> '') then
  begin
    case FSpanType of
      {stMono:
      begin
        FParaText := FParaText + '<code>' + FSpanText + '</code>';
      end;}

      stBold:
      begin
        if FOutputMode = omText then
          FParaText := FParaText + '**' + FSpanText + '**'
        else
        if FOutputMode = omMarkdown then
          FParaText := FParaText + '''''''' + FSpanText + '''''''';
      end;

      else
        FParaText := FParaText + '' + FSpanText + '';
    end;
    FSpanText := '';
  end;

  if FSpanType <> stUndef then
    FSpanTypePrev := FSpanType;
  if IsParaEnd then
    FSpanTypePrev := stUndef;
  FSpanType := stUndef;
end;

procedure TConverter.OnFoundTagHandler(NoCaseTag, ActualTag: string);
begin
  FCurTag := NoCaseTag;
  if NoCaseTag = '</TABLE>' then
  begin
    if FBlockType = btIgnore then
      FBlockType := btUndef;
  end
  else
  if (Pos('<TABLE', NoCaseTag) > 0) and (Pos('BGCOLOR="#EAD7FF"', NoCaseTag) > 0)
  then
  begin
    // navigation table
    FBlockType := btIgnore;
  end
  else
  if (ActualTag = '<table width="100%" border="0" cellpadding="0" cellspacing="0">') then
  begin
    // empty table
    FBlockType := btIgnore;
  end
  else
  if (ActualTag = '<tr align="left">') then
  begin
    FBlockType := btCode;
    FCodeText := '';
  end
  else
  if (ActualTag = '<p style="text-align: center;">') then
  begin
    FParaType := ptHeader;
  end
  else
  if (FBlockType = btUndef) and (FParaType = ptUndef) and (Pos('<span ', ActualTag) = 1) then
  begin
    if (Pos('font-size: 8pt', ActualTag) > 0) then
    begin
      SpanStart(stEmpty);
    end
    else
    if (Pos('Courier New', ActualTag) > 0) then
    begin
      // Mono
      SpanStart(stMono);
    end
    else
    if (Pos('font-weight: bold', ActualTag) > 0) then
    begin
      // Bold
      SpanStart(stBold);
    end;
  end
  else
  if (FBlockType = btUndef) and (FParaType = ptUndef) and (NoCaseTag = '</SPAN>')then
  begin
    SpanEnd();
  end
  else
  if (FBlockType = btCode) and (NoCaseTag = '</P>')then
  begin
    FCodeText := TrimSpacesRight(FCodeText) + sLineBreak;
  end
  else
  if (NoCaseTag = '</P>') then
  begin
    case FParaType of
      ptHeader:
      begin
        if Trim(FParaText) <> '' then
          FOutFile.Add('==== ' + FParaText + ' ====')
        else
          FOutFile.Add('');
      end;

      ptAuthor:
      begin
        FFileAuthor := FFileAuthor + FParaText;
        //FOutFile.Add('Author: ' + FFileAuthor);
      end;

      ptSource:
      begin
        FFileSource := FParaText;
        //FOutFile.Add('Source: ' + FFileSource);
      end;
    else
      //if (FSpanTypePrev <> stEmpty) and (FParaText <> '') then
      //if (FSpanTypePrev <> stEmpty) and (FEmptyParaCount < 2) then

      SpanEnd(True);

      if (Trim(FParaText) <> '') or (FEmptyParaCount < 2) then
        FOutFile.Add(TrimSpacesRight(FParaText));

      if Trim(FParaText) = '' then
        Inc(FEmptyParaCount)
      else
        FEmptyParaCount := 0;
    end;
    FParaType := ptUndef;
    FSpanTypePrev := stUndef;
    FParaText := '';
  end
  else
  if (FBlockType = btCode) and (NoCaseTag = '</TR>') then
  begin
    FBlockType := btUndef;
    //FOutFile.Add('');
    if FOutputMode = omText then
      FOutFile.Add('<code>' + FCodeText + '</code>')
    else
    if FOutputMode = omMarkdown then
    begin
      FOutFile.Add('<syntaxhighlight lang="delphi">');
      FOutFile.Add(FCodeText + '</syntaxhighlight>');  // FCodeText already have last line break
      //FOutFile.Add('</syntaxhighlight>');
    end
    else
    if FOutputMode = omHtml then
      FOutFile.Add('<pre><code class="delphi">' + FCodeText + '</code></pre>');
    FOutFile.Add('');
    FOutFile.Add('');
    FCodeText := '';
  end
  else
  if (Pos('<img ', ActualTag) = 1) then
  begin
    if FOutputMode = omHtml then
      FOutFile.Add(ActualTag)
    else
    if FOutputMode = omMarkdown then
    begin
      FOutFile.Add('');
      FOutFile.Add('[[file:' + ExtractTagParamValue(ActualTag, 'src') + ']]');
      FOutFile.Add('');
    end
    else
    if FOutputMode = omText then
    begin
      FOutFile.Add('');
      FOutFile.Add('[img:' + ExtractTagParamValue(ActualTag, 'src') + ']');
      FOutFile.Add('');
    end;
  end
  else
  if (Pos('<hr', ActualTag) = 1) then
  begin
    PutCredentials();

    if FOutputMode = omHtml then
      FOutFile.Add(ActualTag)
    else
    if FOutputMode = omMarkdown then
    begin
      FOutFile.Add('----');
      FOutFile.Add('');
    end
    else
    if FOutputMode = omText then
    begin
      FOutFile.Add('');
      FOutFile.Add('--------------------------------------------------------------------------------');
      FOutFile.Add('');
    end;
  end;
end;

procedure TConverter.OnFoundTextHandler(AText: string);
begin
  if Copy(AText, 1, 2) = sLineBreak then
    Exit;

  if AText = '&nbsp;' then
    Exit;

  if AText = ' &nbsp; &nbsp; &nbsp; &nbsp;' then
  begin
    FOutFile.Add('');
    Exit;
  end;

  if AText = '&copy;' then
  begin
    FOutFile.Add('');
    //FOutFile.Add('Copyrights:');
    Exit;
  end;

  if (AText = 'Code:') and (Pos('<SPAN STYLE=', FCurTag) = 1) then
  begin
    // Code:
    Exit;
  end;

  AText := StringReplace(AText, '&nbsp;', ' ', [rfReplaceAll]);
  AText := StringReplace(AText, '&amp;', '&', [rfReplaceAll]);
  AText := StringReplace(AText, '&lt;', '<', [rfReplaceAll]);
  AText := StringReplace(AText, '&gt;', '>', [rfReplaceAll]);
  AText := StringReplace(AText, '&#8211;', '-', [rfReplaceAll]);   // EN DASH
  AText := StringReplace(AText, '&#8212;', '-', [rfReplaceAll]);   // EM DASH
  AText := StringReplace(AText, '&#183;', sLinebreak + '* ', [rfReplaceAll]);  // MIDDLE DOT
  {while Pos(' '+sLinebreak, AText) > 0 do
    AText := StringReplace(AText, ' '+sLinebreak, sLinebreak, [rfReplaceAll]);  // пробел перед концом строки }

  //AText := CP1251ToUTF8(AText);
  AText := WinCPToUTF8(AText);

  HtmlUnicodeToUtf8(AText);

  if (Pos('Автор:', AText) in [1..3])
  or (Pos('Author:', AText) in [1..3])
  or (Pos('Tip by ', AText) in [1..3])
  or (Pos('Автор ', AText) in [1..3]) then
  begin
    FParaType := ptAuthor;
    FSpanType := stUndef;
    FParaText := FParaText + AText;
    Exit;
  end;

  if Pos('Взято ', AText) = 1 then
  begin
    FParaType := ptSource;
    FSpanType := stUndef;
    FParaText := '';
    if Length(AText) > 10 then
      FParaText := AText;
    Exit;
  end;

  if Pos('delphiworld.narod', LowerCase(AText)) > 0 then
  begin
    FFileSource := AText;
    FParaText := FParaText + AText;
    Exit;
  end;

  if UpperCase(Copy(AText, 1, 6)) = 'DRKB::' then
  begin
    FFileDrkb := Copy(AText, 7, MaxInt);
    Exit;
  end;

  if FCurTag = '<TITLE>' then
  begin
    FFileTitle := AText;
    //FOutFile.Add('<title>' + AText + '</title>');
  end
  else
  if (FBlockType = btIgnore) then
  begin
    // nothing
  end
  else
  if (FBlockType = btCode) then
  begin
    // code block
    FCodeText := FCodeText + AText;

    if Pos('delphibase.endimus.com', AText) > 0 then
    begin
      FFileSource := 'http://delphibase.endimus.com';
    end;
  end
  else
  begin
    if (FSpanType in [stMono, stBold]) then
      FSpanText := FSpanText + AText
    else
      FParaText := FParaText + AText;
  end;
end;

function TConverter.ExtractTagParamValue(ATagStr, AParamName: string): string;
var
  n1, n2: Integer;
begin
  Result := '';
  // extract src
  n1 := Pos(AParamName + '="', ATagStr);
  if n1 = 0 then
    Exit;
  n1 := n1 + Length(AParamName + '="');
  n2 := PosEx('"', ATagStr, n1);
  Result := Copy(ATagStr, n1, n2-n1);
end;

procedure TConverter.PutCredentials(AClearAfter: Boolean);
begin
  if FOutputMode = omText then
  begin
    FOutFile.Add('Author: ' + FFileAuthor);
    FOutFile.Add('Source: ' + FFileSource);
    FOutFile.Add('ID: ' + FFileDrkb);
  end
  else
  if FOutputMode = omMarkdown then
  begin;
    if FFileAuthor <> '' then
      FOutFile.Add('Author: ' + FFileAuthor + '<br>');
    if FFileSource <> '' then
      FOutFile.Add('Source: ' + FFileSource + '<br>');
    if FFileDrkb <> '' then
      FOutFile.Add('ID: ' + FFileDrkb + '<br>');
  end
  else
  if FOutputMode = omHtml then
  begin
    //FOutFile.Add('<p>Title: ' + FFileTitle + '</p>');
    if FFileAuthor <> '' then
      FOutFile.Add('<p>Author: ' + FFileAuthor + '</p>');
    if FFileSource <> '' then
      FOutFile.Add('<p>Source: ' + FFileSource + '</p>');
    if FFileDrkb <> '' then
      FOutFile.Add('<p>ID: ' + FFileDrkb + '</p>');
  end;

  if AClearAfter then
  begin
    FFileAuthor := '';
    FFileSource := '';
    FFileDrkb := '';
  end;
end;

procedure TConverter.AfterConstruction;
begin
  inherited AfterConstruction;
  FSrcFiles := TStringList.Create();
  FOutFile := TStringList.Create();
end;

procedure TConverter.BeforeDestruction;
begin
  FreeAndNil(FOutFile);
  FreeAndNil(FSrcFiles);
  inherited BeforeDestruction;
end;

procedure TConverter.FillSrcFilesList();
begin
  FSrcFiles.Clear();
  //FindFiles(FSrcFiles, 'test\', '*.htm');
  FindFiles(FSrcFiles, 'drkb3\', '*.htm');
end;

procedure TConverter.Start(AOutputMode: TOutputMode);
var
  i: Integer;
  sFileName: string;
begin
  FOutputMode := AOutputMode;
  FillSrcFilesList();
  for i := 0 to FSrcFiles.Count-1 do
  begin
    sFileName := FSrcFiles[i];
    StripFile(sFileName);
  end;
end;

procedure TConverter.ReadIndex();
var
  sl: TStringList;
  i, ii, iTabs: Integer;
  n1, n2: Integer;
  s, ss, sName, sFileName: string;
begin
  iTabs := 0;
  sName := '';
  sFileName := '';
  FOutFile.Clear();
  FSrcFiles.Clear();
  sl := TStringList.Create();
  try
    sl.LoadFromFile('drkb3\drkb3_full.hhc');
    for i := 0 to sl.Count-1 do
    begin
      ss := sl[i];
      if Pos('<UL>', ss) > 0 then
      begin
        Inc(iTabs);
      end
      else
      if Pos('</UL>', ss) > 0 then
      begin
        Dec(iTabs);
      end
      else
      if Pos('</OBJECT>', ss) > 0 then
      begin
        if (sName <> '') then
        begin
          s := '';
          for ii := 1 to iTabs do
            s := s + '*';

          if (sFileName <> '') then
          begin
            //s := s + ' [[' + sFileName + '|' + sName + ']]';
            s := s + ' [[' + sName + ']]';
          end
          else
            s := s + ' ' + sName;
          s := WinCPToUTF8(s);
          FOutFile.Add(s);
          Assert(sFileName <> '');
          FSrcFiles.Add(sFileName + FSrcFiles.NameValueSeparator + WinCPToUTF8(sName));
        end;
        sName := '';
        sFileName := '';
      end
      else
      if Pos('name="Name"', ss) > 0 then
      begin
        // extract name
        sName := ExtractTagParamValue(ss, 'value');
        {n1 := Pos('value="', ss);
        if n1 = 0 then
          Continue;
        n1 := n1 + Length('value="');
        n2 := PosEx('">', ss, n1);
        sName := Copy(ss, n1, n2-n1);}
      end
      else
      if Pos('name="Local"', ss) > 0 then
      begin
        // extract filename
        sFileName := ExtractTagParamValue(ss, 'value');
        {n1 := Pos('value="', ss);
        if n1 = 0 then
          Continue;
        n1 := n1 + Length('value="');
        n2 := PosEx('">', ss, n1);
        sFileName := Copy(ss, n1, n2-n1); }

        // remove ".htm"
        sFileName := Copy(sFileName, 1, Length(sFileName) - 4);
      end;
    end;
  finally
    sl.Free();
  end;

  FOutFile.SaveToFile('out/_index.md');
end;

function TConverter.FindFilenameByTitle(AStr: string): string;
var
  i, n: Integer;
  sTitle: string;
begin
  Result := '';
  n := -1;
  sTitle := Trim(AStr);
  for i := 0 to FSrcFiles.Count-1 do
  begin
    if Pos(sTitle, FSrcFiles[i]) > 0 then
    begin
      n := i;
      Break;
    end;
  end;

  if n < 0 then Exit;

  Result := Trim(FSrcFiles.Names[n]);
end;

function TConverter.FindFilenameByDrkbID(AStr: string): string;
var
  i, n: Integer;
  s, sName: string;
  sl: TStringList;
begin
  Result := '';
  s := Trim(AStr) + '<br>';
  sl := TStringList.Create();
  for i := 0 to FSrcFiles.Count-1 do
  begin
    sName := Trim(FSrcFiles.Names[i]);
    if sName = '' then
      Continue;
    sl.LoadFromFile('out\' + sName + '.md');
    for n := 0 to sl.Count-1 do
    begin
      if Pos(s, sl[n]) > 0 then
      begin
        Result := sName;
        Exit;
      end;
    end;
  end;
  sl.Free();
end;

procedure TConverter.StripFile(AFileName: string);
var
  ss: string;
  sl: TStringList;
  Parser: THTMLParser;
begin
  FFileName := ExtractFileName(AFileName);
  FOutFile.Clear();
  FBlockType := btUndef;
  FSpanType := stUndef;
  FSpanTypePrev := stUndef;
  FParaType := ptUndef;
  FParaText := '';
  FCodeText := '';
  FFileTitle := '';
  FFileAuthor := '';
  FFileSource := '';
  FFileDrkb := '';
  FSpanText := '';
  FEmptyParaCount := 0;

  sl := TStringList.Create();
  try
    sl.LoadFromFile(AFileName);
    ss := sl.Text;
    Parser := THTMLParser.Create(ss);
    try
      Parser.OnFoundTag := @OnFoundTagHandler;
      Parser.OnFoundText := @OnFoundTextHandler;
      Parser.Exec();
    finally
      Parser.Free();
    end;
  finally
    sl.Free();
  end;

  if FOutputMode = omText then
  begin
    FOutFile.Insert(0, 'Title: ' + FFileTitle);
    PutCredentials();

    FFileName := StringReplace(FFileName, '.htm', '.txt', [rfIgnoreCase]);
  end
  else
  if FOutputMode = omMarkdown then
  begin;
    FOutFile.Insert(0, '=== ' + FFileTitle + ' ===');
    PutCredentials();

    FFileName := StringReplace(FFileName, '.htm', '.md', [rfIgnoreCase]);
  end
  else
  if FOutputMode = omHtml then
  begin
    // HTML
    ss := '<!DOCTYPE html>' + sLineBreak
        + '<html>' + sLineBreak
        + '<head>' + sLineBreak
        + '  <title>' + FFileTitle + '</title>' + sLineBreak
        + '  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />' + sLineBreak
        + '  <link type="text/css" href="default.css" rel="stylesheet" />' + sLineBreak
        + '  <link rel="stylesheet" href="hl_default.css">' + sLineBreak
        + '  <script src="highlight.pack.js"></script>' + sLineBreak
        + '  <script>hljs.initHighlightingOnLoad();</script>' + sLineBreak
        + '</head>' + sLineBreak
        + '<body>' + sLineBreak;
    FOutFile.Insert(0, ss);

    PutCredentials();

    FOutFile.Add('</body>');
    FOutFile.Add('</html>');
  end;


  FOutFile.SaveToFile('out\' + FFileName);
  FOutFile.Clear();
end;

{ TWorker }

procedure TWorker.Execute;
begin
  //
end;

end.

