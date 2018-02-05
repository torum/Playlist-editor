unit uMain;

{$mode objfpc}{$H+}

{$DEFINE Mydebug}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics, Dialogs,
  Menus, ComCtrls, ExtCtrls, StdCtrls, Clipbrd, ActnList, strutils,
  LazUTF8, charencstreams, laz2_XMLRead, laz2_XMLWrite, laz2_DOM,
  UOptions {$ifdef Mydebug},Windows{$endif};


 type
  TPlaylistType = (iTunesTSV, m3u, xspf, new);

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actReplace: TAction;
    actSaveXSPF: TAction;
    actSaveM3u: TAction;
    actSaveiTunesTxt: TAction;
    actOpenXSPF: TAction;
    actOpenM3u: TAction;
    actNewFile: TAction;
    actOpenTxt: TAction;
    ActionList1: TActionList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemSelectFiles: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItemSaveTxt: TMenuItem;
    MenuItemVTVCopySelected: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemOpenTxt: TMenuItem;
    MenuItemOpenM3u: TMenuItem;
    MenuItemOpenXspf: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSaveM3u: TMenuItem;
    MenuItemSaveXspf: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelContents: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    TimerHideStatusbar: TTimer;
    VirtualStringTree1: TVirtualStringTree;
    procedure actNewFileExecute(Sender: TObject);
    procedure actOpenM3uExecute(Sender: TObject);
    procedure actOpenTxtExecute(Sender: TObject);
    procedure actOpenXSPFExecute(Sender: TObject);
    procedure actReplaceExecute(Sender: TObject);
    procedure actSaveM3uExecute(Sender: TObject);
    procedure actSaveiTunesTxtExecute(Sender: TObject);
    procedure actSaveXSPFExecute(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemSelectFilesClick(Sender: TObject);
    procedure MenuItemReplaceClick(Sender: TObject);
    procedure MenuItemOpenXspfClick(Sender: TObject);
    procedure MenuItemSaveTxtClick(Sender: TObject);
    procedure MenuItemVTVCopySelectedClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemOpenTxtClick(Sender: TObject);
    procedure MenuItemOpenM3uClick(Sender: TObject);
    procedure MenuItemSaveM3uClick(Sender: TObject);
    procedure MenuItemSaveXspfClick(Sender: TObject);

    procedure TimerHideStatusbarTimer(Sender: TObject);
    procedure VirtualStringTree1BeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VirtualStringTree1FocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  private
    //fileName:string;
    slMain:TStringlist;
    xDocMain: TXMLDocument;
    currentFormat:TPlaylistType;
    isBusy:boolean;
    isOpened:boolean;// TODO
    //TODO:check if dirty?
    isCloseRequested:boolean;
  public
    procedure CreateNew(sl:TStrings);
  end;

var
  frmMain: TfrmMain;
  frmOptions : TfrmOptions;

implementation

//VirtualTreeView NodeData types
type
  PNodeData = ^TNodeData;
  TNodeData = record
    Column0: integer;
    Column1: String;
    Column2: String;
  end;


{$R *.lfm}

{ TfrmMain }



procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PanelContents.BringToFront;
  slMain:= TStringList.Create;

  ProgressBar1.Height:=6;
  ProgressBar1.BorderWidth:=0;
  ProgressBar1.Visible:=false;

  StatusBar1.Visible:=false;
  //hide StatusBar1
  TimerHideStatusbar.Enabled:=false;
  TimerHideStatusbar.Interval:=5000;


end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  // show nice little welcome form.
  if not Assigned(frmOptions) then begin
    frmOptions := TfrmOptions.Create(self);
    frmOptions.Caption:='Welcome';

    frmOptions.PageControl1.ActivePageIndex:=0;

    if (frmOptions.ShowModal = mrOK) then
    begin
      if Assigned(frmOptions.closeAction) then
      begin
         frmOptions.closeAction.Execute;
      end;
    end;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isCloseRequested:=true;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  slMain.Free;
  if Assigned(xDocMain) then xDocMain.Free;
end;

procedure TfrmMain.MenuItemQuitClick(Sender: TObject);
begin
  Application.Terminate;
end;

// New playlist file
procedure TfrmMain.actNewFileExecute(Sender: TObject);
begin
  MenuItemSelectFilesClick(nil);
end;

procedure TfrmMain.MenuItemSelectFilesClick(Sender: TObject);
var
  i:integer;
  Data: PNodeData;
  tNode: PVirtualNode;
begin
  if isBusy then exit;

  //open
  OpenDialog1.DefaultExt:='*.*';
  OpenDialog1.Filter:='Files|*.*';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist, ofAllowMultiSelect ];
  if OpenDialog1.Execute then
  begin
    if (OpenDialog1.Files.Count > 0)then
    begin

      CreateNew(OpenDialog1.Files);

    end;
  end;
end;

procedure TfrmMain.CreateNew(sl:TStrings);
var
  i:integer;
  Data: PNodeData;
  tNode: PVirtualNode;
begin
  //accepts OpenDialog.files or Dropfiles

  isBusy:=true;
  screen.Cursor:=crHourGlass;

  self.Caption:=' [New] ';

  //init
  slMain.Clear;
  VirtualStringTree1.Clear;
  VirtualStringTree1.DefaultNodeHeight:=36;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  VirtualStringTree1.Header.Columns[1].Text:='Location / File path';

  currentFormat:=new;
  //filename := '';
  Statusbar1.Panels[0].Text:='New';
  Statusbar1.Panels[1].Text:='';

  ProgressBar1.Position := 0;
  ProgressBar1.Max := sl.Count;
  ProgressBar1.Style:=pbstNormal;
  ProgressBar1.Visible:=true;

  for i := 0 to -1 + sl.Count do
  begin
    slMain.Add(sl[i]);

    tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
    Data := VirtualStringTree1.GetNodeData(tNode);
    Data^.Column0 := i+1;
    Data^.Column1 := sl[i];
    Data^.Column2 := '';

    ProgressBar1.Position := i;
    Statusbar1.Panels[1].Text:='Processing... ['+intToStr(i+1)+'/'+intToStr(sl.Count) + ']' ;
    Application.ProcessMessages;
    if (Application.Terminated or isCloseRequested) then exit;

  end;
  Statusbar1.Panels[1].Text:='';
  isBusy:=false;

  //can't do it. so,
  actSaveiTunesTxt.Enabled:=false;

  screen.Cursor:=crDefault;
  StatusBar1.Visible:=false;
  ProgressBar1.Visible:=false;
  ProgressBar1.Position:=0;
end;

// Open playlist
// Open iTunes txt
procedure TfrmMain.actOpenTxtExecute(Sender: TObject);
begin
  MenuItemOpenTxtClick(nil);
end;

procedure TfrmMain.MenuItemOpenTxtClick(Sender: TObject);
var
  slRow:TStringlist;
  line:integer;
  el:TCharEncStream;
  Data: PNodeData;
  tNode: PVirtualNode;
  filename:string;
begin
  //TODO:check if dirty?
  if isBusy then exit;

  OpenDialog1.DefaultExt:='*.txt';
  OpenDialog1.Filter:='Text|*.txt';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist ];
  if OpenDialog1.Execute then
  begin
       if OpenDialog1.Filename = '' then exit;
       if (not FileExists(OpenDialog1.Filename)) then exit;
       filename:=OpenDialog1.Filename;
  end else begin
    exit;
  end;

  isBusy:=true;
  screen.Cursor:=crHourGlass;

  self.Caption:=' [iTunes Text(TSV)] ' + OpenDialog1.Filename;

  //init variables
  slMain.Clear;
  VirtualStringTree1.Clear;
  VirtualStringTree1.DefaultNodeHeight:=36;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  VirtualStringTree1.Header.Columns[1].Text:='Location / File path';

  currentFormat:=iTunesTSV;
  //filename := OpenDialog1.Filename;
  //Edit1.text:='';
  //Edit2.text:='';
  Statusbar1.Panels[0].Text:='iTunes';
  StatusBar1.Visible:=true;

  el:=TCharEncStream.Create;
  slRow:=TStringlist.Create;

  try
    el.LoadFromFile(filename);
    //convert UTF-16 to UTF-8 and "load" to the main list.
    slMain.Text:=el.UTF8Text;

    slRow.StrictDelimiter := true;
    slRow.Delimiter := #$9; //TAB
    slRow.QuoteChar := '"';

    ProgressBar1.Position := 0;
    ProgressBar1.Max := slMain.Count;
    ProgressBar1.Style:=pbstNormal;
    ProgressBar1.Visible:=true;

    if (slMain.Count > 0) then begin

      //for each tab delimited line
      for line := 1 to -1 + slMain.Count do
      begin
        slRow.DelimitedText := slMain[line];

        if (slRow.Count = 31) then begin
          //add to treeview
          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := line;
          Data^.Column1 := slRow[30];
          Data^.Column2 := '';
        end else begin
            showmessage('Wrong column number. invalid text!');
            Statusbar1.Panels[1].Text:='Wrong column number. invalid text!';
            screen.Cursor:=crDefault;
            //TODO raise exception
            exit;
        end;

        ProgressBar1.Position := line;
        Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line)+'/'+intToStr(slMain.Count-1) + ']' ;
        Application.ProcessMessages;
        if (Application.Terminated or isCloseRequested) then exit;
      end;

      Statusbar1.Panels[1].Text:='';
    end else begin
      showmessage('Empty & invalid text!');
      Statusbar1.Panels[1].Text:='Empty & invalid text!';
      //todo raise exception
      isBusy:=false;
      screen.Cursor:=crDefault;
      exit;
    end;
  finally
    isBusy:=false;
    slRow.Free;
    el.Free;
    screen.Cursor:=crDefault;

    //iTunes to iTunes is ok.
    actSaveiTunesTxt.Enabled:=true;

    ProgressBar1.Visible:=false;
    ProgressBar1.Position:=0;
    StatusBar1.Visible:=false;
  end;

end;

// Open M3U
procedure TfrmMain.actOpenM3uExecute(Sender: TObject);
begin
  MenuItemOpenM3uClick(nil);
end;

procedure TfrmMain.MenuItemOpenM3uClick(Sender: TObject);
var
  line,i:integer;
  el:TCharEncStream;
  Data: PNodeData;
  tNode: PVirtualNode;
  filename:string;
begin
  if isBusy then exit;

  //open m3u
  OpenDialog1.DefaultExt:='*.m3u';
  OpenDialog1.Filter:='M3U|*.m3u;*.m3u8';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist ];
  if OpenDialog1.Execute then
  begin
       if OpenDialog1.Filename = '' then exit;
       if not (FileExists(OpenDialog1.Filename)) then exit;
  end else begin
    exit;
  end;

  isBusy:=true;
  screen.Cursor:=crHourGlass;

  self.Caption:=' [M3U] ' + OpenDialog1.Filename;

  //init
  slMain.Clear;
  VirtualStringTree1.Clear;
  VirtualStringTree1.DefaultNodeHeight:=36;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  VirtualStringTree1.Header.Columns[1].Text:='Location / File path';

  currentFormat:=m3u;
  filename := OpenDialog1.Filename;
  //Edit1.text:='';
  //Edit2.text:='';
  Statusbar1.Panels[0].Text:='M3U';
  Statusbar1.Panels[1].Text:='';

  el:=TCharEncStream.Create;

  try

    el.LoadFromFile(filename);

    slMain.Text:=el.UTF8Text;


    ProgressBar1.Position := 0;
    ProgressBar1.Max := slMain.Count;
    ProgressBar1.Style:=pbstNormal;
    ProgressBar1.Visible:=true;

    line:=0;
    for i := 0 to -1 + slMain.Count do
    begin

      if (not AnsiStartsStr('#', slMain.Strings[i])) and
         (not (slMain.Strings[i] = #13#10)) and
         (not (slMain.Strings[i] = '')) then
      begin

        line:=line+1;
        tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
        Data := VirtualStringTree1.GetNodeData(tNode);
        Data^.Column0 := line;
        Data^.Column1 := slMain[i];
        Data^.Column2 := '';

        ProgressBar1.Position := i;
        Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line)+'/'+intToStr(slMain.Count) + ']' ;
        Application.ProcessMessages;
        if (Application.Terminated or isCloseRequested) then exit;
      end;
    end;
    Statusbar1.Panels[1].Text:='';
  finally
    isBusy:=false;
    el.Free;

    //can't do it. so,
    actSaveiTunesTxt.Enabled:=false;

    screen.Cursor:=crDefault;
    StatusBar1.Visible:=false;
    ProgressBar1.Visible:=false;
    ProgressBar1.Position:=0;
  end;
end;

// Open XSPF
procedure TfrmMain.actOpenXSPFExecute(Sender: TObject);
begin
  MenuItemOpenXspfClick(nil);
end;

procedure TfrmMain.MenuItemOpenXspfClick(Sender: TObject);
var
  xTrackListNode,xTrackNode: TDOMElement;
  xTracklistNodelist,xTrackNodelist,xLocationNodelist:TDomNodeList;
  Data: PNodeData;
  tNode: PVirtualNode;
  line:integer;
  filename:string;
begin
  if isBusy then exit;

  //open xspf
  OpenDialog1.DefaultExt:='*.xspf';
  OpenDialog1.Filter:='XSPF|*.xspf';
  OpenDialog1.FileName:='';
  OpenDialog1.Files.Clear;
  OpenDialog1.Options:=[ ofFileMustExist ];
  if OpenDialog1.Execute then
  begin
       if OpenDialog1.Filename = '' then exit;
       if not (FileExists(OpenDialog1.Filename)) then exit;
  end else begin
    exit;
  end;

  isBusy:=true;
  screen.Cursor:=crHourGlass;

  self.Caption:=' [XSPF] ' + OpenDialog1.Filename;

  //init
  slMain.Clear;
  VirtualStringTree1.Clear;
  VirtualStringTree1.DefaultNodeHeight:=36;
  VirtualStringTree1.Header.Columns[0].Text:='#';
  VirtualStringTree1.Header.Columns[1].Text:='Location / File path';

  currentFormat:=xspf;
  filename := OpenDialog1.Filename;
  //Edit1.text:='';
  //Edit2.text:='';
  Statusbar1.Panels[0].Text:='XSPF';
  Statusbar1.Panels[1].Text:='';

  try
    if Assigned(xDocMain) then xDocMain.Free;
     ReadXMLFile(xDocMain, filename);

     ProgressBar1.Position := 0;
     //ProgressBar1.Max := //no way to know.


     if Assigned(xDocMain) then
     begin

       if Assigned(xDocMain.DocumentElement) then
       begin
         if (xDocMain.DocumentElement.NodeName = 'playlist') then
         begin
           ProgressBar1.Style:=pbstMarquee;//start bar
           progressBar1.Visible:=true;

           xTracklistNodelist := xDocMain.DocumentElement.GetElementsByTagName('trackList');
           if Assigned(xTracklistNodelist) then
           begin
             if (xTracklistNodelist.Count>0) then
             begin
               xTrackListNode:=xTracklistNodelist.Item[0] as TDOMElement;
               if Assigned(xTrackListNode) then
               begin

                 xTrackNodelist:=xTrackListNode.GetElementsByTagName('track');
                 if Assigned(xTrackNodelist) then
                 begin
                   if (xTrackNodelist.Count>0) then
                   begin
                     xTrackNode:=xTrackNodelist.Item[0] as TDOMElement;
                     line:=0;

                     while Assigned(xTrackNode) do
                     begin

                       xLocationNodelist:=xTrackNode.GetElementsByTagName('location');
                       if Assigned(xLocationNodelist) then
                       begin
                         if (xLocationNodelist.Count>0) then
                         begin

                           line:=line+1;
                           slMain.Add(xLocationNodelist.Item[0].TextContent);

                           tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
                           Data := VirtualStringTree1.GetNodeData(tNode);
                           Data^.Column0 := line;
                           Data^.Column1 := xLocationNodelist.Item[0].TextContent;
                           Data^.Column2 := '';

                           //ProgressBar1.Position := line;
                           Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line) + ']' ;
                           Application.ProcessMessages;
                           if (Application.Terminated or isCloseRequested) then exit;
                         end;
                       end;
                       xTrackNode:= xTrackNode.NextSibling as TDOMElement;
                     end;

                     Statusbar1.Panels[1].Text:='';
                   end;
                 end;
               end;
             end;
           end;

         end;
       end;
     end;


   finally
     isBusy:=false;
     //xDoc.Free;
     screen.Cursor:=crDefault;

    //can't do it. so,
    actSaveiTunesTxt.Enabled:=false;

     Statusbar1.Panels[1].Text:='';
     //stop bar
     ProgressBar1.Visible:=false;
     ProgressBar1.Style:=pbstNormal;
     ProgressBar1.Position:=0;
   end;

end;

// Save playlist
// Save iTunes txt
procedure TfrmMain.actSaveiTunesTxtExecute(Sender: TObject);
begin
  MenuItemSaveTxtClick(nil);
end;

procedure TfrmMain.MenuItemSaveTxtClick(Sender: TObject);
var
  us:TCharEncStream;
begin
  if isBusy then exit;
  //save as txt
  case currentFormat of
    iTunesTSV:begin

      us:=TCharEncStream.Create;
      try
         //save as BOMed UTF-16
        us.HasBOM:=true;
        us.HaveType:=true;
        us.ForceType:=true;
        us.UniStreamType:=ufUtf16le;//ufUtf16be;
        us.UTF8Text:=slMain.Text;
        //us.SaveToFile(filename+'.testUTF16.txt');

        SaveDialog1.DefaultExt:='*.txt';
        SaveDialog1.Filter:='TEXT|*.txt';
        SaveDialog1.FileName:='';
        SaveDialog1.Files.Clear;
        if SaveDialog1.Execute then
        begin
          us.SaveToFile(SaveDialog1.Filename);
          Statusbar1.Panels[1].Text:='Saved as txt: '+SaveDialog1.Filename;
          Statusbar1.Visible:= true;
          TimerHideStatusbar.Enabled:=true;
        end;
      finally
        us.Free;
      end;
    end else begin
      showmessage('"m3u/xspf to iTunes text" not supported! Use m3u.');
      Statusbar1.Panels[1].Text:= '"m3u/xspf to iTunes text" not supported! Use m3u.';
    end;
  end;
end;

// Save M3U
procedure TfrmMain.actSaveM3uExecute(Sender: TObject);
begin
  MenuItemSaveM3uClick(nil);
end;

procedure TfrmMain.MenuItemSaveM3uClick(Sender: TObject);
var
  slFile:TStringList;
  slRow:TStringList;
  i:integer;
begin
  if isBusy then exit;

  //save as m3u
    case currentFormat of

      iTunesTSV:begin

        slFile:=TStringlist.Create;
        slRow:=TStringlist.Create;
        slRow.StrictDelimiter := true;
        slRow.Delimiter := #$9; //TAB
        slRow.QuoteChar := '"';

        for i := 1 to -1 + slMain.Count do
        begin
          slRow.DelimitedText := slMain[i];
          slFile.Add(slRow[30]);
        end;

        try

          SaveDialog1.DefaultExt:='*.m3u';
          SaveDialog1.Filter:='M3U|*.m3u';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
            slFile.SaveToFile(SaveDialog1.Filename);
            Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
            Statusbar1.Visible:= true;
            TimerHideStatusbar.Enabled:=true;
          end;

        finally


          slFile.Free;
          slRow.Free;
        end;
      end;

      m3u:begin
        try
          SaveDialog1.DefaultExt:='*.m3u';
          SaveDialog1.Filter:='M3U|*.m3u';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
            {$ifdef Mydebug}
            OutputDebugString(PChar(TrimRight( 'm3u to m3u SaveDialog1.Filename '+ SaveDialog1.Filename) ));
            {$endif}
             slMain.SaveToFile(SaveDialog1.Filename);
             Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
             Statusbar1.Visible:= true;
             TimerHideStatusbar.Enabled:=true;
          end;

        finally
          {$ifdef Mydebug}
          OutputDebugString(PChar(TrimRight( 'm3u to m3u SaveDialog1.Filename '+ SaveDialog1.Filename) ));
          {$endif}
        end;
      end;

      xspf:begin
        //TODO URI %decoding!!!!!
        try
          SaveDialog1.DefaultExt:='m3u';
          SaveDialog1.Filter:='M3U|*.m3u';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
             slMain.SaveToFile(SaveDialog1.Filename);
             Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
             Statusbar1.Visible:= true;
             TimerHideStatusbar.Enabled:=true;
          end;
        finally

        end;
      end;

      new:begin
        try
          SaveDialog1.DefaultExt:='*.m3u';
          SaveDialog1.Filter:='M3U|*.m3u';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
            {$ifdef Mydebug}
            OutputDebugString(PChar(TrimRight( 'new m3u SaveDialog1.Filename '+ SaveDialog1.Filename) ));
            {$endif}
             slMain.SaveToFile(SaveDialog1.Filename);
             Statusbar1.Panels[1].Text:='Saved as m3u: '+SaveDialog1.Filename;
             Statusbar1.Visible:= true;
             TimerHideStatusbar.Enabled:=true;
          end;

        finally
          {$ifdef Mydebug}
          OutputDebugString(PChar(TrimRight( 'new m3u SaveDialog1.Filename '+ SaveDialog1.Filename) ));
          {$endif}
        end;
      end;
  end;
end;

// Save XSPF
procedure TfrmMain.actSaveXSPFExecute(Sender: TObject);
begin
  MenuItemSaveXspfClick(nil);
end;

procedure TfrmMain.MenuItemSaveXspfClick(Sender: TObject);
var
  xDoc:TXMLDocument;
  xRootNode,xTrackListNode,xTrackNode,xNode:TDomElement;
  slRow:TStringList;
  i:integer;
  xDomText:TDOMText;
begin
  if isBusy then exit;

  //save as xspf
    case currentFormat of
      iTunesTSV:begin
        //create xDoc
        xdoc := TXMLDocument.create;
        xRootNode := xdoc.CreateElement('playlist');
        xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
        xdoc.Appendchild(xRootNode);

        xTrackListNode := xdoc.CreateElement('trackList');
        xRootNode.AppendChild(xTrackListNode);

        slRow:=TStringlist.Create;
        slRow.StrictDelimiter := true;
        slRow.Delimiter := #$9; //TAB
        slRow.QuoteChar := '"';

        for i := 1 to -1 + slMain.Count do
        begin
          slRow.DelimitedText := slMain[i];

          xTrackNode := xdoc.CreateElement('track');
          xNode := xdoc.CreateElement('location');
          //TODO URI %encoding!!!!!
          xDomText:=xDoc.CreateTextNode(slRow[30]);
          xNode.AppendChild(xDomText);
          xTrackNode.AppendChild(xNode);

          xTrackListNode.AppendChild(xTrackNode);
        end;

        try

          SaveDialog1.DefaultExt:='xspf';
          SaveDialog1.Filter:='XSPF|*.xspf';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
           writeXMLFile(xDoc,SaveDialog1.filename);
           Statusbar1.Panels[1].Text:='saved as xspf.';
           Statusbar1.Visible:= true;
           TimerHideStatusbar.Enabled:=true;
          end;

        finally
        end;
      end;

      m3u:begin
        //create xDoc
        xdoc := TXMLDocument.create;
        xRootNode := xdoc.CreateElement('playlist');
        xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
        xdoc.Appendchild(xRootNode);

        xTrackListNode := xdoc.CreateElement('trackList');
        xRootNode.AppendChild(xTrackListNode);


        for i := 1 to -1 + slMain.Count do
        begin

          if (not AnsiStartsStr('#', slMain.Strings[i])) and
             (not (slMain.Strings[i] = #13#10)) and
             (not (slMain.Strings[i] = '')) then
          begin
            xTrackNode := xdoc.CreateElement('track');
            xNode := xdoc.CreateElement('location');
            //TODO URI %encoding!!!!!
            xDomText:=xDoc.CreateTextNode(slMain[i]);
            xNode.AppendChild(xDomText);
            xTrackNode.AppendChild(xNode);

            xTrackListNode.AppendChild(xTrackNode);
          end;
        end;

        try
          SaveDialog1.DefaultExt:='xspf';
          SaveDialog1.Filter:='XSPF|*.xspf';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
           writeXMLFile(xDoc,SaveDialog1.filename);
           Statusbar1.Panels[1].Text:='saved as xspf.';
           Statusbar1.Visible:= true;
           TimerHideStatusbar.Enabled:=true;
          end;
        finally
        end;
      end;

      xspf:begin
        //use existing xDocMain
        try
          SaveDialog1.DefaultExt:='xspf';
          SaveDialog1.Filter:='XSPF|*.xspf';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
           writeXMLFile(xDoc,SaveDialog1.filename);
           Statusbar1.Panels[1].Text:='saved as xspf.';
           Statusbar1.Visible:= true;
           TimerHideStatusbar.Enabled:=true;
          end;
        finally
        end;
      end;

      new:begin
        //create xDoc
        xdoc := TXMLDocument.create;
        xRootNode := xdoc.CreateElement('playlist');
        xRootNode.SetAttribute('xmlns','http://xspf.org/ns/0/');
        xdoc.Appendchild(xRootNode);

        xTrackListNode := xdoc.CreateElement('trackList');
        xRootNode.AppendChild(xTrackListNode);


        for i := 1 to -1 + slMain.Count do
        begin
          if (not (slMain.Strings[i] = '')) then
          begin
            xTrackNode := xdoc.CreateElement('track');
            xNode := xdoc.CreateElement('location');
            //TODO URI %encoding!!!!!
            xDomText:=xDoc.CreateTextNode(slMain[i]);
            xNode.AppendChild(xDomText);
            xTrackNode.AppendChild(xNode);
            xTrackListNode.AppendChild(xTrackNode);
          end;
        end;

        try
          SaveDialog1.DefaultExt:='xspf';
          SaveDialog1.Filter:='XSPF|*.xspf';
          SaveDialog1.FileName:='';
          SaveDialog1.Files.Clear;
          if SaveDialog1.Execute then
          begin
           writeXMLFile(xDoc,SaveDialog1.filename);
           Statusbar1.Panels[1].Text:='saved as xspf.';
           Statusbar1.Visible:= true;
           TimerHideStatusbar.Enabled:=true;
          end;
        finally
        end;

      end;

    end;
end;



// Treeview popup menu
// Treeview popup Copy
procedure TfrmMain.MenuItemVTVCopySelectedClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  //copy
  Node := VirtualStringTree1.FocusedNode;
  if Node = nil then exit;

  Clipboard.AsText:=PNodeData(VirtualStringTree1.GetNodeData(Node))^.Column1;

end;

// Edits
// Replace
procedure TfrmMain.actReplaceExecute(Sender: TObject);
begin
  MenuItemReplaceClick(nil);
end;

procedure TfrmMain.MenuItemReplaceClick(Sender: TObject);
begin
  if isBusy then exit;
  if not Assigned(frmOptions) then begin
    frmOptions := TfrmOptions.Create(self);
  end;

  frmOptions.Caption:='Edit';

  //todo make other invisible too.
  //frmOptions.TabSheetWelcome.Visible:=false;
  frmOptions.TabSheetWelcome.TabVisible:=false;

  //frmOptions.TabSheetReplace.Visible:=true;
  frmOptions.TabSheetReplace.TabVisible:=true;
  frmOptions.PageControl1.ActivePage:=frmOptions.TabSheetReplace;

  if (frmOptions.ShowModal = mrOK) then
  begin
    if Assigned(frmOptions.closeAction) then
    begin
       //frmOptions.closeAction.Execute;
    end;
  end;



  {
  Panel1.BringToFront;
  Edit1.SetFocus;
  if Edit1.Text = '' then begin
    buttonOK.Enabled:=false;
  end else begin
    buttonOK.Enabled:=true;
  end;
  }
end;

procedure TfrmMain.ButtonOKClick(Sender: TObject);
var
  slRow:TStringlist;
  line,i:integer;
  tNode: PVirtualNode;
  Data: PNodeData;
  xTrackListNode,xTrackNode: TDOMElement;
  xTracklistNodelist,xTrackNodelist,xLocationNodelist:TDomNodeList;
begin
  //Replace
  {

  PanelContents.BringToFront;
  VirtualStringTree1.Clear;
  Statusbar1.Panels[1].Text:='';
  StatusBar1.Visible:=true;

  case currentFormat of
    iTunesTSV:begin
        isBusy:=true;

        slRow:=TStringlist.Create;
        slRow.StrictDelimiter := true;
        slRow.Delimiter := #$9; //TAB
        slRow.QuoteChar := '"';

        //for each tab delimited line
        for line := 1 to -1 + slMain.Count do
        begin
          slRow.DelimitedText := slMain[line];

          //replace
          slRow[30]:=StringReplace(slRow[30],Edit1.Text,Edit2.Text,[rfReplaceAll]);

          tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
          Data := VirtualStringTree1.GetNodeData(tNode);
          Data^.Column0 := line;
          Data^.Column1 := slRow[30];
          Data^.Column2 := '';

          slMain[line]:= StringReplace(trim(slRow.text),#13#10,#$9,[rfReplaceAll]);

          statusbar1.Panels[1].Text:='Processing...'+intToStr(line);
          Application.ProcessMessages;
        end;
        statusbar1.Panels[1].Text:='';
        isBusy:=false;
    end;
    m3u:begin
        isBusy:=true;

        line:=0;
        //for each tab delimited line
        for i := 0 to -1 + slMain.Count do
        begin

          if (not AnsiStartsStr('#', slMain.Strings[i])) and
             (not (slMain.Strings[i] = #13#10)) and
             (not (slMain.Strings[i] = '')) then
          begin

            slMain[i]:=StringReplace(slMain[i],Edit1.Text,Edit2.Text,[rfReplaceAll]);

            line:=line+1;
            tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
            Data := VirtualStringTree1.GetNodeData(tNode);
            Data^.Column0 := line;
            Data^.Column1 := slMain[i];
            Data^.Column2 := '';

            statusbar1.Panels[1].Text:='Processing...'+intToStr(line);
            Application.ProcessMessages;
          end else begin
            //#comment or empty line. just leave it as is.
          end;
        end;
        statusbar1.Panels[1].Text:='';
        isBusy:=false;
    end;
    xspf:begin
      isBusy:=true;

      slMain.Clear;

      if Assigned(xDocMain) then
           begin
             if Assigned(xDocMain.DocumentElement) then
             begin
               if (xDocMain.DocumentElement.NodeName = 'playlist') then
               begin

                 xTracklistNodelist := xDocMain.DocumentElement.GetElementsByTagName('trackList');
                 if Assigned(xTracklistNodelist) then
                 begin
                   if (xTracklistNodelist.Count>0) then
                   begin
                     xTrackListNode:=xTracklistNodelist.Item[0] as TDOMElement;
                     if Assigned(xTrackListNode) then
                     begin

                       xTrackNodelist:=xTrackListNode.GetElementsByTagName('track');
                       if Assigned(xTrackNodelist) then
                       begin
                         if (xTrackNodelist.Count>0) then
                         begin
                           xTrackNode:=xTrackNodelist.Item[0] as TDOMElement;
                           line:=0;

                           while Assigned(xTrackNode) do
                           begin

                             xLocationNodelist:=xTrackNode.GetElementsByTagName('location');
                             if Assigned(xLocationNodelist) then
                             begin
                               if (xLocationNodelist.Count>0) then
                               begin

                                 line:=line+1;
                                 //slMain.Add(xLocationNodelist.Item[0].TextContent);
                                 xLocationNodelist.Item[0].TextContent :=StringReplace(xLocationNodelist.Item[0].TextContent,Edit1.Text,Edit2.Text,[rfReplaceAll]);
                                 slMain.Add(xLocationNodelist.Item[0].TextContent);

                                 tNode := VirtualStringTree1.AddChild(VirtualStringTree1.RootNode);
                                 Data := VirtualStringTree1.GetNodeData(tNode);
                                 Data^.Column0 := line;
                                 Data^.Column1 := xLocationNodelist.Item[0].TextContent;
                                 Data^.Column2 := '';

                                 Statusbar1.Panels[1].Text:='Processing... ['+intToStr(line)+'/'+intToStr(slMain.Count) + ']' ;
                                 Application.ProcessMessages;

                               end;
                             end;
                             xTrackNode:= xTrackNode.NextSibling as TDOMElement;
                           end;

                           Statusbar1.Panels[1].Text:='';
                         end;
                       end;
                     end;
                   end;
                 end;

               end;
             end;
           end;

      statusbar1.Panels[1].Text:='';
      isBusy:=false;
    end;

  end;

  StatusBar1.Visible:=false;
  VirtualStringTree1.SetFocus;
  }
end;

procedure TfrmMain.Edit1Change(Sender: TObject);
begin
  {
  if Edit1.Text = '' then begin
    buttonOK.Enabled:=false;
  end else begin
    buttonOK.Enabled:=true;
  end;
  }
end;


procedure TfrmMain.ButtonCancelClick(Sender: TObject);
begin
  {
  VirtualStringTree1.BringToFront;
  VirtualStringTree1.SetFocus;
  Edit1.text:='';
  Edit2.text:='';
  }
end;


procedure TfrmMain.TimerHideStatusbarTimer(Sender: TObject);
begin
  if statusbar1.Visible then begin
    Statusbar1.Visible:=false;
  end;
  TimerHideStatusbar.Enabled:=false;
end;


// Treeview properties
procedure TfrmMain.VirtualStringTree1BeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var
  Data1: PNodeData;
begin
  Data1 := Sender.GetNodeData(Node);
  if (Odd(Data1^.Column0)) then
  begin
    ItemColor := $EBEBEC;// $FFEEEE; ////$DEDEE0;//
    EraseAction := eaColor;
  end;
end;

procedure TfrmMain.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VirtualStringTree1.Refresh;
end;

procedure TfrmMain.VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PNodeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  case Column of
    0: result := Data1^.Column0 - Data2^.Column0;
    1: Result := CompareText(Data1^.Column1, Data2^.Column1);
   end;
end;

procedure TfrmMain.VirtualStringTree1FocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VirtualStringTree1.Refresh;
end;

procedure TfrmMain.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
  var
    Data: PNodeData;
  begin
    Data := VirtualStringTree1.GetNodeData(Node);
    if Assigned(Data) then begin
      Data^.Column0 := 0;
      Data^.Column1 := '';
      Data^.Column2 := '';
    end;
end;

procedure TfrmMain.VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

procedure TfrmMain.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
  var
    Data: PNodeData;
  begin
    Data := VirtualStringTree1.GetNodeData(Node);
    case Column of
      0: CellText := intToStr(Data^.Column0) + '.';
      1: CellText := Data^.Column1;
      2: CellText := Data^.Column2;
    end;

end;

{
function Src2Utf8(const S: string): string;
var
  I: Integer;
  S1: string;
  B: Byte;

begin
  I:= 0;
  Result:= '';
  SetLength(S1, 3);
  S1[1]:= '$';
  while I < Length(S) do begin
    Inc(I);
    if S[I] <> Char('%') then Result:= Result + S[I]
    else begin
      Inc(I);
      S1[2]:= S[I];
      Inc(I);
      S1[3]:= S[I];
      B:= StrToInt(S1);
      Result:= Result + Char(B);
    end;
  end;
end;

}

end.

