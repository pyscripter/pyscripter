unit dlgNewFile;

interface

uses
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  VirtualTrees.Types,
  VirtualTrees.BaseTree,
  VirtualTrees.BaseAncestorVCL,
  VirtualTrees.AncestorVCL,
  VirtualTrees,
  cFileTemplates,
  dlgPyIDEBase;

type
  TNewFileDialog = class(TPyIDEDlgBase)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    tvCategories: TVirtualStringTree;
    Label1: TLabel;
    Panel4: TPanel;
    Label2: TLabel;
    btnCancel: TButton;
    btnCreate: TButton;
    btnManageTemplates: TButton;
    Splitter1: TSplitter;
    lvTemplates: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvCategoriesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormShow(Sender: TObject);
    procedure tvCategoriesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnManageTemplatesClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure lvTemplatesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvTemplatesDblClick(Sender: TObject);
  private
    FCategories: TStringList;
  public
    SelectedTemplate: TFileTemplate;
    procedure SetUp;
  end;

implementation

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Contnrs,
  MPCommonObjects,
  dmCommands;

{$R *.dfm}

procedure TNewFileDialog.btnCreateClick(Sender: TObject);
begin
  if Assigned(lvTemplates.Selected) then begin
    SelectedTemplate := TFileTemplate(lvTemplates.Selected.Data);
    ModalResult := mrOk;
  end;
end;

procedure TNewFileDialog.btnManageTemplatesClick(Sender: TObject);
begin
  CommandsDataModule.actFileTemplatesExecute(Self);
  SetUp;
end;

procedure TNewFileDialog.FormCreate(Sender: TObject);
begin
  inherited;
  FCategories := TStringList.Create;
  FCategories.CaseSensitive := False;
  lvTemplates.LargeImages := LargeSysImages;
end;

procedure TNewFileDialog.FormDestroy(Sender: TObject);
begin
  FCategories.Free;
end;

procedure TNewFileDialog.FormShow(Sender: TObject);
begin
  SetUp;
end;

procedure TNewFileDialog.lvTemplatesDblClick(Sender: TObject);
begin
  btnCreateClick(Self);
end;

procedure TNewFileDialog.lvTemplatesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnCreate.Enabled := Assigned(Item) and Selected;
end;

procedure TNewFileDialog.SetUp;
begin
  FCategories.Clear;
  tvCategories.Clear;
  lvTemplates.Items.Clear;
  for var I  := 0 to FileTemplates.Count - 1 do
    if FCategories.IndexOf(TFileTemplate(FileTemplates[I]).Category) < 0 then
      FCategories.Add(TFileTemplate(FileTemplates[I]).Category);
  tvCategories.RootNodeCount := FCategories.Count;
  if FCategories.Count > 0 then
    tvCategories.Selected[tvCategories.RootNode.FirstChild] := True;
end;

procedure TNewFileDialog.tvCategoriesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  I, Index: Integer;
  FileTemplate: TFileTemplate;
  FName: string;
  FileInfo: TSHFileInfo;
begin
  if Assigned(Node) and (vsSelected in Node.States) then begin
    lvTemplates.Items.Clear;
    Index := Node.Index;
    for I := 0 to FileTemplates.Count - 1 do begin
      FileTemplate := FileTemplates[I] as TFileTemplate;
      if CompareText(FCategories[Index], FileTemplate.Category) = 0 then begin
        with lvTemplates.Items.Add do begin
          Caption := FileTemplate.Name;
          Data := FileTemplate;
          FName := '.' + FileTemplate.Extension;
          if SHGetFileInfo(PChar(FName),
                                   FILE_ATTRIBUTE_NORMAL,
                                   FileInfo,
                                   SizeOf( FileInfo),
                                   SHGFI_USEFILEATTRIBUTES or
                                   SHGFI_LARGEICON or
                                   SHGFI_ICON or
                                   SHGFI_SYSICONINDEX) > 0
          then
            ImageIndex := FileInfo.iIcon
          else
            ImageIndex := 0;
        end;
      end;
    end;
  end;
end;

procedure TNewFileDialog.tvCategoriesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  if TextType = ttNormal then
    CellText := FCategories[Node.Index];
end;

end.
