{-----------------------------------------------------------------------------
 Unit Name: cFileTemplates
 Author:    Kiriakos Vlahos
 Date:      08-Aug-2006
 Purpose:   Data Structures for File Templates
 History:
-----------------------------------------------------------------------------}

unit cFileTemplates;

interface

Uses
  Classes, SysUtils, Contnrs, JvAppStorage;

Type

  TFileTemplate = class(TInterfacedPersistent, IJvAppStorageHandler)
  protected
    // IJvAppStorageHandler implementation
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  public
    Name: WideString;
    Template: WideString;
    Extension: WideString;
    Category: WideString;
    Highlighter : WideString;
    procedure Assign(Source: TPersistent); override;
  end;

  TFileTemplates = class(TObjectList)
    function CreateListItem(Sender: TJvCustomAppStorage; const Path: string;
      Index: Integer): TPersistent;
    procedure  AddPythonTemplate;
    procedure AddHTMLTemplate;
    procedure AddCSSTemplate;
    procedure AddXMLTemplate;
    procedure AddPlainTextTemplate;
    procedure Assign(Source: TFileTemplates);
  end;

var
  FileTemplates : TFileTemplates;

implementation

{ TFileTemplate }

procedure TFileTemplate.Assign(Source: TPersistent);
begin
  if Source is TFileTemplate then begin
    Name := TFileTemplate(Source).Name;
    Template := TFileTemplate(Source).Template;
    Extension := TFileTemplate(Source).Extension;
    Category := TFileTemplate(Source).Category;
    Highlighter := TFileTemplate(Source).Highlighter;
  end else
    inherited;
end;

procedure TFileTemplate.ReadFromAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  SL : TStringList;
begin
  Name := AppStorage.ReadWideString(BasePath+'\Name');
  Highlighter := AppStorage.ReadWideString(BasePath+'\Highlighter');
  Extension := AppStorage.ReadWideString(BasePath+'\Extension');
  Category := AppStorage.ReadWideString(BasePath+'\Category');
  SL := TStringList.Create;
  try
    AppStorage.ReadStringList(BasePath+'\Template', SL);
    Template := UTF8Decode(SL.Text);
  finally
    SL.Free;
  end;
end;

procedure TFileTemplate.WriteToAppStorage(AppStorage: TJvCustomAppStorage;
  const BasePath: string);
Var
  SL : TStringList;
begin
  AppStorage.WriteWideString(BasePath+'\Name', Name);
  AppStorage.WriteWideString(BasePath+'\Highlighter', Highlighter);
  AppStorage.WriteWideString(BasePath+'\Extension', Extension);
  AppStorage.WriteWideString(BasePath+'\Category', Category);
  SL := TStringList.Create;
  try
    SL.Text := UTF8Encode(Template);
    AppStorage.WriteStringList(BasePath+'\Template', SL);
  finally
    SL.Free;
  end;
end;


{ TFileTemplates }

procedure TFileTemplates.AddCSSTemplate;
Var
  FileTemplate : TFileTemplate;
begin
  FileTemplate := TFileTemplate.Create;
  FileTemplate.Name := 'Cascading Style Sheet';
  FileTemplate.Extension := 'css';
  FileTemplate.Category := 'Internet';
  FileTemplate.Highlighter := 'Cascading Style Sheet';
  FileTemplate.Template :=
    'BODY {' + sLineBreak +
    '' + sLineBreak +
    '}';
  Add(FileTemplate);
end;

procedure TFileTemplates.AddHTMLTemplate;
Var
  FileTemplate : TFileTemplate;
begin
  FileTemplate := TFileTemplate.Create;
  FileTemplate.Name := 'HTML Document';
  FileTemplate.Extension := 'htm';
  FileTemplate.Category := 'Internet';
  FileTemplate.Highlighter := 'HTML';
  FileTemplate.Template :=
    '<!-- Created: $[DateTime-''DD/MM/YYYY''-DateFormat] by $[UserName] -->' + sLineBreak +
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional/EN">' + sLineBreak +
    '<html>' + sLineBreak +
    '  <head>' + sLineBreak +
    '    <title>Untitled</title>' + sLineBreak +
    '    <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">' + sLineBreak +
    '    <meta name="generator" content="PyScripter">' + sLineBreak +
    '  </head>' + sLineBreak +
    '  <body>' + sLineBreak +
    '' + sLineBreak +
    '  </body>' + sLineBreak +
    '</html>';
  Add(FileTemplate);
end;

procedure TFileTemplates.AddPlainTextTemplate;
Var
  FileTemplate : TFileTemplate;
begin
  FileTemplate := TFileTemplate.Create;
  FileTemplate.Name := 'Text File';
  FileTemplate.Extension := 'txt';
  FileTemplate.Category := 'Other';
  FileTemplate.Highlighter := '';
  FileTemplate.Template := '';
  Add(FileTemplate);
end;

procedure TFileTemplates.AddPythonTemplate;
Var
  FileTemplate : TFileTemplate;
begin
  FileTemplate := TFileTemplate.Create;
  FileTemplate.Name := 'Python Script';
  FileTemplate.Extension := 'py';
  FileTemplate.Category := 'Python';
  FileTemplate.Highlighter := 'Python';
  FileTemplate.Template :=
    '#-------------------------------------------------------------------------------' + sLineBreak +
    '# Name:        $[ActiveDoc-Name]' + sLineBreak +
    '# Purpose:     ' + sLineBreak +
    '#' + sLineBreak +
    '# Author:      $[UserName]' + sLineBreak +
    '#' + sLineBreak +
    '# Created:     $[DateTime-''DD/MM/YYYY''-DateFormat]' + sLineBreak +
    '# Copyright:   (c) $[UserName] $[DateTime-''YYYY''-DateFormat]' + sLineBreak +
    '# Licence:     <your licence>' + sLineBreak +
    '#-------------------------------------------------------------------------------' + sLineBreak +
    '#!/usr/bin/env python' + sLineBreak +
    '' + sLineBreak +
    'def main():' + sLineBreak +
    '    pass' + sLineBreak +
    '' + sLineBreak +
    'if __name__ == ''__main__'':' + sLineBreak +
    '    main()';

  Add(FileTemplate);
end;

procedure TFileTemplates.AddXMLTemplate;
Var
  FileTemplate : TFileTemplate;
begin
  FileTemplate := TFileTemplate.Create;
  FileTemplate.Name := 'XML Document';
  FileTemplate.Extension := 'xml';
  FileTemplate.Category := 'Internet';
  FileTemplate.Highlighter := 'XML';
  FileTemplate.Template :=
    '<?xml version="1.0" encoding="UTF-8"?>' + sLineBreak;
  Add(FileTemplate);
end;

procedure TFileTemplates.Assign(Source: TFileTemplates);
var
  i : Integer;
  FileTemplate: TFileTemplate;
begin
  Clear;
  for i := 0 to Source.Count - 1 do begin
    FileTemplate := TFileTemplate.Create;
    FileTemplate.Assign(Source[i] as TFileTemplate);
    Add(FileTemplate)
  end;
end;

function TFileTemplates.CreateListItem(Sender: TJvCustomAppStorage;
  const Path: string; Index: Integer): TPersistent;
begin
  Result := TFileTemplate.Create;
end;

initialization
  FileTemplates := TFileTemplates.Create(True);
  with FileTemplates do begin
   AddPythonTemplate;
   AddHTMLTemplate;
   AddXMLTemplate;
   AddCSSTemplate;
   AddPlainTextTemplate;
  end
finalization
  FileTemplates.Free;
end.
