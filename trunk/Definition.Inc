(**************************************************************************)
(*                                                                        *)
(* Module:   'Definition.Inc'          Copyright (c) 1998                 *)
(*                                                                        *)
(* Version: 1.5                        Dr. Dietmar Budelsky               *)
(*                                     budelsky@ibs.bm.eunet.de           *)
(*                                     IBS Schillings GmbH & Co KG        *)
(*                                     Ein Unternehmen der KROHNE-Gruppe  *)
(*                                     Heisenbergstr. 18                  *)
(*                                     50169 Kerpen-Türnich               *)
(*                                     Phone: (49)22 37/97 44-0           *)
(*                                                                        *)
(**************************************************************************)
(*  Changes for Delphi 4 or higher are made for Compilers higher          *)
(*  than Version 10.0.                                                     *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free, as long as this  *)
(* header and its copyright text is intact.                               *)
(* Dr. Dietmar Budelsky, 1998-01-07                                       *)
(**************************************************************************)

/////////////////////////////////////////////////////////////////////////////
// Select a Python version by commenting out the corresponding line, or
// modify the default Python version below, in the Python versions section.
/////////////////////////////////////////////////////////////////////////////

//{$DEFINE PYTHON15}
//{$DEFINE PYTHON20}
//{$DEFINE PYTHON21}
//{$DEFINE PYTHON22}
//{$DEFINE PYTHON23}
//{$DEFINE PYTHON24}
//{$DEFINE PYTHON25}
//{$DEFINE PYTHON26}
//{$DEFINE PYTHON30}
//{$DEFINE PYTHON31}

/////////////////////////////////////////////////////////////////////////////
// Python for Delphi settings. Comment out the setting you want to enable.
/////////////////////////////////////////////////////////////////////////////

{$DEFINE PREFER_UNICODE} // this will convert a variant containing an OleStr to a UniCode Python string.

/////////////////////////////////////////////////////////////////////////////
// OS symbols. Note that MSWINDOWS is defined from Delphi6/Kylix.
/////////////////////////////////////////////////////////////////////////////

{$IFNDEF MSWINDOWS}
  {$IFDEF WIN32}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Delphi, C++ Builder and Kylix versions
/////////////////////////////////////////////////////////////////////////////

{$IFDEF VER90} // Delphi 2.0
  {$DEFINE DELPHI2}
{$ENDIF}
{$IFDEF VER93} // C++ Builder 1.0
  {$DEFINE CBUILDER1}
{$ENDIF}
{$IFDEF VER100} // Delphi 3.0
  {$DEFINE DELPHI3}
{$ENDIF}
{$IFDEF VER110} // C++ Builder 3.0
  {$DEFINE CBUILDER3}
  {$DEFINE DELPHI3_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER120} // Delphi 4.0
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI4_OR_HIGHER}
{$ENDIF}
{$IFDEF VER125} // C++ Builder 4.0
  {$DEFINE CBUILDER4}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130} // Delphi 5.0
  {$DEFINE DELPHI5}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$IFDEF BCB}  { Borland C++Builder 5.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
{$IFDEF VER140} // Delphi 6.0
  {$DEFINE DELPHI6}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$IFDEF BCB}  { Borland C++Builder 6.0 }
    {$DEFINE CBUILDER3}
    {$DEFINE CBUILDER4}
    {$DEFINE CBUILDER5}
    {$DEFINE CBUILDER6}
    {$ObjExportAll On}
  {$ENDIF} 
{$ENDIF}
{$IFDEF VER150} // Delphi 7.0
  {$DEFINE DELPHI7}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
{$ENDIF}
{$IFDEF VER160} // Delphi 8
  {$DEFINE DELPHI8}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
{$ENDIF}
{$IFDEF VER170} // Delphi 2005
  {$DEFINE DELPHI2005}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
{$ENDIF}
{$IFDEF VER180} // Delphi 2006 and 2007
  {$DEFINE DELPHI2006}
  {$DEFINE DELPHI4_OR_HIGHER}
  {$DEFINE DELPHI5_OR_HIGHER}
  {$DEFINE DELPHI6_OR_HIGHER}
  {$DEFINE DELPHI7_OR_HIGHER}
  {$DEFINE DELPHI8_OR_HIGHER}
  {$DEFINE DELPHI2005_OR_HIGHER}
  {$DEFINE DELPHI2006_OR_HIGHER}
  {$IFDEF VER185} // Delphi 2007
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2007_OR_HIGHER}
  {$ENDIF}
{$ENDIF}


/////////////////////////////////////////////////////////////////////////////
// Python versions
/////////////////////////////////////////////////////////////////////////////

// Here we select a default Python version, if no version was explicitely specified.
// Note that the installer will let the user specify its default Python version, and
// thus will edit this file.

{$IFNDEF PYTHON31}
  {$IFNDEF PYTHON30}
    {$IFNDEF PYTHON26}
      {$IFNDEF PYTHON25}
        {$IFNDEF PYTHON24}
          {$IFNDEF PYTHON23}
            {$IFNDEF PYTHON22}
              {$IFNDEF PYTHON21}
                {$IFNDEF PYTHON20}
                   {$IFNDEF PYTHON15}
    {---<START OF DEFAULT PYTHON VERSION>---}
        {$DEFINE PYTHON25}
    {---<END OF DEFAULT PYTHON VERSION>---}
                   {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF PYTHON20}
  {$DEFINE PYTHON20_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON21}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON22}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON23}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
  {$DEFINE PYTHON23_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON24}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON25}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON26}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON30}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
{$ENDIF}
{$IFDEF PYTHON31}
  {$DEFINE PYTHON20_OR_HIGHER}
  {$DEFINE PYTHON21_OR_HIGHER}
  {$DEFINE PYTHON22_OR_HIGHER}
  {$DEFINE PYTHON23_OR_HIGHER}
  {$DEFINE PYTHON24_OR_HIGHER}
  {$DEFINE PYTHON25_OR_HIGHER}
  {$DEFINE PYTHON26_OR_HIGHER}
  {$DEFINE PYTHON30_OR_HIGHER}
  {$DEFINE PYTHON31_OR_HIGHER}
{$ENDIF}

/////////////////////////////////////////////////////////////////////////////
// Misc
/////////////////////////////////////////////////////////////////////////////

{$IFDEF PYTHON20_OR_HIGHER}
  {$IFDEF DELPHI4_OR_HIGHER}
    {$DEFINE UNICODE_SUPPORT}
  {$ENDIF}
{$ENDIF}

{$IFNDEF UNICODE_SUPPORT}
  {$UNDEF PREFER_UNICODE}
{$ENDIF}

{$IFDEF DELPHI4_OR_HIGHER}
  {$DEFINE HAS_SYNCOBJS_UNIT}
{$ENDIF}

{$IFDEF LINUX}
  {$IFDEF VER140} // Kylix 1 or 2
    // Note that this symbol should be defined only for Kylix2, or Delphi7 and later.
    {$DEFINE HAS_MODIFIED_DISPINVOKE}
  {$ENDIF}
{$ENDIF}
{$IFNDEF HAS_MODIFIED_DISPINVOKE}
  {$IFDEF DELPHI7_OR_HIGHER}
    {$DEFINE HAS_MODIFIED_DISPINVOKE}
  {$ENDIF}
{$ENDIF}
