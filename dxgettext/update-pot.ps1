# This script uses the current version of dxgettext (2.2.2018) compiled from sources.
#
push-location -path ..\Source
..\dxgettext\dxgettext.exe --delphi --no-line-numbers --nonascii --useignorepo --so ..\locale\default.pot
pop-location
.\msgmerge.exe .\defaultheader.po ..\locale\default.pot -o ..\locale\default.pot