INSTRUCTIONS FOR COMPILIING PyScripter
======================================

I am using Borland Delphi XE7 to compile PyScripter but it should compile with other recent versions.

Use TortoiseSVN to download the code from pyscripter.googlecode.com

To compile the PythonIDE.dpr, you first need to install the following components:            

   - Python for Delphi (http://python4delphi.googlecode.com/ - use SVN)

   - JCL (jcl.sf.net) - at Github.com or use daily packages from http://jvcl.sourceforge.net/daily/
   - JVCL (jvcl.sf.net) - at Github.com or use daily packages from http://jcl.sourceforge.net/daily/

   - Toolbar2000 (http://www.innosetup.com/tb2k.php)
     *** For now download "TB2K with XE7 support.7z" from 
     https://onedrive.live.com/redir?resid=83A7119830FC7582!2251&authkey=!APLUfnvNQ35oYog&ithint=folder%2cexe ***

   - SpTBXLib (Use SVN from https://code.google.com/p/sptbxlib/)
  
   - Install the PyScripterCustom package from the components subdirectory.

   - SynEdit (synedit.sf.net) - get it from Github.com
   - SynWeb Highlighters - get it from Github.com

   - VirtualTreeView - get it from Github.com
   
   - MustangPeak Components: 
     - Common Library (http://mustangpeakcommonlib.googlecode.com/) - use SVN
     - EasyListView (http://mustangpeakcommonlib.googlecode.com/) - use SVN
       After installing define SpTBX in Options.inc 
     - VirtualShellTools (http://mustangpeakcommonlib.googlecode.com/) - use SVN  
       After installing define USE_TOOLBAR_TB2K in Addins.inc and immediately below add {$DEFINE SpTBX}

   - Vcl Style Utils (Use SVN to get it from vcl-styles-utils.googlecode.com)
     Add the Common and and Common\delphi-detours-library directories to your library path. 	



If you have managed to do all the above then you should be able to compile PyScripter! :) 
