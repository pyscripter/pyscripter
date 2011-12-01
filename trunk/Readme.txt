INSTRUCTIONS FOR COMPILIING PyScripter
======================================

I am using Borland Delphi 2010 to compile PyScripter but it should compile with Delphi 2009.

Use TortoiseSVN to download the code from pyscripter.googlecode.com

To compile the PythonIDE.dpr, you first need to install the following components:            

   - Python for Delphi (http://python4delphi.googlecode.com/ - use SVN)

   - JCL (jcl.sf.net) - use SVN

   - JVCL (jvcl.sf.net) - use SVN

   - Toolbar2000 (http://www.innosetup.com/tb2k.php)
   - SpTBXLib (Use SVN svn://www.soft-gems.net/mustangpeak/SpTBXLib)
   
   - Install the PyScripterCustom package from the components subdirectory.

   - SynEdit (synedit.sf.net) - use SVN
   - SynWeb Highlighters at http://flatdev.ovh.org/ - use SVN

   - VirtualTreeView (http://www.delphi-gems.com/) 
   
   - MustangPeak Components: 
     - Common Library (http://mustangpeakcommonlib.googlecode.com/) - use SVN
     - EasyListView (http://mustangpeakcommonlib.googlecode.com/) - use SVN
       Define SpTBX in Options.inc 
     - VirtualShellTools (http://mustangpeakcommonlib.googlecode.com/) - use SVN  
       Define USE_TOOLBAR_TB2K in Addins.inc and immediately below add {$DEFINE SpTBX}



If you have managed to do all the above then you should be able to compile PyScripter! :) 
