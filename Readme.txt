INSTRUCTIONS FOR COMPILIING PyScripter
======================================

I am using Borland Delphi 2010 to compile PyScripter but it should compile with Delphi 2009.

Use TortoiseSVN to download the code from pyscripter.googlecode.com

To compile the PythonIDE.dpr, you first need to install the following components:            

   - Python for Delphi (http://python4delphi.googlecode.com/ - use SVN)

   - JCL and JVCL (jvcl.sf.net)

   - Toolbar2000 (http://www.innosetup.com/tb2k.php)
   - SpTBXLib version (Use SVN svn://www.soft-gems.net/mustangpeak/SpTBXLib)
   
   - Install the PyScripterCustom package

   - Unicode version of SynEdit (synedit.sf.net) at http://mh-nexus.de/unisynedit.htm (latest version needed)
   - SynWeb Highlighters at http://flatdev.ovh.org/

   - VirtualTreeView (http://www.delphi-gems.com/) 
   
   - MustangPeak Components: Common Library, EasyListView and VirtualShellTools.  
     Use SVN (svn://www.soft-gems.net/mustangpeak)
     In VirtualShellTools you need to modify the files Addins.inc the "Include" directory to enable SpTBX

If you have managed to do all the above then you should be able to compile PyScripter! :) 
