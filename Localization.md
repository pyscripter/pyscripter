&lt;wiki:gadget url="http://pyscripter.googlecode.com/svn/wiki/files/adsense3.xml" border="0" width="468" height="60" /&gt;

# Localization #

PyScripter uses [gettext](http://www.gnu.org/software/gettext/) for creating localizing the user interface. The files that contains the strings be translated is located in the directory:

C:\Program Files\PyScripter\locale
assuming that PyScripter is located at C:\Program Files\PyScripter\.
There are two files of interest to translators in that directory:
  * default.po
  * languages.po

The first contains the strings of PyScripter and the second the localized names of different languages.
To create a translation for a new language:
  1. Create a new directory  C:\Program Files\PyScripter\locale\##\LC\_MESSAGES\
> > In this path, ## represents the two-letter [ISO 639-1](http://www.loc.gov/standards/iso639-2/php/code_list.php) language code.
  1. Copy the two po files in that directory
  1. Translate the two files using a gettext editor. [Poedit](http://www.poedit.net/) is the recommended editor.
  1. Compile the po files to mo files (Poedit can do that) and you are set. Use the View Menu to change the language and test your translation.
  1. Please submit your translation files to pyscripter@gmail.com for inclusion in the next PyScripter distribution.

**Tip:** Use the translation memory of Poedit to speed up the translation process.