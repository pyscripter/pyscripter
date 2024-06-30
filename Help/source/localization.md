:::{index} Localization
:::

# Localization

PyScripter uses [gettext](http://www.gnu.org/software/gettext/") for creating localizing the 
user interface. The files that contains the strings be translated is located in the directory:
"C:\Program Files\PyScripter\locale" assuming that PyScripter is located at "C:\Program Files\PyScripter\".

There are two files of interest to translators in that directory:
1. default.po
2. languages.po  

The first contains the strings of PyScripter and the second the localized names of different 
languages.  

## Creating a new language locally 

To create a new translation for a language:  

1. Create a new directory "C:\Program Files\PyScripter\locale\\##\LC\_MESSAGES\"
   In this path, ## represents the two-letter 
   [ISO 639-1](http://www.loc.gov/standards/iso639-2/php/code_list.php) language code.  
2. Copy the two po files in that directory.
3. Translate the two files using a gettext editor. [Poedit](http://www.poedit.net/) or 
   [Virtaal](http://virtaal.translatehouse.org/) are two nice editors for .po files.
4. Compile the po files to mo files (Poedit can do that) and you are set. Use the 
   [View Menu](viewmenu) to change the language and test your translation.
5. Please submit your translation files to pyscripter@gmail.com for inclusion in the next 
   PyScripter distribution.  
  
**Tip:** Use the translation memory of Poedit to speed up the translation process.

## Get involved in the regular translation process 

We use [transifex.com](https://www.transifex.com/), the web based translation 
platform, as our preferred way to work on translations. So if you want to help us, please head 
over to [PyScripter at Transifex](https://app.transifex.com/pyscripter/pyscripter/dashboard/), 
register an account and join one of our language teams.

You can also request a new language for PyScripter via Transifex, but please do this only if you 
are ready to complete it and maintain it for a while. A 5% finished translation is of no use to 
anybody.

With transifex you can either work on translations online or work offline with poEdit or 
Virtaal. Once you are member of a translation team, you can download and upload the translation 
files to your computer.
