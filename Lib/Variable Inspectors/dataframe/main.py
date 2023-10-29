#!/usr/bin/env python
"""
    DataExplore Application based on pandastable.
    Created January 2014
    Copyright (C) Damien Farrell

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"""

from __future__ import absolute_import, print_function
import sys, datetime, pickle, gzip
from tkinter import *
from tkinter.ttk import *
#import ttkbootstrap as ttk
#from ttkbootstrap.constants import *
from tkinter import filedialog, messagebox, simpledialog

# Check requirements
import os
apppath = os.path.dirname(os.path.abspath(__file__))
with open(os.path.join(apppath, "requirements.txt")) as f:
    requirements = f.read()
    for line in requirements.splitlines():
        try:
            if line != "":
                __import__(line)
        except:
            messagebox.showerror("Missing package dependencies",
                "To use the dataframe debug inspector you need to install the following packages:\n"
                + requirements)
            exit()

from collections import OrderedDict
import matplotlib
matplotlib.use('TkAgg')
import pandas as pd
import re, platform, time
from pandastable.core import Table
from pandastable.data import TableModel
#from .prefs import Preferences
from pandastable import images, util, dialogs, plotting, config
from pandastable.dialogs import MultipleValDialog
from pandastable import plugin

class DataExplore(Frame):
    """DataExplore application using pandastable widget.
        Args:
            parent: parent tkinter Frame, default None
            data: data, a pandas DataFrame
            projfile: path to a project file, opened on launch
            msgpack: path to a dataframe stored as msgpack, default None
    """

    def __init__(self, parent=None, data=None, projfile=None, msgpack=None):
        """Initialize the application. """

        self.parent=parent
        if not self.parent:
            Frame.__init__(self)
            self.main=self.master
        else:
            self.main=Toplevel()
            self.master=self.main

        self.main.withdraw()

        if getattr(sys, 'frozen', False):
            #the application is frozen
            self.modulepath = os.path.dirname(sys.executable)
        else:
            self.modulepath = os.path.dirname(__file__)

        icon = os.path.join(self.modulepath,'dataexplore.gif')
        img = PhotoImage(file=icon)
        self.main.tk.call('wm', 'iconphoto', self.main._w, img)

        # Get platform into a variable
        self.currplatform = platform.system()
        self.setConfigDir()
        #if not hasattr(self,'defaultsavedir'):
        self.defaultsavedir = os.path.join(os.path.expanduser('~'))
        self.loadAppOptions()
        #start logging
        self.start_logging()

        self.main.title('DataExplore')
        self.createMenuBar()
        self.discoverPlugins()
        self.setupGUI()
        self.setStyles()
        self.clipboarddf = None
        self.projopen = False

        opts = {'layout':{'type':'checkbutton','default':'horizontal'}}
        #self.prefs = Prefs('.dataexplore', opts=opts)
        if data != None:
            self.data = data
            self.newProject(data)
        elif projfile != None:
            self.loadProject(projfile)
        elif msgpack != None:
            self.load_msgpack(msgpack)
        else:
            self.newProject()
        self.main.protocol('WM_DELETE_WINDOW',self.quit)
        self.main.lift()
        return

    def start_logging(self):
        import logging
        from pandastable.core import logfile
        logging.basicConfig(filename=logfile,format='%(asctime)s %(message)s')

    def setStyles(self):
        """Set theme and widget styles"""

        style = self.style = Style(self)
        available_themes = self.style.theme_names()
        plf = util.checkOS()
        if plf == 'linux':
            style.theme_use('default')
        elif plf == 'darwin':
            style.theme_use('clam')

        self.bg = bg = self.style.lookup('TLabel.label', 'background')
        style.configure('Horizontal.TScale', background=bg)
        #set common background style for all widgets because of color issues
        #if plf in ['linux','darwin']:
        #    self.option_add("*background", bg)
        dialogs.applyStyle(self.menu)
        return

    def setConfigDir(self):
        """Set up config folder"""

        homepath = os.path.join(os.path.expanduser('~'))
        path = '.dataexplore'
        self.configpath = os.path.join(homepath, path)
        self.pluginpath = os.path.join(self.configpath, 'plugins')
        if not os.path.exists(self.configpath):
            os.mkdir(self.configpath)
            os.makedirs(self.pluginpath)
        return

    def setupGUI(self):
        """Add all GUI elements"""

        self.m = PanedWindow(self.main, orient=HORIZONTAL)
        self.m.pack(fill=BOTH,expand=1)
        self.nb = Notebook(self.main)
        self.m.add(self.nb)
        self.setGeometry()
        return

    def createMenuBar(self):
        """Create the menu bar for the application. """

        self.menu = Menu(self.main)
        file_menu = Menu(self.menu,tearoff=0)
        #add recent first
        self.createRecentMenu(file_menu)
        filemenuitems = {'01New Project':{'cmd': self.newProject},
                    '02Open Project':{'cmd': lambda: self.loadProject(asksave=True)},
                    '03Close':{'cmd':self.closeProject},
                    '04Save':{'cmd':self.saveProject},
                    '05Save As':{'cmd':self.saveasProject},
                    '06sep':'',
                    '07Import CSV':{'cmd':self.importCSV},
                    '08Import HDF5':{'cmd':self.importHDF},
                    '09Import from URL':{'cmd':self.importURL},
                    '10Import Excel':{'cmd':self.importExcel},
                    '10Export CSV':{'cmd':self.exportCSV},
                    '11sep':'',
                    '12Quit':{'cmd':self.quit}}

        self.file_menu = self.createPulldown(self.menu, filemenuitems, var=file_menu)
        self.menu.add_cascade(label='File',menu=self.file_menu['var'])

        editmenuitems = {'01Undo Last Change':{'cmd': self.undo},
                        '02Copy Table':{'cmd': self.copyTable},
                        '03Find/Replace':{'cmd':self.findText},
                        '04Preferences':{'cmd': self.currentTablePrefs}
                        }
        self.edit_menu = self.createPulldown(self.menu, editmenuitems)
        self.menu.add_cascade(label='Edit',menu=self.edit_menu['var'])

        self.sheet_menu={'01Add Sheet':{'cmd': lambda: self.addSheet(select=True)},
                         '02Remove Sheet':{'cmd': lambda: self.deleteSheet(ask=True)},
                         '03Copy Sheet':{'cmd':self.copySheet},
                         '04Rename Sheet':{'cmd':self.renameSheet},
                         #'05Sheet Description':{'cmd':self.editSheetDescription}
                         }
        self.sheet_menu = self.createPulldown(self.menu,self.sheet_menu)
        self.menu.add_cascade(label='Sheet',menu=self.sheet_menu['var'])

        self.view_menu={'01Zoom In':{'cmd': lambda: self._call('zoomIn')},
                        '02Zoom Out':{'cmd': lambda: self._call('zoomOut')},
                        '03Wrap Columns':{'cmd': lambda: self._call('setWrap')},
                        '04sep':'',
                        '05Dark Theme':{'cmd': lambda: self._call('setTheme', name='dark')},
                        '06Bold Theme':{'cmd': lambda: self._call('setTheme', name='bold')},
                        '07Default Theme':{'cmd': lambda: self._call('setTheme', name='default')},
                        }
        self.view_menu = self.createPulldown(self.menu,self.view_menu)
        self.menu.add_cascade(label='View',menu=self.view_menu['var'])

        self.table_menu={'01Describe Table':{'cmd':self.describe},
                         '02Convert Column Names':{'cmd':lambda: self._call('convertColumnNames')},
                         '03Convert Numeric':{'cmd': lambda: self._call('convertNumeric')},
                         '04Clean Data': {'cmd': lambda: self._call('cleanData')},
                         '05Find Duplicates': {'cmd': lambda: self._call('findDuplicates')},
                         '06Correlation Matrix':{'cmd': lambda: self._call('corrMatrix')},
                         '07Concatenate Tables':{'cmd':self.concat},
                         '08Table to Text':{'cmd': lambda: self._call('showasText')},
                         '09Table Info':{'cmd': lambda: self._call('showInfo')},
                         '10sep':'',
                         '11Transform Values':{'cmd': lambda: self._call('transform')},
                         '12Group-Aggregate':{'cmd': lambda: self._call('aggregate')},
                         '13Cross Tabulation':{'cmd': lambda: self._call('crosstab')},
                         '14Merge/Concat Tables': {'cmd': lambda: self._call('doCombine')},
                         '15Pivot Table':{'cmd': lambda: self._call('pivot')},
                         '16Melt Table':{'cmd': lambda: self._call('melt')},
                         '17Time Series Resampling':{'cmd': lambda: self._call('resample')}
                        }
        self.table_menu = self.createPulldown(self.menu,self.table_menu)
        self.menu.add_cascade(label='Tools',menu=self.table_menu['var'])

        self.dataset_menu={'01Sample Data':{'cmd':self.sampleData},
                         '03Iris Data':{'cmd': lambda: self.getData('iris.csv')},
                         '03Tips Data':{'cmd': lambda: self.getData('tips.csv')},
                         '04Stacked Data':{'cmd':self.getStackedData},
                         '05Pima Diabetes':
                             {'cmd': lambda: self.getData('pima.csv')},
                         '06Titanic':
                             {'cmd': lambda: self.getData('titanic3.csv')},
                         '07miRNA expression':
                             {'cmd': lambda: self.getData('miRNA.csv')},
                         '08CO2 time series':
                             {'cmd': lambda: self.getData('co2-ppm-mauna-loa.csv')},
                         '09Zoo Dataset':
                             {'cmd': lambda: self.getData('zoo_dataset.csv')},
                         }
        self.dataset_menu = self.createPulldown(self.menu,self.dataset_menu)
        self.menu.add_cascade(label='Datasets',menu=self.dataset_menu['var'])

        self.plots_menu={'01Store plot':{'cmd':self.addPlot},
                         '02Clear plots':{'cmd':self.updatePlotsMenu},
                         '03PDF report':{'cmd':self.pdfReport},
                         '04sep':''}
        self.plots_menu = self.createPulldown(self.menu,self.plots_menu)
        self.menu.add_cascade(label='Plots',menu=self.plots_menu['var'])

        self.plugin_menu={'01Update Plugins':{'cmd':self.discoverPlugins},
                          '02Install Plugin':{'cmd':self.installPlugin},
                          '03sep':''}
        self.plugin_menu=self.createPulldown(self.menu,self.plugin_menu)
        self.menu.add_cascade(label='Plugins',menu=self.plugin_menu['var'])

        self.help_menu={'01Online Help':{'cmd':self.online_documentation},
                        '02View Error Log':{'cmd':self.showErrorLog},
                        '03About':{'cmd':self.about}}
        self.help_menu=self.createPulldown(self.menu,self.help_menu)
        self.menu.add_cascade(label='Help',menu=self.help_menu['var'])

        self.main.config(menu=self.menu)
        return

    def createRecentMenu(self, menu):
        """Recent projects menu"""

        from functools import partial
        recent = self.appoptions['recent']
        recentmenu = Menu(menu)
        menu.add_cascade(label="Open Recent", menu=recentmenu)
        for r in recent:
            recentmenu.add_command(label=r, command=partial(self.loadProject, r))
        return

    def bring_to_foreground(self, set_focus=False):
        self.main.deiconify()
        self.main.attributes('-topmost', True)
        self.main.after_idle(self.main.attributes, '-topmost', False)
        self.main.lift()

        if set_focus:
            #Looks like at least on Windows the following is required for the window
            #to also get focus (deiconify, ..., iconify, deiconify)
            import platform
            if platform.system() != "Linux":
                # http://stackoverflow.com/a/13867710/261181
                self.main.iconify()
                self.main.deiconify()
        return

    def getBestGeometry(self):
        """Calculate optimal geometry from screen size"""

        ws = self.main.winfo_screenwidth()
        hs = self.main.winfo_screenheight()
        if ws<1400:
            g = '%dx%d+%d+%d' % (ws,hs,0,0)
            self.w = ws
        else:
            self.w = w = ws/1.3; h = hs*0.7
            x = (ws/2)-(w/2); y = (hs/2)-(h/2)
            g = '%dx%d+%d+%d' % (w,h,x,y)
        return g

    def setGeometry(self):
        self.winsize = self.getBestGeometry()
        self.main.geometry(self.winsize)
        return

    def createPulldown(self, menu, dict, var=None):
        """Create pulldown menu, returns a dict.
        Args:
            menu: parent menu bar
            dict: dictionary of the form -
            {'01item name':{'cmd':function name, 'sc': shortcut key}}
            var: an already created menu
        """

        if var is None:
            var = Menu(menu,tearoff=0)
        dialogs.applyStyle(var)
        items = list(dict.keys())
        items.sort()
        for item in items:
            if item[-3:] == 'sep':
                var.add_separator()
            else:
                command = dict[item]['cmd']
                label = '%-25s' %(item[2:])
                if 'img' in dict[item]:
                    img = dict[item]['img']
                else:
                    img = None
                if 'sc' in dict[item]:
                    sc = dict[item]['sc']
                    #bind command
                    #self.main.bind(sc, command)
                else:
                    sc = None
                var.add('command', label=label, command=command, image=img,
                        compound="left")#, accelerator=sc)
        dict['var'] = var
        return dict

    def progressDialog(self):

        t = Toplevel(self)
        pb = Progressbar(t, mode="indeterminate")
        pb.pack(side="bottom", fill=X)
        t.title('Progress')
        t.transient(self)
        t.grab_set()
        t.resizable(width=False, height=False)
        return pb

    def currentTablePrefs(self):
        """Preferences dialog"""

        table = self.getCurrentTable()
        table.showPreferences()
        return

    def loadMeta(self, table, meta):
        """Load meta data for a sheet, this includes plot options and
        table selections"""

        tablesettings = meta['table']
        if 'childtable' in meta:
            childtable = meta['childtable']
            childsettings = meta['childselected']
        else:
            childtable = None
        #load plot options
        opts = {'mplopts': table.pf.mplopts,
                'mplopts3d': table.pf.mplopts3d,
                'labelopts': table.pf.labelopts
                }
        for m in opts:
            if m in meta and meta[m] is not None:
                #util.setAttributes(opts[m], meta[m])
                opts[m].updateFromDict(meta[m])
                #check options loaded for missing values
                #avoids breaking file saves when options changed
                defaults = plotting.get_defaults(m)
                for key in defaults:
                    if key not in opts[m].opts:
                        opts[m].opts[key] = defaults[key]

        #load table settings
        util.setAttributes(table, tablesettings)
        #load plotviewer
        if 'plotviewer' in meta:
            #print (meta['plotviewer'])
            util.setAttributes(table.pf, meta['plotviewer'])
            table.pf.updateWidgets()

        if childtable is not None:
            table.createChildTable(df=childtable)
            util.setAttributes(table.child, childsettings)

        #redraw col selections
        if type(table.multiplecollist) is tuple:
            table.multiplecollist = list(table.multiplecollist)
        table.drawMultipleCols()
        return

    def saveMeta(self, table):
        """Save meta data such as current plot options"""

        meta = {}
        #save plot options
        meta['mplopts'] = table.pf.mplopts.kwds
        meta['mplopts3d'] = table.pf.mplopts3d.kwds
        meta['labelopts'] = table.pf.labelopts.kwds
        #print (table.pf.mplopts.kwds)

        #save table selections
        meta['table'] = util.getAttributes(table)
        meta['plotviewer'] = util.getAttributes(table.pf)
        #print (meta['plotviewer'])
        #save row colors since its a dataframe and isn't picked up by getattributes currently
        meta['table']['rowcolors'] = table.rowcolors
        #save child table if present
        if table.child != None:
            meta['childtable'] = table.child.model.df
            meta['childselected'] = util.getAttributes(table.child)

        return meta

    def saveAppOptions(self):
        """Save global app options to config dir"""

        appfile = os.path.join(self.configpath, 'app.p')
        file = open(appfile,'wb')
        pickle.dump(self.appoptions, file, protocol=2)
        file.close()
        return

    def loadAppOptions(self):
        """Load global app options if present"""

        appfile = os.path.join(self.configpath, 'app.p')
        if os.path.exists(appfile):
            self.appoptions = pickle.load(open(appfile,'rb'))
        else:
            self.appoptions = {}
            self.appoptions['recent'] = []
        return

    def newProject(self, data=None, df=None):
        """Create a new project from data or empty"""

        w = self.closeProject()
        if w == None:
            return
        self.sheets = OrderedDict()
        self.sheetframes = {} #store references to enclosing widgets
        self.openplugins = {} #refs to running plugins
        self.updatePlotsMenu()
        for n in self.nb.tabs():
            self.nb.forget(n)
        if data != None:
            for s in sorted(data.keys()):
                if s == 'meta':
                    continue
                df = data[s]['table']
                if 'meta' in data[s]:
                    meta = data[s]['meta']
                else:
                    meta=None
                #try:
                self.addSheet(s, df, meta)
                '''except Exception as e:
                    print ('error reading in options?')
                    print (e)'''
        else:
            self.addSheet('sheet1')
        self.filename = None
        self.projopen = True
        self.main.title('DataExplore')
        return

    def loadProject(self, filename=None, asksave=False):
        """Open project file"""

        w=True
        if asksave == True:
            w = self.closeProject()
        if w == None:
            return

        if filename == None:
            filename = filedialog.askopenfilename(defaultextension='.dexpl"',
                                                    initialdir=self.defaultsavedir,
                                                    filetypes=[("project","*.dexpl"),
                                                               ("All files","*.*")],
                                                    parent=self.main)
        if not filename:
            return
        if not os.path.exists(filename):
            print ('no such file')
            self.removeRecent(filename)
            return
        ext = os.path.splitext(filename)[1]
        if ext != '.dexpl':
            print ('does not appear to be a project file')
            return
        if os.path.isfile(filename):
            #new format uses pickle
            try:
                data = pickle.load(gzip.GzipFile(filename, 'r'))
            except OSError as oe:
                msg = 'DataExplore can no longer open the old format project files.\n'\
                'if you really need the file revert to pandastable<=0.12.1 and save the data.'
                messagebox.showwarning("Project open error", msg)
                return
            #create backup file before we change anything
            #backupfile = filename+'.bak'
            #pd.to_msgpack(backupfile, data, encoding='utf-8')
        else:
            print ('no such file')
            self.quit()
            return
        self.newProject(data)
        self.filename = filename
        self.main.title('%s - DataExplore' %filename)
        self.projopen = True
        self.defaultsavedir = os.path.dirname(os.path.abspath(filename))
        self.addRecent(filename)
        return

    def removeRecent(self, filename):
        """Remove file from recent list"""

        recent = self.appoptions['recent']
        if filename in recent:
            recent.remove(filename)
            self.saveAppOptions()
        return

    def addRecent(self, filename):
        """Add file name to recent projects"""

        recent = self.appoptions['recent']
        if not os.path.abspath(filename) in recent:
            if len(recent)>=5:
                recent.pop(0)
            recent.append(os.path.abspath(filename))
            self.saveAppOptions()
        return

    def saveProject(self, filename=None):
        """Save project"""

        if filename != None:
            self.filename = filename
        if not hasattr(self, 'filename') or self.filename == None:
            self.saveasProject()
        else:
            self.doSaveProject(self.filename)
        return

    def saveasProject(self):
        """Save as a new filename"""

        filename = filedialog.asksaveasfilename(parent=self.main,
                                                defaultextension='.dexpl',
                                                initialdir=self.defaultsavedir,
                                                filetypes=[("project","*.dexpl")])
        if not filename:
            return
        self.filename = filename
        self.defaultsavedir = os.path.dirname(os.path.abspath(filename))
        self.doSaveProject(self.filename)
        self.addRecent(filename)
        return

    def doSaveProject(self, filename):
        """Save sheets as dict in msgpack"""

        self._checkTables()
        data={}
        for i in self.sheets:
            table = self.sheets[i]
            data[i] = {}
            data[i]['table'] = table.model.df
            data[i]['meta'] = self.saveMeta(table)

        #pd.to_msgpack(filename, data, encoding='utf-8')
        #changed to pickle format
        file = gzip.GzipFile(filename, 'w')
        pickle.dump(data, file)
        return

    def _checkTables(self):
        """Check tables before saving that so we are not saving
        filtered copies"""

        for s in self.sheets:
            t=self.sheets[s]
            if t.filtered==True:
                t.showAll()
        return

    def closeProject(self):
        """Close"""

        if self.projopen == False:
            w = False
        else:
            w = messagebox.askyesnocancel("Close Project",
                                        "Save this project?",
                                        parent=self.master)
        if w==None:
            return
        elif w==True:
            self.saveProject()
        else:
            pass
        for n in self.nb.tabs():
            self.nb.forget(n)
        self.filename = None
        self.projopen = False
        self.main.title('DataExplore')
        return w

    def importCSV(self):
        """Import csv to a new sheet"""

        self.addSheet(select=True)
        table = self.getCurrentTable()
        table.importCSV(dialog=True)
        return

    def importHDF(self):
        """Import csv to a new sheet"""

        self.addSheet(select=True)
        table = self.getCurrentTable()
        table.importHDF(dialog=True)
        return

    def importURL(self):
        """Import CSV from URL"""

        url = simpledialog.askstring("Import url", "Input CSV URL",
                                     parent=self.master)
        if url is not None:
            name = os.path.basename(url)
            df = pd.read_csv(url)
            self.addSheet(sheetname=name, df=df, select=True)
        return

    def exportCSV(self):
        """Import csv to a new sheet"""

        table = self.getCurrentTable()
        table.doExport()
        return

    def importExcel(self, filename=None):
        if filename is None:
            filename = filedialog.askopenfilename(parent=self.master,
                                                          defaultextension='.xls',
                                                          initialdir=os.getcwd(),
                                                          filetypes=[("xls","*.xls"),
                                                                     ("xlsx","*.xlsx"),
                                                            ("All files","*.*")])

        data = pd.read_excel(filename,sheet_name=None)
        for n in data:
            self.addSheet(n, df=data[n], select=True)
        return

    def load_dataframe(self, df, name=None, select=False):
        """Load a DataFrame into a new sheet
           Args:
            df: dataframe
            name: name of new sheet
            select: set new sheet as selected
        """

        if hasattr(self,'sheets'):
            self.addSheet(sheetname=name, df=df, select=select)
        else:
            data = {name:{'table':df}}
            self.newProject(data)
        return

    def load_msgpack(self, filename):
        """Load a msgpack file"""

        size = round((os.path.getsize(filename)/1.0485e6),2)
        print (size)
        df = pd.read_msgpack(filename)
        name = os.path.splitext(os.path.basename(filename))[0]
        self.load_dataframe(df, name)
        return

    def load_pickle(self, filename):
        """Load a pickle file"""

        df = pd.read_pickle(filename)
        name = os.path.splitext(os.path.basename(filename))[0]
        self.load_dataframe(df, name)
        return

    def getData(self, name):
        """Get predefined data from dataset folder"""

        filename = os.path.join(self.modulepath, 'datasets', name)
        df = pd.read_csv(filename, index_col=0)
        name = os.path.splitext(os.path.basename(filename))[0]
        self.load_dataframe(df, name, select=True)
        return

    def addSheet(self, sheetname=None, df=None, meta=None, select=False):
        """Add a sheet with new or existing data"""

        names = [self.nb.tab(i, "text") for i in self.nb.tabs()]
        def checkName(name):
            if name == '':
                messagebox.showwarning("Whoops", "Name should not be blank.")
                return 0
            if name in names:
                messagebox.showwarning("Name exists", "Sheet name already exists!")
                return 0

        noshts = len(self.nb.tabs())
        if sheetname == None:
            sheetname = simpledialog.askstring("New sheet name?", "Enter sheet name:",
                                                initialvalue='sheet'+str(noshts+1))
        if sheetname == None:
            return
        if checkName(sheetname) == 0:
            return
        #Create the table
        main = PanedWindow(orient=HORIZONTAL)
        self.sheetframes[sheetname] = main
        self.nb.add(main, text=sheetname)
        f1 = Frame(main)
        table = Table(f1, dataframe=df, showtoolbar=1, showstatusbar=1)
        f2 = Frame(main)
        #show the plot frame
        pf = table.showPlotViewer(f2)
        #load meta data
        if meta != None:
            self.loadMeta(table, meta)
        #add table last so we have save options loaded already
        main.add(f1,weight=3)
        table.show()
        main.add(f2,weight=4)

        if table.plotted == 'main':
            table.plotSelected()
        elif table.plotted == 'child' and table.child != None:
            table.child.plotSelected()
        self.saved = 0
        self.currenttable = table
        #attach menu state of undo item so that it's disabled after an undo
        #table.undo_callback = lambda: self.toggleUndoMenu('active')
        self.sheets[sheetname] = table

        if select == True:
            ind = self.nb.index('end')-1
            s = self.nb.tabs()[ind]
            self.nb.select(s)
        return sheetname

    def deleteSheet(self, ask=False):
        """Delete a sheet"""

        s = self.nb.index(self.nb.select())
        name = self.nb.tab(s, 'text')
        w=True
        if ask == True:
            w = messagebox.askyesno("Delete Sheet",
                                     "Remove this sheet?",
                                     parent=self.master)
        if w==False:
            return
        self.nb.forget(s)
        del self.sheets[name]
        del self.sheetframes[name]
        return

    def copySheet(self, newname=None):
        """Copy a sheet"""

        currenttable = self.getCurrentTable()
        newdata = currenttable.model.df
        meta = self.saveMeta(currenttable)
        self.addSheet(newname, df=newdata, meta=meta)
        return

    def renameSheet(self):
        """Rename a sheet"""

        s = self.nb.tab(self.nb.select(), 'text')
        newname = simpledialog.askstring("New sheet name?",
                                          "Enter new sheet name:",
                                          initialvalue=s)
        if newname == None:
            return
        self.copySheet(newname)
        self.deleteSheet()
        return

    def editSheetDescription(self):
        """Add some meta data about the sheet"""

        from pandastable.dialogs import SimpleEditor
        w = Toplevel(self.main)
        w.grab_set()
        w.transient(self)
        ed = SimpleEditor(w, height=25)
        ed.pack(in_=w, fill=BOTH, expand=Y)
        #ed.text.insert(END, buf.getvalue())
        return

    def getCurrentSheet(self):
        """Get current sheet name"""

        s = self.nb.index(self.nb.select())
        name = self.nb.tab(s, 'text')
        return name

    def getCurrentTable(self):

        s = self.nb.index(self.nb.select())
        name = self.nb.tab(s, 'text')
        table = self.sheets[name]
        return table

    def getSheetList(self):
        return list(self.sheets.keys())

    def describe(self):
        """Describe dataframe"""

        table = self.getCurrentTable()
        df = table.model.df
        d = df.describe()
        table.createChildTable(d,index=True)
        return

    def findText(self):

        table = self.getCurrentTable()
        table.findText()
        return

    def concat(self):
        """Concat 2 tables"""

        vals = list(self.sheets.keys())
        if len(vals)<=1:
            return
        d = MultipleValDialog(title='Concat',
                                initialvalues=(vals,vals),
                                labels=('Table 1','Table 2'),
                                types=('combobox','combobox'),
                                parent = self.master)
        if d.result == None:
            return
        else:
            s1 = d.results[0]
            s2 = d.results[1]
        if s1 == s2:
            return
        df1 = self.sheets[s1].model.df
        df2 = self.sheets[s2].model.df
        m = pd.concat([df1,df2])
        self.addSheet('concat-%s-%s' %(s1,s2),m)
        return

    def sampleData(self):
        """Load sample table"""

        d = MultipleValDialog(title='Sample Data',
                                initialvalues=(100,5),
                                labels=('Rows','Columns'),
                                types=('int','int'),
                                parent = self.master)
        if d.result == None:
            return
        rows=d.results[0]
        cols=d.results[1]
        df = TableModel.getSampleData(rows=rows,cols=cols)
        name='sample'
        i=1
        while name in self.sheets:
            name='sample'+str(i)
            i+=1
        self.addSheet(sheetname=name, df=df, select=True)
        return

    def getStackedData(self):

        df = TableModel.getStackedData()
        self.addSheet(sheetname='stacked-data', df=df)
        return

    def fileRename(self):
        """Start file renaming util"""

        from pandastable.plugins.rename import BatchRenameApp
        br = BatchRenameApp(self.master)
        return

    def copyTable(self, subtable=False):
        """Copy current table dataframe"""

        table = self.getCurrentTable()
        table.model.df.to_clipboard()
        return

    def pasteTable(self, subtable=False):
        """Paste copied dataframe into current table"""

        #add warning?
        if self.clipboarddf is None:
            return
        df = self.clipboarddf
        table = self.getCurrentTable()
        if subtable == True:
            table.createChildTable(df)
        else:
            model = TableModel(df)
            table.updateModel(model)
        return

    def discoverPlugins(self):
        """Discover available plugins"""

        if getattr(sys, 'frozen', False):
            #the application is frozen
            apppath = os.path.dirname(sys.executable)
        else:
            apppath = os.path.dirname(os.path.abspath(__file__))
        paths = [apppath,self.configpath]
        pluginpaths = [os.path.join(p, 'plugins') for p in paths]
        #print (pluginpaths)
        failed = plugin.init_plugin_system(pluginpaths)
        self.updatePluginMenu()
        return

    def installPlugin(self):
        """Adds a user supplied .py file to plugin folder"""

        filename = filedialog.askopenfilename(defaultextension='.py"',
                                              initialdir=os.getcwd(),
                                              filetypes=[("python","*.py")],
                                              parent=self.main)
        if filename:
            import shutil
            shutil.copy(filename, self.pluginpath)
            self.updatePluginMenu()
        return

    def updatePluginMenu(self):
        """Update plugins"""

        self.plugin_menu['var'].delete(3, self.plugin_menu['var'].index(END))
        plgmenu = self.plugin_menu['var']
        #for plg in plugin.get_plugins_instances('gui'):
        for plg in plugin.get_plugins_classes('gui'):
            def func(p, **kwargs):
                def new():
                   self.loadPlugin(p)
                return new
            plgmenu.add_command(label=plg.menuentry,
                               command=func(plg))
        return

    def loadPlugin(self, plugin):
        """Instantiate the plugin and call it's main method"""

        p = plugin()
        #plugin should add itself to the table frame if it's a dialog
        try:
            p.main(parent=self)
        except Exception as e:
            messagebox.showwarning("Plugin error", e,
                                    parent=self)
        name = self.getCurrentSheet()
        #track which plugin is running so the last one is removed?
        self.openplugins[name] = p
        return

    def hidePlot(self):
        name = self.getCurrentSheet()
        pw = self.sheetframes[name]
        pw.forget(1)
        return

    def showPlot(self):
        name = self.getCurrentSheet()
        table = self.sheets[name]
        pw = self.sheetframes[name]
        pw.add(table.pf, weight=2)
        return

    def addPlot(self):
        """Store the current plot so it can be re-loaded"""

        import pickle
        from . import plotting
        name = self.getCurrentSheet()
        table = self.sheets[name]
        fig = table.pf.fig
        t = time.strftime("%H:%M:%S")
        label = name+'-'+t
        #dump and reload the figure to get a new object
        p = pickle.dumps(fig)
        fig = pickle.loads(p)
        self.plots[label] = fig

        def func(label):
            fig = self.plots[label]
            win = Toplevel()
            win.title(label)
            plotting.addFigure(win, fig)

        menu = self.plots_menu['var']
        menu.add_command(label=label, command=lambda: func(label))
        return

    def updatePlotsMenu(self, clear=True):
        """Clear stored plots"""

        if clear == True:
            self.plots = {}
        menu = self.plots_menu['var']
        menu.delete(4, menu.index(END))
        return

    def pdfReport(self):
        """Create pdf report from stored plots"""

        from matplotlib.backends.backend_pdf import PdfPages
        filename = filedialog.asksaveasfilename(parent=self.main,
                                                defaultextension='.pdf',
                                                initialdir=self.defaultsavedir,
                                                filetypes=[("pdf","*.pdf")])
        if not filename:
            return
        pdf_pages = PdfPages(filename)
        from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

        for p in self.plots:
            fig = self.plots[p]
            canvas = FigureCanvasTkAgg(fig, master=self)
            pdf_pages.savefig(fig)
        pdf_pages.close()
        return

    def undo(self):
        """Restores last version of current table"""

        table = self.getCurrentTable()
        table.undo()
        #self.toggleUndoMenu('disabled')
        return

    def toggleUndoMenu(self, state='active'):
        menu = self.edit_menu['var']
        menu.entryconfigure(0, state=state)
        return

    def _call(self, func, **args):
        """Call a table function from it's string name"""

        table = self.getCurrentTable()
        getattr(table, func)(**args)
        return

    def _check_snap(self):
        if os.environ.has_key('SNAP_USER_COMMON'):
            print ('running inside snap')
            return True
        return False

    def about(self):
        """About dialog"""

        abwin = Toplevel()
        x,y,w,h = dialogs.getParentGeometry(self.main)
        abwin.geometry('+%d+%d' %(x+w/2-200,y+h/2-200))
        abwin.title('About')
        abwin.transient(self)
        abwin.grab_set()
        abwin.resizable(width=False, height=False)
        abwin.configure(background=self.bg)
        logo = images.tableapp_logo()
        label = Label(abwin,image=logo,anchor=CENTER)
        label.image = logo
        label.grid(row=0,column=0,sticky='ew',padx=4,pady=4)
        style = Style()
        style.configure("BW.TLabel", font='arial 11')
        from . import __version__
        pandasver = pd.__version__
        pythonver = platform.python_version()
        mplver = matplotlib.__version__
        if self._check_snap == True:
            snap='(snap)'
        else:
            snap=''

        text='DataExplore Application\n'\
                +'version '+__version__+snap+'\n'\
                +'Copyright (C) Damien Farrell 2014-\n'\
                +'This program is free software; you can redistribute it and/or\n'\
                +'modify it under the terms of the GNU General Public License\n'\
                +'as published by the Free Software Foundation; either version 3\n'\
                +'of the License, or (at your option) any later version.\n'\
                +'Using Python v%s\n' %pythonver\
                +'pandas v%s, matplotlib v%s' %(pandasver,mplver)

        row=1
        #for line in text:
        tmp = Label(abwin, text=text, style="BW.TLabel")
        tmp.grid(row=row,column=0,sticky='news',pady=2,padx=4)

        return

    def showErrorLog(self):
        """Open log file"""

        from pandastable.core import logfile
        f=open(logfile,'r')
        s=''.join(f.readlines())
        from pandastable.dialogs import SimpleEditor
        w = Toplevel(self)
        w.grab_set()
        w.transient(self)
        ed = SimpleEditor(w)
        ed.pack(in_=w, fill=BOTH, expand=Y)
        ed.text.insert(END, s)
        return

    def online_documentation(self,event=None):
        """Open the online documentation"""
        import webbrowser
        link='https://pandastable.readthedocs.io/en/latest/'
        webbrowser.open(link,autoraise=1)
        return

    def quit(self):
        self.main.destroy()
        return

class TestApp(Frame):
    """Basic test frame for the table"""
    def __init__(self, parent=None):
        self.parent = parent
        Frame.__init__(self)
        self.main = self.master
        self.main.geometry('800x500+200+100')
        self.main.title('DataExplore Test')
        f = Frame(self.main)
        f.pack(fill=BOTH,expand=1)
        df = TableModel.getSampleData()
        self.table = pt = Table(f, dataframe=df, enable_menus=True,
                                showtoolbar=True, showstatusbar=True)
        #options = config.load_options()
        options = {'floatprecision': 5, 'textcolor':'blue'}
        pt.show()
        #pt.hideRowHeader()
        config.apply_options(options, pt)
        self.table.rowheader.bgcolor = 'orange'
        self.table.colheader.bgcolor = 'lightgreen'
        self.table.colheader.textcolor = 'purple'
        #test row coloring
        pt.setRowColors(rows=range(2,100,2), clr='lightblue', cols='all')
        #test deleting
        pt.setSelectedRows([[4,6,8,10]])
        pt.deleteRow()
        pt.setSelectedRow()
        pt.insertRow()
        #pt.redraw()
        pt.editable = False
        #pt.showRowHeader()
        return

def main():
    if len(sys.argv) < 2:
        messagebox.showerror("Expected an argument with the filename")
        exit()

    pickle_file = sys.argv[1]

    import numpy as np
    with open(pickle_file, 'rb') as f:
        value = pickle.load(f)

    if isinstance(value, np.matrix):
        col_names = ['col' + str(i) for i in np.arange(value.shape[1]) + 1]
        df = pd.DataFrame(data=value, columns=col_names)
    elif isinstance(value, np.ndarray):
        if len(value.shape) == 1:
            df = pd.DataFrame(data=value, columns=["col1"])
        elif len(value.shape) == 2:
            col_names = ['col' + str(i) for i in np.arange(value.shape[0]) + 1]
            df = pd.DataFrame(data=value.T, columns=col_names)
        else:
            messagebox.showerror("Only 1 and 2-dimensional numpy arrays are supported")
            exit()
    elif isinstance(value, pd.DataFrame):
        df = value
    else:
        messagebox.showerror('Invalid pickled value')
        exit()

    app = DataExplore()
    app.deleteSheet(app.getCurrentSheet)
    app.load_dataframe(df, 'Inspected dataframe')

    app.main.deiconify()
    app.mainloop()

if __name__ == '__main__':
    main()
