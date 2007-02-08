inherited PythonIIForm: TPythonIIForm
  Left = 259
  Top = 201
  HelpContext = 410
  Caption = 'Python Interpreter'
  ClientHeight = 453
  ClientWidth = 703
  Icon.Data = {
    0000010001001010040000000000280100001600000028000000100000002000
    0000010004000000000080000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00098F
    F8733A3333808912228F2AA333070793AA20780A3330F709102A2780A333F223
    3000A272AA332AAAAA200A22AA330A3333AA023223330333333A22AA03330333
    333333AA0333F003333333323307F88000300330008F0FF7B33B3308FFF000F7
    CC3CC30F000000F7B33B330F000000F80000337F0000000F807007F000008001
    0000000000008000000000000000000000000000000000000000000000000000
    0000000000000000000080010000C00F0000C00F0000C00F0000E01F0000}
  Position = poDefault
  OnActivate = FormActivate
  OnHelp = FormHelp
  ExplicitWidth = 711
  ExplicitHeight = 479
  DesignSize = (
    703
    453)
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Width = 700
    Height = 444
    Color = clInactiveBorder
    ExplicitWidth = 700
    ExplicitHeight = 444
    object SynEdit: TSynEdit
      Left = 0
      Top = 0
      Width = 700
      Height = 444
      HelpContext = 410
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      PopupMenu = InterpreterPopUp
      TabOrder = 0
      OnDblClick = SynEditDblClick
      BorderStyle = bsNone
      Gutter.BorderStyle = gbsNone
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Gutter.Gradient = True
      Options = [eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent, eoTabsToSpaces]
      RightEdge = 0
      TabWidth = 4
      WantTabs = True
      WordWrap = True
      OnCommandProcessed = SynEditCommandProcessed
      OnProcessCommand = SynEditProcessCommand
      OnProcessUserCommand = SynEditProcessUserCommand
      OnReplaceText = SynEditReplaceText
      OnPaintTransient = SynEditPaintTransient
      ExplicitLeft = 1
      ExplicitTop = -1
    end
  end
  object PythonEngine: TPythonEngine
    AutoLoad = False
    DllName = 'python24.dll'
    APIVersion = 1012
    RegVersion = '2.4'
    FatalAbort = False
    FatalMsgDlg = False
    UseLastKnownVersion = False
    InitScript.Strings = (
      'import sys'
      'import code'
      ''
      'try:'
      '    sys.ps1'
      'except AttributeError:'
      '    sys.ps1 = '#39'>>> '#39
      '    sys.ps2 = '#39'... '#39
      ''
      'class PythonInteractiveInterpreter(code.InteractiveInterpreter):'
      '    debugIDE = __import__("DebugIDE")'
      ''
      '    class Future:'
      '        """'
      '            Python future implementation'
      '            calculates a function value in a thread'
      '        """'
      ''
      '        def __init__(self, func, *args, **kwargs):'
      '            import threading'
      '            # Constructor'
      '            self.__done = False'
      '            self.__result = None'
      '            self.__status = '#39'working'#39
      '            self.__excpt = None'
      ''
      '            # Run the actual function in a separate thread'
      
        '            self.__T = threading.Thread(target=self.Wrapper, arg' +
        's=(func, args, kwargs))'
      '            self.__T.setName("FutureThread")'
      '            self.__T.start()'
      ''
      '        def __repr__(self):'
      
        '            return '#39'<Future at '#39'+hex(id(self))+'#39':'#39'+self.__status' +
        '+'#39'>'#39
      ''
      '        def __call__(self):'
      '            import time'
      '            debugIDE = __import__("DebugIDE")'
      '            while not self.__done:'
      '                debugIDE.awakeGUI()'
      '                time.sleep(0.001)'
      ''
      '            if self.__excpt:'
      
        '                raise self.__excpt[0], self.__excpt[1], self.__e' +
        'xcpt[2].tb_next'
      ''
      '            return self.__result'
      ''
      '        def isDone(self):'
      '          return self.__done'
      ''
      '        def Wrapper(self, func, args, kwargs):'
      
        '            # Run the actual function, and let us housekeep arou' +
        'nd it'
      '            import sys'
      '            try:'
      '                self.__result = func(*args, **kwargs)'
      '            except:'
      '                self.__result = "Exception raised within Future"'
      '                self.__excpt = sys.exc_info()'
      ''
      '            self.__done = True'
      '            self.__status=`self.__result`'
      ''
      '    def execInThread(self, func, args):'
      '        #keeps GUI alive'
      '        future = self.Future(func, *args)'
      '        return future()'
      ''
      '    class IDEDebugger(__import__('#39'bdb'#39').Bdb):'
      '        debugIDE = __import__("DebugIDE")'
      ''
      '        def do_clear(self, arg):'
      '            import string'
      '            numberlist = string.split(arg)'
      '            for i in numberlist:'
      '                self.clear_bpbynumber(i)'
      ''
      '        def stop_here(self, frame):'
      '            import bdb'
      '            if not self.InitStepIn:'
      '                self.InitStepIn = True'
      '                self.set_continue()'
      '                return 0'
      '            return bdb.Bdb.stop_here(self, frame)'
      ''
      '        def user_call(self, frame, args):'
      '            self.InIDEDebug = True'
      '            self.currentframe = frame'
      '            try:'
      '                self.debugIDE.user_call(frame, args)'
      '            finally:'
      '                self.InIDEDebug = False'
      ''
      '        def user_line(self, frame):'
      '            self.InIDEDebug = True'
      '            self.currentframe = frame'
      '            try:'
      '                self.debugIDE.user_line(frame)'
      '            finally:'
      '                self.InIDEDebug = False'
      ''
      '        def user_return(self, frame, retval):'
      '            self.InIDEDebug = True'
      '            self.currentframe = frame'
      '            try:'
      '                self.debugIDE.user_return(frame, retval)'
      '            finally:'
      '                self.InIDEDebug = False'
      ''
      '        def user_exception(self, frame, exc_stuff):'
      '            self.InIDEDebug = True'
      '            self.currentframe = frame'
      '            try:'
      '                self.debugIDE.user_exception(frame, exc_stuff)'
      '            finally:'
      '                self.InIDEDebug = False'
      ''
      '        def trace_dispatch(self, frame, event, arg):'
      '            self.tracecount += 1'
      
        '            if self.tracecount == 30:  #yield processing every 3' +
        '0 steps'
      '                self.tracecount = 0'
      '                self.debugIDE.user_yield()'
      
        '                if self.quitting: raise __import__('#39'bdb'#39').BdbQui' +
        't'
      
        '            return __import__('#39'bdb'#39').Bdb.trace_dispatch(self, fr' +
        'ame, event, arg)'
      ''
      '        def run(self, cmd, globals=None, locals=None):'
      '            import bdb'
      '            import sys'
      ''
      '            if globals is None:'
      '                globals = self.locals'
      '            if locals is None:'
      '                locals = globals'
      ''
      '            maindictcopy = self.locals.copy()'
      '            sysmodulescopy = sys.modules.copy()'
      ''
      '            try:'
      '                try:'
      '                    bdb.Bdb.run(self, cmd, globals, locals)'
      '                except SystemExit:'
      '                    pass'
      '            finally:'
      
        '                if self.debugIDE.cleanupMainDict() and (self.loc' +
        'als is globals):'
      '                    self.locals.clear()'
      '                    self.locals.update(maindictcopy)'
      '                if self.debugIDE.cleanupSysModules():'
      '                    sys.modules.clear()'
      '                    sys.modules.update(sysmodulescopy)'
      ''
      '    class IDETestResult(__import__('#39'unittest'#39').TestResult):'
      '        debugIDE = __import__("DebugIDE")'
      ''
      '        def startTest(self, test):'
      
        '            __import__('#39'unittest'#39').TestResult.startTest(self, te' +
        'st)'
      '            self.debugIDE.testResultStartTest(test)'
      ''
      '        def stopTest(self, test):'
      
        '            __import__('#39'unittest'#39').TestResult.stopTest(self, tes' +
        't)'
      '            self.debugIDE.testResultStopTest(test)'
      ''
      '        def addError(self, test, err):'
      
        '            __import__('#39'unittest'#39').TestResult.addError(self, tes' +
        't, err)'
      
        '            self.debugIDE.testResultAddError(test, self._exc_inf' +
        'o_to_string(err, test))'
      ''
      '        def addFailure(self, test, err):'
      
        '            __import__('#39'unittest'#39').TestResult.addFailure(self, t' +
        'est, err)'
      
        '            self.debugIDE.testResultAddFailure(test, self._exc_i' +
        'nfo_to_string(err, test))'
      ''
      '        def addSuccess(self, test):'
      '            self.debugIDE.testResultAddSuccess(test)'
      
        '            __import__('#39'unittest'#39').TestResult.addSuccess(self, t' +
        'est)'
      ''
      '    def __init__(self, locals = None):'
      '        code.InteractiveInterpreter.__init__(self, locals)'
      '        self.locals["__name__"] = "__main__"'
      ''
      '        self.debugger = self.IDEDebugger()'
      '        self.debugger.InitStepIn = False'
      '        self.debugger.InIDEDebug = False'
      '        self.debugger.tracecount = 0'
      '        self.debugger.locals = self.locals'
      ''
      '        import repr'
      '        pyrepr = repr.Repr()'
      '        pyrepr.maxstring = 80'
      '        pyrepr.maxother = 80'
      '        self._repr = pyrepr.repr'
      ''
      '    def run_nodebug(self, cmd, globals=None, locals=None):'
      '        import types'
      '        import sys'
      ''
      '        if globals is None:'
      '            globals = self.locals'
      '        if locals is None:'
      '            locals = globals'
      ''
      '        maindictcopy = self.locals.copy()'
      '        sysmodulescopy = sys.modules.copy()'
      ''
      '        if not isinstance(cmd, types.CodeType):'
      '            cmd = cmd+'#39'\n'#39
      '        try:'
      '            try:'
      '                exec cmd in globals, locals'
      '            except SystemExit:'
      '                pass'
      '        finally:'
      
        '            if self.debugIDE.cleanupMainDict() and (self.locals ' +
        'is globals):'
      '                self.locals.clear()'
      '                self.locals.update(maindictcopy)'
      '            if self.debugIDE.cleanupSysModules():'
      '                sys.modules.clear()'
      '                sys.modules.update(sysmodulescopy)'
      ''
      '    def objecttype(self, ob):'
      '        try:'
      '            if hasattr(ob, "__class__"):'
      '                return ob.__class__.__name__'
      '            elif hasattr(ob, "__bases__"):'
      '                return "classobj"'
      '            else:'
      '                return type(ob).__name__'
      '        except:'
      '            return "Unknown type"'
      ''
      '    def saferepr(self, x):'
      '        try:'
      '            return self._repr(x)'
      '        except:'
      '            return '#39'<unprintable %s object>'#39' % type(x).__name__'
      ''
      '    def _getmembers(self, ob):'
      '        return [(i, getattr(ob, i)) for i in dir(ob)]'
      ''
      '    def safegetmembers(self, x):'
      '        try:'
      '            return dict(self._getmembers(x))'
      '        except:'
      '            return {}'
      ''
      '    def find_dotted_module(self, name, path=None):'
      '        import imp'
      '        segs = name.split('#39'.'#39')'
      '        file = None'
      '        while segs:'
      '            if file: file.close()'
      
        '            file, filename, desc = imp.find_module(segs[0], path' +
        ')'
      '            del segs[0]'
      '            path = [filename]'
      '        return file, filename, desc'
      ''
      ''
      '    def findModuleOrPackage(self, modName, path=None):'
      '        if path is None:'
      '            import sys'
      '            path = sys.path'
      '        try:'
      
        '            f, filename, (ext, mode, type) =  self.find_dotted_m' +
        'odule(modName, path)'
      '        except ImportError, err:'
      '            return None'
      ''
      '        if f is not None:'
      '            f.close()'
      '        if filename:'
      '            import imp, os'
      '            if type == imp.PKG_DIRECTORY:'
      '                return os.path.join(filename, '#39'__init__.py'#39')'
      '            elif type in (imp.PY_SOURCE, imp.C_EXTENSION):'
      '                return filename'
      ''
      '    def setupRefactoring(self):'
      '        class ProgressStatusLogger:'
      
        '            """ File like logger that uses the Editor statusbar ' +
        'as output """'
      '            debugIDE = __import__("DebugIDE")'
      '            def __init__(self):'
      '                self._buffer = '#39#39
      ''
      '            def write(self, txt):'
      '                self._buffer += txt'
      '                if txt.endswith('#39'\n'#39'):'
      
        '                    debugIDE.statusWrite('#39'BRM: '#39' + self._buffer.' +
        'strip())'
      '                    self._buffer = '#39#39
      ''
      '        class WarningsLogger:'
      
        '            """ File like logger that uses the Editor statusbar ' +
        'as output """'
      '            debugIDE = __import__("DebugIDE")'
      '            def __init__(self):'
      '                self._buffer = '#39#39
      ''
      '            def write(self, txt):'
      '                self._buffer += txt'
      '                if txt.endswith('#39'\n'#39'):'
      
        '                    debugIDE.messageWrite('#39'  '#39' + self._buffer.st' +
        'rip())'
      '                    self._buffer = '#39#39
      ''
      '        try:'
      '            import bike'
      '            import bike.parsing.load'
      '            self.BRMContext = bike.init()'
      '            self.BRMCache = bike.parsing.load.Cache'
      
        '            self.BRMContext.setProgressLogger(ProgressStatusLogg' +
        'er())'
      '            self.BRMContext.setWarningLogger(WarningsLogger())'
      
        '            bike.bikefacade.BRMContext_impl.normalizeFilename = ' +
        'lambda self, fn : fn'
      '        except:'
      '            self.BRMContext = None'
      ''
      '    def loadFileToBRMCache(self, filename, src):'
      '        if self.BRMContext is not None:'
      '            import bike'
      '            sourcenode = None'
      ''
      '            try:'
      
        '                sourcenode = self.BRMCache.instance.srcnodecache' +
        '[filename]'
      '            except KeyError:'
      '                pass'
      ''
      '            if sourcenode is None:'
      
        '                from bike.parsing.newstuff import translateFname' +
        'ToModuleName'
      
        '                sourcenode = bike.parsing.load.SourceFile.create' +
        'FromString(filename,'
      
        '                                      translateFnameToModuleName' +
        '(filename), src)'
      '            if sourcenode is None:'
      '                return False'
      ''
      '            sourcenode.filename = filename # important'
      '            sourcenode.fastparseroot.filename = filename'
      '            for node in sourcenode.fastparseroot.flattenedNodes:'
      '                node.filename = filename'
      
        '            self.BRMCache.instance.srcnodecache[filename]=source' +
        'node'
      '            return True'
      ''
      '        return False'
      ''
      '    def runcode(self, code):'
      '        def softspace(file, newvalue):'
      '            oldvalue = 0'
      '            try:'
      '                oldvalue = file.softspace'
      '            except AttributeError:'
      '                pass'
      '            try:'
      '                file.softspace = newvalue'
      '            except (AttributeError, TypeError):'
      
        '                # "attribute-less object" or "read-only attribut' +
        'es"'
      '                pass'
      '            return oldvalue'
      ''
      '        try:'
      '            if self.debugger.InIDEDebug:'
      
        '                exec code in self.debugger.currentframe.f_global' +
        's, self.debugger.currentframe.f_locals'
      '            else:'
      '                exec code in self.locals'
      '        except SystemExit:'
      '            pass'
      '        except:'
      '            self.showtraceback()'
      '        else:'
      '            import sys'
      '            if softspace(sys.stdout, 0):'
      '                print'
      ''
      '    def evalcode(self, code):'
      '        # may raise exceptions'
      '        try:'
      '            if self.debugger.InIDEDebug:'
      
        '                return eval(code, self.debugger.currentframe.f_g' +
        'lobals, self.debugger.currentframe.f_locals)'
      '            else:'
      '                return eval(code, self.locals)'
      '        except SystemExit:'
      '            return None'
      ''
      '    def _find_constructor(self, class_ob):'
      
        '        # Given a class object, return a function object used fo' +
        'r the'
      
        '        # constructor (ie, __init__() ) or None if we can'#39't find' +
        ' one.  (from IDLE)'
      '        try:'
      '            return class_ob.__init__.im_func'
      '        except AttributeError:'
      '            for base in class_ob.__bases__:'
      '                rc = self._find_constructor(base)'
      '                if rc is not None: return rc'
      '        return None'
      ''
      '    def get_arg_text(self, ob):'
      
        '        "Get a string describing the arguments for the given obj' +
        'ect - From IDLE"'
      '        import types'
      
        '        from inspect import isclass, isfunction, getargspec, for' +
        'matargspec'
      '        argText = ""'
      '        if ob is not None:'
      '            argOffset = 0'
      '            if isclass(ob):'
      
        '                # Look for the highest __init__ in the class cha' +
        'in.'
      '                fob = self._find_constructor(ob)'
      '                if fob is None:'
      '                    fob = lambda: None'
      '                else:'
      '                    argOffset = 1'
      '            elif type(ob)==types.MethodType:'
      
        '                # bit of a hack for methods - turn it into a fun' +
        'ction'
      '                # but we drop the "self" param.'
      '                fob = ob.im_func'
      '                argOffset = 1'
      '            else:'
      '                fob = ob'
      '            # Try and build one for Python defined functions'
      '            if isfunction(fob):'
      '                try:'
      
        '                    args, varargs, varkw, defaults = getargspec(' +
        'fob)'
      
        '                    argText = formatargspec(args[argOffset:], va' +
        'rargs, varkw, defaults)[1:-1]'
      
        '                    #argText = "%s(%s)" % (fob.func_name, argTex' +
        't)'
      '                except:'
      '                    pass'
      '            return argText'
      ''
      '    def Win32RawInput(self, prompt=None):'
      '        "Provide raw_input() for gui apps"'
      '        # flush stderr/out first.'
      '        debugIDE = __import__("DebugIDE")'
      '        import sys'
      '        try:'
      '            sys.stdout.flush()'
      '            sys.stderr.flush()'
      '        except:'
      '            pass'
      '        if prompt is None: prompt = ""'
      
        '        ret = debugIDE.InputBox(u'#39'Python input'#39', unicode(prompt)' +
        ',u"")'
      '        if ret is None:'
      '            raise KeyboardInterrupt, "operation cancelled"'
      '        return ret'
      ''
      '    def Win32Input(self, prompt=None):'
      '        "Provide input() for gui apps"'
      '        return eval(raw_input(prompt))'
      ''
      '_II = PythonInteractiveInterpreter(globals())'
      ''
      'sys.modules['#39'__builtin__'#39'].raw_input=_II.Win32RawInput'
      'sys.modules['#39'__builtin__'#39'].input=_II.Win32Input'
      'del DebugOutput'
      'del code'
      'del PythonInteractiveInterpreter'
      'del sys'
      '')
    InitThreads = True
    IO = PythonIO
    PyFlags = [pfInteractive]
    Left = 557
    Top = 54
  end
  object PythonIO: TPythonInputOutput
    Left = 586
    Top = 53
  end
  object SynCodeCompletion: TSynCompletionProposal
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]{}. =:'
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Images = CommandsDataModule.CodeImages
    OnExecute = SynCodeCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    Left = 627
    Top = 9
    EndOfTokenChrW = '()[]{}. =:'
    TriggerCharsW = '.'
  end
  object DebugIDE: TPythonModule
    Engine = PythonEngine
    Events = <
      item
        Name = 'user_call'
      end
      item
        Name = 'user_line'
      end
      item
        Name = 'user_return'
      end
      item
        Name = 'user_exception'
        DocString.Strings = (
          'Return the 8087 CW value')
      end
      item
        Name = 'user_yield'
      end
      item
        Name = 'InputBox'
        OnExecute = InputBoxExecute
      end
      item
        Name = 'statusWrite'
        OnExecute = StatusWriteExecute
      end
      item
        Name = 'messageWrite'
        OnExecute = MessageWriteExecute
      end
      item
        Name = 'get8087CW'
        OnExecute = Get8087CWExecute
      end
      item
        Name = 'maskFPUexceptions'
        OnExecute = MaskFPUExceptionsExecute
      end
      item
        Name = 'unmaskFPUexceptions'
        OnExecute = UnMaskFPUExceptionsExecute
      end
      item
        Name = 'testResultStartTest'
        OnExecute = testResultStartTestExecute
      end
      item
        Name = 'testResultStopTest'
        OnExecute = testResultStopTestExecute
      end
      item
        Name = 'testResultAddSuccess'
        OnExecute = testResultAddSuccess
      end
      item
        Name = 'testResultAddFailure'
        OnExecute = testResultAddFailure
      end
      item
        Name = 'testResultAddError'
        OnExecute = testResultAddError
      end
      item
        Name = 'cleanupMainDict'
        OnExecute = CleanUpMainDictExecute
      end
      item
        Name = 'cleanupSysModules'
        OnExecute = CleanUpSysModulesExecute
      end
      item
        Name = 'awakeGUI'
        OnExecute = awakeGUIExecute
      end>
    ModuleName = 'DebugIDE'
    Errors = <>
    Left = 612
    Top = 54
  end
  object SynParamCompletion: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoCaseSensitive, scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. ='
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    Images = CommandsDataModule.CodeImages
    OnExecute = SynParamCompletionExecute
    ShortCut = 0
    Editor = SynEdit
    TimerInterval = 300
    Left = 657
    Top = 8
    EndOfTokenChrW = '()[]. ='
    TriggerCharsW = '('
  end
  object InterpreterPopUp: TTBXPopupMenu
    Images = CommandsDataModule.Images
    OnPopup = InterpreterPopUpPopup
    Left = 46
    Top = 6
    object TBXPythonEngines: TTBXSubmenuItem
      Caption = 'Python Engine'
      OnPopup = TBXPythonEnginesPopup
      object TBXItem5: TTBXItem
        Action = actPythonEngineInternal
      end
      object TBXItem10: TTBXItem
        Action = actPythonEngineRemote
      end
      object TBXItem9: TTBXItem
        Action = actPythonEngineRemoteTk
      end
      object TBXItem8: TTBXItem
        Action = actPythonEngineRemoteWx
      end
      object TBXSepReinitialize: TTBXSeparatorItem
      end
      object TBXItem6: TTBXItem
        Action = actReinitialize
      end
    end
    object TBXSeparatorItem3: TTBXSeparatorItem
    end
    object TBXItem1: TTBXItem
      Action = actCleanUpNameSpace
    end
    object TBXItem2: TTBXItem
      Action = actCleanUpSysModules
    end
    object TBXSeparatorItem1: TTBXSeparatorItem
    end
    object TBXItem4: TTBXItem
      Action = actCopyHistory
    end
    object TBXItem7: TTBXItem
      Action = actClearContents
    end
    object TBXSeparatorItem2: TTBXSeparatorItem
    end
    object TBXItem3: TTBXItem
      Action = CommandsDataModule.actInterpreterEditorOptions
    end
  end
  object InterpreterActionList: TActionList
    Images = CommandsDataModule.Images
    Left = 8
    Top = 45
    object actCleanUpNameSpace: TAction
      AutoCheck = True
      Caption = 'Clean up &Namespace'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Clean up the globals namespace after run'
      OnExecute = actCleanUpNameSpaceExecute
    end
    object actCleanUpSysModules: TAction
      AutoCheck = True
      Caption = 'Clean up &sys.modules'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Clean up the globals namespace after run'
      OnExecute = actCleanUpSysModulesExecute
    end
    object actCopyHistory: TAction
      Caption = 'Copy &History'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Copy history to Clipboard'
      ImageIndex = 12
      OnExecute = actCopyHistoryExecute
    end
    object actReinitialize: TAction
      Caption = 'Reinitiali&ze Python engine'
      HelpContext = 340
      HelpType = htContext
      OnExecute = actReinitializeExecute
    end
    object actClearContents: TAction
      Caption = 'Clear &All'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Clear all interpreter output'
      ImageIndex = 14
      OnExecute = actClearContentsExecute
    end
    object actPythonEngineInternal: TAction
      AutoCheck = True
      Caption = '&Internal'
      Checked = True
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use internal Python Engine'
      OnExecute = actPythonEngineExecute
    end
    object actPythonEngineRemote: TAction
      Tag = 1
      AutoCheck = True
      Caption = '&Remote'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use a remote Python engine'
      OnExecute = actPythonEngineExecute
    end
    object actPythonEngineRemoteTk: TAction
      Tag = 2
      AutoCheck = True
      Caption = 'Remote (&Tk)'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use a remote Python engine for Tkinter applications'
      OnExecute = actPythonEngineExecute
    end
    object actPythonEngineRemoteWx: TAction
      Tag = 3
      AutoCheck = True
      Caption = 'Remote (&Wx)'
      GroupIndex = 1
      HelpContext = 340
      HelpType = htContext
      Hint = 'Use a remote Python engine for wxPython applications'
      OnExecute = actPythonEngineExecute
    end
  end
end
