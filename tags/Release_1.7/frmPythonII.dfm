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
      Gutter.Gradient = True
      Options = [eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoTabIndent, eoTabsToSpaces]
      TabWidth = 4
      WantTabs = True
      WordWrap = True
      OnCommandProcessed = SynEditCommandProcessed
      OnProcessCommand = SynEditProcessCommand
      OnProcessUserCommand = SynEditProcessUserCommand
      OnReplaceText = SynEditReplaceText
      OnPaintTransient = SynEditPaintTransient
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
      '    class IDEDebugger(__import__('#39'bdb'#39').Bdb):'
      '        DebugIDE = __import__("DebugIDE")'
      '        def do_clear(self, arg):'
      '            import string'
      '            numberlist = string.split(arg)'
      '            for i in numberlist:'
      '                self.clear_bpbynumber(i)'
      '        def stop_here(self, frame):'
      '            import bdb'
      '            if not self.InitStepIn:'
      '                self.InitStepIn = True'
      '                self.set_continue()'
      '                return 0'
      '            return bdb.Bdb.stop_here(self, frame)'
      '        def user_call(self, frame, args):'
      '            self.InIDEDebug = True'
      '            self.CurrentFrame = frame'
      '            try:'
      '                self.DebugIDE.user_call(frame, args)'
      '            finally:'
      '                self.InIDEDebug = False'
      '        def user_line(self, frame):'
      '            self.InIDEDebug = True'
      '            self.CurrentFrame = frame'
      '            try:'
      '                self.DebugIDE.user_line(frame)'
      '            finally:'
      '                self.InIDEDebug = False'
      '        def user_return(self, frame, retval):'
      '            self.InIDEDebug = True'
      '            self.CurrentFrame = frame'
      '            try:'
      '                self.DebugIDE.user_return(frame, retval)'
      '            finally:'
      '                self.InIDEDebug = False'
      '        def user_exception(self, frame, exc_stuff):'
      '            self.InIDEDebug = True'
      '            self.CurrentFrame = frame'
      '            try:'
      '                self.DebugIDE.user_exception(frame, exc_stuff)'
      '            finally:'
      '                self.InIDEDebug = False'
      '        def trace_dispatch(self, frame, event, arg):'
      '            self.tracecount += 1'
      
        '            if self.tracecount == 30:  #yield processing every 3' +
        '0 steps'
      '                self.tracecount = 0'
      '                self.DebugIDE.user_yield()'
      
        '                if self.quitting: raise __import__('#39'bdb'#39').BdbQui' +
        't'
      
        '            return __import__('#39'bdb'#39').Bdb.trace_dispatch(self, fr' +
        'ame, event, arg)'
      '        def run_nodebug(self, cmd, globals=None, locals=None):'
      '            import types'
      '            import sys'
      '            import __main__'
      '            maindictcopy = __main__.__dict__.copy()'
      '            sysmodulescopy = sys.modules.copy()'
      '            if globals is None:'
      '                globals = __main__.__dict__'
      '            if locals is None:'
      '                locals = globals'
      '            if not isinstance(cmd, types.CodeType):'
      '                cmd = cmd+'#39'\n'#39
      '            try:'
      '                try:'
      '                    exec cmd in globals, locals'
      '                except SystemExit:'
      '                    pass'
      '            finally:'
      '                if self.CleanupMaindict:'
      '                    __main__.__dict__.clear()'
      '                    __main__.__dict__.update(maindictcopy)'
      '                if self.CleanupSysModules:'
      '                    sys.modules.clear()'
      '                    sys.modules.update(sysmodulescopy)'
      ''
      '        def run(self, cmd, globals=None, locals=None):'
      '            import bdb'
      '            import sys'
      '            import __main__'
      '            maindictcopy = __main__.__dict__.copy()'
      '            sysmodulescopy = sys.modules.copy()'
      '            if globals is None:'
      '                globals = __main__.__dict__'
      '            if locals is None:'
      '                locals = globals'
      '            try:'
      '                try:'
      '                    bdb.Bdb.run(self, cmd, globals, locals)'
      '                except SystemExit:'
      '                    pass'
      '            finally:'
      '                if self.CleanupMaindict:'
      '                    __main__.__dict__.clear()'
      '                    __main__.__dict__.update(maindictcopy)'
      '                if self.CleanupSysModules:'
      '                    sys.modules.clear()'
      '                    sys.modules.update(sysmodulescopy)'
      ''
      '    class IDETestResult(__import__('#39'unittest'#39').TestResult):'
      '        DebugIDE = __import__("DebugIDE")'
      ''
      '        def startTest(self, test):'
      
        '            __import__('#39'unittest'#39').TestResult.startTest(self, te' +
        'st)'
      '            self.DebugIDE.testResultStartTest(test)'
      ''
      '        def stopTest(self, test):'
      
        '            __import__('#39'unittest'#39').TestResult.stopTest(self, tes' +
        't)'
      '            self.DebugIDE.testResultStopTest(test)'
      ''
      '        def addError(self, test, err):'
      
        '            __import__('#39'unittest'#39').TestResult.addError(self, tes' +
        't, err)'
      
        '            self.DebugIDE.testResultAddError(test, self._exc_inf' +
        'o_to_string(err, test))'
      ''
      '        def addFailure(self, test, err):'
      
        '            __import__('#39'unittest'#39').TestResult.addFailure(self, t' +
        'est, err)'
      
        '            self.DebugIDE.testResultAddFailure(test, self._exc_i' +
        'nfo_to_string(err, test))'
      ''
      '        def addSuccess(self, test):'
      '            self.DebugIDE.testResultAddSuccess(test)'
      
        '            __import__('#39'unittest'#39').TestResult.addSuccess(self, t' +
        'est)'
      ''
      '    def __init__(self, locals = None, globals = None):'
      '        if locals is None: locals = __main__.__dict__'
      '        if globals is None: globals = locals'
      '        self.globals = globals'
      '        code.InteractiveInterpreter.__init__(self, locals)'
      '        self.debugger = self.IDEDebugger()'
      '        self.debugger.InitStepIn = False'
      '        self.debugger.InIDEDebug = False'
      '        self.debugger.tracecount = 0'
      '        self.debugger.CleanupMaindict = True'
      '        self.debugger.CleanupSysModules = True'
      ''
      '        import repr'
      '        saferepr = repr.Repr()'
      '        saferepr.maxstring = 80'
      '        saferepr.maxother = 80'
      '        self.saferepr = saferepr.repr'
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
      '            import DebugIDE'
      '            def __init__(self):'
      '                self._buffer = '#39#39
      ''
      '            def write(self, txt):'
      '                self._buffer += txt'
      '                if txt.endswith('#39'\n'#39'):'
      
        '                    DebugIDE.statusWrite('#39'BRM: '#39' + self._buffer.' +
        'strip())'
      '                    self._buffer = '#39#39
      ''
      '        class WarningsLogger:'
      
        '            """ File like logger that uses the Editor statusbar ' +
        'as output """'
      '            import DebugIDE'
      '            def __init__(self):'
      '                self._buffer = '#39#39
      ''
      '            def write(self, txt):'
      '                self._buffer += txt'
      '                if txt.endswith('#39'\n'#39'):'
      
        '                    DebugIDE.messageWrite('#39'  '#39' + self._buffer.st' +
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
      '        '
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
      
        '                exec code in self.debugger.CurrentFrame.f_global' +
        's, self.debugger.CurrentFrame.f_locals'
      '            else:'
      '                exec code in self.globals, self.locals'
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
      
        '                return eval(code, self.debugger.CurrentFrame.f_g' +
        'lobals, self.debugger.CurrentFrame.f_locals)'
      '            else:'
      '                return eval(code, self.globals, self.locals)'
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
      '        import DebugIDE'
      '        import sys'
      '        try:'
      '            sys.stdout.flush()'
      '            sys.stderr.flush()'
      '        except:'
      '            pass'
      '        if prompt is None: prompt = ""'
      '        ret=DebugIDE.InputBox('#39'PyScripter'#39', prompt,"")'
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
    Left = 558
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
      end>
    ModuleName = 'DebugIDE'
    Errors = <>
    Left = 613
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
    Left = 48
    Top = 8
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
    object TBXSeparatorItem2: TTBXSeparatorItem
    end
    object TBXItem3: TTBXItem
      Action = CommandsDataModule.actInterpreterEditorOptions
    end
  end
  object InterpreterActionList: TActionList
    Images = CommandsDataModule.Images
    Left = 48
    Top = 40
    object actCleanUpNameSpace: TAction
      AutoCheck = True
      Caption = 'Clean up &Namespace'
      Hint = 'Clean up the globals namespace after run'
      OnExecute = actCleanUpNameSpaceExecute
    end
    object actCleanUpSysModules: TAction
      AutoCheck = True
      Caption = 'Clean up &sys.modules'
      Hint = 'Clean up the globals namespace after run'
      OnExecute = actCleanUpSysModulesExecute
    end
    object actCopyHistory: TAction
      Caption = 'Copy &History'
      Hint = 'Copy history to Clipboard'
      ImageIndex = 12
      OnExecute = actCopyHistoryExecute
    end
  end
end
