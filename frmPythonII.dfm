inherited PythonIIForm: TPythonIIForm
  Left = 259
  Top = 201
  HelpContext = 410
  Caption = 'Python Interpreter'
  ClientHeight = 453
  ClientWidth = 703
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000060000001A000000210000
    0021000000180000000500000000000000000000000000000000000000000000
    0000000000000000000000000000E0E0E023F0F0F0C3FCFCFCF8FFFFFFFFFBFB
    FBF8E2E2E2CD5B5B5B5000000006000000000000000000000000000000000000
    0000000000000000000000000000FDFDFDB8B3F5FFFF5DE3FFFF48D9FFFF49D3
    FFFFA0E6FFFFE2E2E2CF0000001C000000000000000000000000000000000000
    0000000000000000000000000000FFFFFFF665EDFFFF5CE8FFFF51E0FFFFD5F6
    FFFF3FD0FFFFFCFCFCF900000028000000000000000000000000000000000000
    0000000000060000001900000021FFFFFFFF62EDFFFF61EDFFFF5AE7FFFF4FDE
    FFFF44D6FFFFFFFFFFFF0000003B00000020000000170000000500000000DFDF
    DF23F0F0F0C2FCFCFCF8FFFFFFFFFFFFFFFF62EDFFFF62EDFFFFBEF7FFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBF8E2E2E2CD5A5A5A5000000006FDFD
    FDB9CCBAA5FF886A46FF7D6445FFFFFFFFFF63EDFFFF62EDFFFF62EDFFFF60EB
    FFFF56E3FFFF4BDBFFFF40D2FFFF39CBFFFF9DE4FFFFE2E2E2CF0000001DFFFF
    FFF6A17A4BFF8F6C41FF846743FFF1EEEAFF9BF4FFFF63EDFFFF62EDFFFF62ED
    FFFF5EEAFFFF54E2FFFF49D9FFFF3ED1FFFF40CCFFFFFCFCFCF900000029FFFF
    FFFFA4763DFF99713FFF8D6B42FFA38E74FFF0EEEAFFFFFFFFFFFFFFFFFFFFFF
    FFFFEDFDFFFF85EDFFFF52E0FFFF47D8FFFF3CCFFFFFFFFFFFFF0000002AFFFF
    FFF5B28247FFA2753DFF966F40FF8B6A42FF806544FF7C6345FF7C6345FF7D64
    46FFAC9C89FFEDFDFFFF5BE7FFFF4FDFFFFF50D9FFFFFBFBFBF800000021FFFF
    FFB6DCC2A2FFAD7C3FFFA0743EFF946E40FF896942FF7E6445FF7C6345FF7C63
    45FF7D6547FFFFFFFFFF61ECFFFF5CE6FFFFACEFFFFFEBEBEBC40000000B0000
    0000FFFFFFB8FFFFFFF7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC9BEB1FF7C63
    45FF7C6345FFFFFFFFFFFFFFFFFFFEFEFEF5FAFAFAB8BFBFBF25000000000000
    0000000000000000000000000000FFFFFFFF9B723FFF906C41FF856743FF7D63
    45FF7C6345FFFFFFFFFF00000029000000000000000000000000000000000000
    0000000000000000000000000000FFFFFFF5A67940FFE8DFD3FF8E6B41FF8266
    44FF7F6649FFFBFBFBF800000021000000000000000000000000000000000000
    0000000000000000000000000000FFFFFFB6D8C0A3FFA87D49FF977040FF9373
    4DFFC4B7A8FFEBEBEBC40000000B000000000000000000000000000000000000
    000000000000000000000000000000000000FFFFFFB8FFFFFFF7FFFFFFFFFEFE
    FEF5FAFAFAB9BFBFBF250000000000000000000000000000000000000000F81F
    9C41F00F9C41F00F9C41F00F9C4180019C4100009C4100009C4100009C410000
    9C4100009C4100009C4180019C41F00F9C41F00F9C41F00F9C41F81F9C41}
  Position = poDefault
  OnHelp = FormHelp
  ExplicitWidth = 719
  ExplicitHeight = 487
  DesignSize = (
    703
    453)
  PixelsPerInch = 96
  TextHeight = 13
  inherited FGPanel: TPanel
    Left = -4
    Top = -5
    Width = 700
    Height = 444
    Color = clInactiveBorder
    ExplicitLeft = -4
    ExplicitTop = -5
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
      OnMouseDown = SynEditMouseDown
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
      'import pyscripter'
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
      '            while not (self.__done or not self.__T.isAlive()):'
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
      '            import sys, thread'
      '            self.thread_id = thread.get_ident()'
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
      '        import time'
      '        #keeps GUI alive'
      '        future = self.Future(func, *args)'
      '        time.sleep(0.001)'
      '        self.thread_id = future.thread_id'
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
      '            self.debugIDE.user_call(frame, args)'
      ''
      '        def user_line(self, frame):'
      '            self.debugIDE.user_line(frame)'
      ''
      '        def user_return(self, frame, retval):'
      '            self.debugIDE.user_return(frame, retval)'
      ''
      '        def user_exception(self, frame, exc_stuff):'
      '            self.debugIDE.user_exception(frame, exc_stuff)'
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
      '            self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
      ''
      '            try:'
      '                try:'
      '                    bdb.Bdb.run(self, cmd, globals, locals)'
      '                except SystemExit, e:'
      '                    if isinstance(e.code, basestring):'
      '                        print e.code'
      '                    elif isinstance(e.code, int):'
      '                        print "Exit code: ", e.code'
      '            finally:'
      
        '                sys.stdin, sys.stdout, sys.stderr = self.saveStd' +
        'io'
      
        '                if self.debugIDE.cleanupMainDict() and (self.loc' +
        'als is globals):'
      '                    self.locals.clear()'
      '                    self.locals.update(maindictcopy)'
      '                    __import__("gc").collect()'
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
      '        self.debugger.tracecount = 0'
      '        self.debugger.currentframe = None'
      '        self.debugger.locals = self.locals'
      ''
      '        import repr'
      '        pyrepr = repr.Repr()'
      '        pyrepr.maxstring = 80'
      '        pyrepr.maxother = 80'
      '        self._repr = pyrepr.repr'
      ''
      '        self.commontypes = __import__("sets").ImmutableSet(['
      '              '#39'NoneType'#39','
      '              '#39'NotImplementedType'#39','
      '              '#39'bool'#39','
      '              '#39'buffer'#39','
      '              '#39'builtin_function_or_method'#39','
      '              '#39'code'#39','
      '              '#39'complex'#39','
      '              '#39'dict'#39','
      '              '#39'dictproxy'#39','
      '              '#39'ellipsis'#39','
      '              '#39'file'#39','
      '              '#39'float'#39','
      '              '#39'frame'#39','
      '              '#39'function'#39','
      '              '#39'generator'#39','
      '              '#39'getset_descriptor'#39','
      '              '#39'instancemethod'#39','
      '              '#39'int'#39','
      '              '#39'list'#39','
      '              '#39'long'#39','
      '              '#39'member_descriptor'#39','
      '              '#39'method-wrapper'#39','
      '              '#39'object'#39','
      '              '#39'slice'#39','
      '              '#39'str'#39','
      '              '#39'traceback'#39','
      '              '#39'tuple'#39','
      '              '#39'unicode'#39','
      '              '#39'xrange'#39'])'
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
      '        self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
      ''
      '        if not isinstance(cmd, types.CodeType):'
      '            cmd = cmd+'#39'\n'#39
      '        try:'
      '            try:'
      '                exec cmd in globals, locals'
      '            except SystemExit, e:'
      '                if isinstance(e.code, basestring):'
      '                    print e.code'
      '                elif isinstance(e.code, int):'
      '                    print "Exit code: ", e.code'
      '        finally:'
      '            sys.stdin, sys.stdout, sys.stderr = self.saveStdio'
      
        '            if self.debugIDE.cleanupMainDict() and (self.locals ' +
        'is globals):'
      '                self.locals.clear()'
      '                self.locals.update(maindictcopy)'
      '                __import__("gc").collect()'
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
      '    def objectinfo(self, ob):'
      '        res = [False, False, False, False, False]'
      '        try:'
      '            import inspect'
      
        '            if hasattr(ob, "__dict__") and isinstance(ob.__dict_' +
        '_, dict):'
      '                res[0] = True'
      '            if inspect.ismodule(ob):'
      '                res[1] = True'
      '            elif inspect.ismethod(ob):'
      '                res[2] = True'
      '            elif inspect.isfunction(ob):'
      '                res[3] = True'
      '            elif inspect.isclass(ob):'
      '                res[4] = True'
      '            return tuple(res)'
      '        except:'
      '            return tuple(res)'
      ''
      '    def saferepr(self, x):'
      '        try:'
      '            return self._repr(x)'
      '        except:'
      '            return '#39'<unprintable %s object>'#39' % type(x).__name__'
      ''
      '    def membercount(self, x):'
      '        try:'
      '            if type(x) is dict:'
      '                return len(x)'
      '            elif type(x).__name__ in self.commontypes:'
      '                return 0'
      '            else:'
      '                return len(dir(x))'
      '        except:'
      '            return 0'
      ''
      '    def _getmembers(self, ob):'
      '        result = {}'
      '        for i in dir(ob):'
      '            try:'
      '                result[i] = getattr(ob, i)'
      '            except:'
      '                result[i] = None'
      '        return result'
      ''
      '    def safegetmembers(self, x):'
      '        try:'
      '            return self._getmembers(x)'
      '        except:'
      '            return {}'
      ''
      '    def safegetmembersfullinfo(self, x):'
      '        try:'
      '            d = self._getmembers(x)'
      '            for (i,j) in d.items():'
      
        '                d[i] = (j, self.saferepr(j), self.objecttype(j),' +
        ' self.objectinfo(j), self.membercount(j))'
      '            return d'
      '        except:'
      '            return {}'
      ''
      '    def getitemsfullinfo(self, x):'
      '        try:'
      '            assert type(x) == dict'
      '            members = x.items()'
      '            d = {}'
      '            for (i,j) in members:'
      
        '                d[i] = (j, self.saferepr(j), self.objecttype(j),' +
        ' self.objectinfo(j), self.membercount(j))'
      '            return d'
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
      '        import sys'
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
      '        self.saveStdio = (sys.stdin, sys.stdout, sys.stderr)'
      '        try:'
      '            if self.debugger.currentframe:'
      
        '                exec code in self.debugger.currentframe.f_global' +
        's, self.debugger.currentframe.f_locals'
      '            else:'
      '                exec code in self.locals'
      '        except SystemExit, e:'
      '            if isinstance(e.code, basestring):'
      '                print e.code'
      '            elif isinstance(e.code, int):'
      '                print "Exit code: ", e.code'
      '        except:'
      '            self.showtraceback()'
      '        else:'
      '            if softspace(sys.stdout, 0):'
      '                print'
      '        sys.stdin, sys.stdout, sys.stderr = self.saveStdio'
      ''
      '    def evalcode(self, code):'
      '        # may raise exceptions'
      '        try:'
      '            if self.debugger.currentframe:'
      
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
      '    def setupdisplayhook(self):'
      '        if pyscripter.IDEOptions.PrettyPrintOutput:'
      '            import sys, pprint, __builtin__'
      
        '            def pphook(value, show=pprint.pprint, bltin=__builti' +
        'n__):'
      '                if value is not None:'
      '                    bltin._ = value'
      '                    show(value)'
      '            sys.displayhook = pphook'
      ''
      '_II = PythonInteractiveInterpreter(globals())'
      ''
      'sys.modules['#39'__builtin__'#39'].raw_input=_II.Win32RawInput'
      'sys.modules['#39'__builtin__'#39'].input=_II.Win32Input'
      ''
      'import os'
      'try:'
      '    sys.path.remove(os.path.dirname(sys.executable))'
      '    sys.path.remove(os.path.dirname(sys.executable))'
      'except:'
      '    pass'
      ''
      'del DebugOutput'
      'del code'
      'del PythonInteractiveInterpreter'
      'del sys'
      'del os')
    InitThreads = True
    IO = PythonIO
    PyFlags = [pfInteractive]
    Left = 557
    Top = 54
  end
  object PythonIO: TPythonInputOutput
    UnicodeIO = False
    RawOutput = False
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
    OnClose = SynCodeCompletionClose
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
  object InterpreterPopUp: TSpTBXPopupMenu
    Images = CommandsDataModule.Images
    OnPopup = InterpreterPopUpPopup
    Left = 45
    Top = 12
    object TBXPythonEngines: TSpTBXSubmenuItem
      Caption = 'Python Engine'
      LinkSubitems = PyIDEMainForm.mnPythonEngines
    end
    object TBXSeparatorItem3: TSpTBXSeparatorItem
    end
    object TBXItem1: TSpTBXItem
      Caption = 'Clean up &Namespace'
      Hint = 'Clean up the globals namespace after run'
      Action = actCleanUpNameSpace
    end
    object TBXItem2: TSpTBXItem
      Caption = 'Clean up &sys.modules'
      Hint = 'Clean up the globals namespace after run'
      Action = actCleanUpSysModules
    end
    object TBXSeparatorItem4: TSpTBXSeparatorItem
    end
    object TBXItem8: TSpTBXItem
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      Action = CommandsDataModule.actEditCut
    end
    object TBXItem6: TSpTBXItem
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      Action = CommandsDataModule.actEditCopy
    end
    object TBXItem5: TSpTBXItem
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      Action = CommandsDataModule.actEditPaste
    end
    object TBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object TBXItem4: TSpTBXItem
      Caption = 'Copy &History'
      Hint = 'Copy history to Clipboard'
      Action = actCopyHistory
    end
    object TBXItem7: TSpTBXItem
      Caption = 'Clear &All'
      Hint = 'Clear all interpreter output'
      Action = actClearContents
    end
    object TBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object TBXItem3: TSpTBXItem
      Caption = '&Interpreter Editor Options...'
      Hint = 'Set Interpreter Editor Options'
      Action = CommandsDataModule.actInterpreterEditorOptions
    end
  end
  object InterpreterActionList: TActionList
    Images = CommandsDataModule.Images
    Left = 8
    Top = 45
    object actCleanUpNameSpace: TAction
      Category = 'Interpreter'
      AutoCheck = True
      Caption = 'Clean up &Namespace'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Clean up the globals namespace after run'
      OnExecute = actCleanUpNameSpaceExecute
    end
    object actCleanUpSysModules: TAction
      Category = 'Interpreter'
      AutoCheck = True
      Caption = 'Clean up &sys.modules'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Clean up the globals namespace after run'
      OnExecute = actCleanUpSysModulesExecute
    end
    object actCopyHistory: TAction
      Category = 'Interpreter'
      Caption = 'Copy &History'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Copy history to Clipboard'
      ImageIndex = 12
      OnExecute = actCopyHistoryExecute
    end
    object actClearContents: TAction
      Category = 'Interpreter'
      Caption = 'Clear &All'
      HelpContext = 410
      HelpType = htContext
      Hint = 'Clear all interpreter output'
      ImageIndex = 14
      OnExecute = actClearContentsExecute
    end
  end
  object PyDelphiWrapper: TPyDelphiWrapper
    Engine = PythonEngine
    Module = PyscripterModule
    Left = 612
    Top = 86
  end
  object PyscripterModule: TPythonModule
    Engine = PythonEngine
    ModuleName = 'pyscripter'
    Errors = <>
    Left = 584
    Top = 87
  end
end
