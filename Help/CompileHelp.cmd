del PyScripter.chm
py -3.12 -m sphinx -M clean source build
py -3.12 -m sphinx -M htmlhelp source build
copy /b build\htmlhelp\Pyscripter.hhp+source\Aliases.hpp build\htmlhelp\Pyscripter.hhp
pushd build\htmlhelp
"c:\Program Files (x86)\HTML Help Workshop\hhc.exe" pyscripter.hhp
del ..\..\..\PyScripter.chm
move PyScripter.chm ..\..\..
popd
