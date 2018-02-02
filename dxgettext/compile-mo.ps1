Get-ChildItem -Path ..\locale\ -recurse -file *.po | ForEach {
  $moName = [io.path]::ChangeExtension($_.FullName, 'mo')
  .\msgfmt.exe $_.FullName -o $moName
}