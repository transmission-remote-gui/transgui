if %~1==x86 set choco_args=--x86

choco install %choco_args% lazarus

c:\lazarus\lazbuild.exe transgui.lpi
