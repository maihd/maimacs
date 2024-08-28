@echo off

tools\luajit.exe -O3 tools\fnlfmt.bin --fix %1
