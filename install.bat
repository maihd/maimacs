@echo off

set EMACS_HOME=%UserProfile%

set MAIMACS_DIR=%~dp0
for /f "delims=" %%A in ('
    PowerShell -NoP "$(Get-Location).Path.Replace('\','/')"
') do set MAIMACS_DIR=%%A

echo %MAIMACS_DIR%

if exist %EMACS_HOME%\.emacs.d (
    echo ~/.emacs.d exists, please add install script by hand
)

if not exist %EMACS_HOME%\.emacs.d (
    mkdir %EMACS_HOME%.emacs.d
)

echo (load-file "%MAIMACS_DIR%/init.el")    >  %EMACS_HOME%\.emacs.d\init.el
echo (maimacs-init "%MAIMACS_DIR%")         >> %EMACS_HOME%\.emacs.d\init.el
