@echo off

set EMACS_HOME=%UserProfile%

:: if exist %EMACS_HOME%\.emacs.d (
::     echo ~/.emacs.d exists, please add install script by hand
:: )

if not exist %EMACS_HOME%\.emacs.d (
    mkdir %EMACS_HOME%\.emacs.d
)

echo (load-file "%~dp0/init.el")    >  %EMACS_HOME%\.emacs.d\init.el
echo (maimacs-init "%~dp0")         >> %EMACS_HOME%\.emacs.d\init.el
