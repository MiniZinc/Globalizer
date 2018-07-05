
@echo off

set prefix=.
set exe=%prefix%/minizinc-globalizer.exe

set command=%1

if "%command%"==""      GOTO all
if "%command%"=="all"   GOTO all
if "%command%"=="clean" GOTO clean

echo Unrecognised argument. Exiting.
goto end

:all
echo Building and copying:
stack --local-bin-path "%prefix%" build --copy-bins
if %ERRORLEVEL% GEQ 1 goto error
goto end

:clean
echo Deleting %exe%.
del /Q "%exe%"
echo Deleting .stack-work.
rmdir /S /Q .stack-work
goto end

:error
echo Build failed. Exiting.
goto end

:end


