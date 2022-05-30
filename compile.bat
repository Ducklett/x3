@echo off
node --trace-uncaught compiler
echo exited with code %errorlevel%
