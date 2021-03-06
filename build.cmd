@echo off
setlocal enableextensions enabledelayedexpansion
PUSHD %~dp0


IF NOT exist .paket\paket.exe (
	echo installing paket
	dotnet tool install Paket --version 5.242.2 --tool-path .paket
)

if NOT exist paket.lock (
    echo No paket.lock found, running paket install.
    .paket\paket.exe install
)

if NOT exist packages\build\fake-cli\tools\netcoreapp2.1\any\fake-cli.dll (
	echo running paket restore
	.paket\paket.exe restore
)

dotnet "packages\build\fake-cli\tools\netcoreapp2.1\any\fake-cli.dll" build %* 

