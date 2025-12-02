param(
	[Parameter(Mandatory=$true)]
	[string]
	$Source
)

$ErrorActionPreference = "Stop"
$PSNativeCommandUseErrorActionPreference = $true

.\build\src\kl_compiler $Source --emit-llvm -o out.ll
llvm-as out.ll
llc out.bc -filetype=obj -march=x86-64
link msvcrt.lib out.obj User32.lib legacy_stdio_definitions.lib

