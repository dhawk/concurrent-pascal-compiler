cd core_results
del /Q *.*
cd ..
CodeCoverage.exe -e "..\bin\test_cpc_core.exe" -m "..\bin\test_cpc_core.map" -uf core_units.lst -spf core_paths.lst -od "core_results\" -lt -html
cd core_results
CodeCoverage_summary.html
cd ..
