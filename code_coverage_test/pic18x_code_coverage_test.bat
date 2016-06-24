cd pic18x_results
del /Q *.*
cd ..
CodeCoverage.exe -e "..\bin\test_cpc_pic18x.exe" -m "..\bin\test_cpc_pic18x.map" -uf pic18x_dcov_units.lst -spf pic18x_dcov_paths.lst -od "pic18x_results\" -lt -html
cd pic18x_results
CodeCoverage_summary.html
cd ..
