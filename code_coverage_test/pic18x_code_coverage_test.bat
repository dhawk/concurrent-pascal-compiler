CodeCoverage.exe -e "..\bin\test_cpc_pic18x.exe" -m "..\bin\test_cpc_pic18x.map" -uf pic18x_dcov_units.lst -spf pic18x_dcov_paths.lst -od "results\" -lt -html
cd results
del pic18x_code_coverage_summary.html
rename CodeCoverage_summary.html pic18x_code_coverage_summary.html
pic18x_code_coverage_summary.html
cd ..
