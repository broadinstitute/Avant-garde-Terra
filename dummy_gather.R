library(plyr)

dir.create("final_result")

args <- commandArgs(TRUE)
print(args)

params_file <- as.character(args[1])
avg_results_path <- as.character(args[2])
MetaData_Analytes_path <- as.character(args[3])
Transition_Locator <- as.character(args[4])
MetaData_Replicate <- as.character(args[5])
output_path <- as.character(args[6])

print(params_file)
print(avg_results_path)
print(MetaData_Analytes_path)
print(Transition_Locator)
print(MetaData_Replicate)
print(output_path)