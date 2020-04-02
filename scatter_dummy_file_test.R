library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

args <- commandArgs(TRUE)
analyte_data <- as.character(args[1])
params_file <- as.character(args[2])
analyte_hash_id <- as.character(args[3])
output_dir <- as.character(args[4])

print(args)

source(params_file)
results<- paste0("the path is : ", as.character(analyte_data), "\n",
                "params file is : ", as.character(params_file),"\n",
                "hash id file is : ", as.character(analyte_hash_id),
                "sourced parameter (alpha): ", alpha, "\n", 
                "output_dir: ", output_dir)

print(results)