library(plyr)

args <- commandArgs(TRUE)
files <- as.character(args[1])

dir.create("final_result")

df = do.call("rbind", lapply(unlist(strsplit(files, " ")), function(f){read.csv(f)}))

write.csv(x=df, file = "final_result/all_csv.csv", row.names = FALSE)