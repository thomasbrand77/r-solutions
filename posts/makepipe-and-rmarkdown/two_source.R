library(data.table)

# read data
dt = fread("./one_output.csv")

# process data
dt[, newer_col := 2L]

# write data
fwrite(dt, file = "./two_output.csv")
