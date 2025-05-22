library(data.table)

# read data
dt = fread("./one_input.csv")

# process data
dt[, new_col := 1L]

# write data
fwrite(dt, file = "./one_output.csv")
