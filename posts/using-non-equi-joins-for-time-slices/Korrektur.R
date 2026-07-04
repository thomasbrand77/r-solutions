

calcTSL = function(dt,
                   intervals,
                   cols_dt = c("startDate", "endDate"),
                   cols_intervals = c("startDate", "endDate"),
                   id_dt = "client") {
  
  cols_cp = paste("cp",cols_dt, sep = ".")
  cols_i  = paste("i",cols_intervals, sep = ".")
  cols_to_delete = c(cols_cp, cols_i)
  
  # prepare intervals: generate join-columns
  intervals[,(cols_i) := lapply(.SD, copy), .SDcols = cols_intervals]
  setkeyv(intervals, cols_i)
  
  # repare dt: generate join-columns
  dt[,(cols_cp) := lapply(.SD, copy), .SDcols = cols_dt]
  
  
  result = dt[
    intervals,
    on = c(paste0(cols_cp[2]," > ",cols_intervals[1]), 
           paste0(cols_cp[1]," < ",cols_intervals[2]))
  ][, # programming on data.table
    ':='(start = pmax(start, i.start),
         end   = pmin(end,   i.end)),
    env = list(start   = cols_dt[1],
               end     = cols_dt[2],
               i.start = cols_i[1],
               i.end   = cols_i[2])
  ][, # delete unnecessary columns
    (cols_to_delete) := NULL
  ]
  
  # sort result by id + interval
  setkeyv(result, c(id_dt,cols_dt))
  
  return(result)
}

