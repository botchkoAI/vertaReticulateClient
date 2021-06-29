# Title     : TODO
# Objective : TODO
# Created by: sidi
# Created on: 28.06.21
# --- launch API ----
# plumb_path <- ""
r <- plumber::plumb("plumber.R")
r$run(host = "0.0.0.0", port = 8000,swagger=T)

