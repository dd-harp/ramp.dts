## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ramp.dts)
#devtools::load_all()

## -----------------------------------------------------------------------------
enbrq <- dts_setup(MYZname = "ENBRQ_dts", Xname = "trace")

