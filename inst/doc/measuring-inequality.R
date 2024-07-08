## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  results = "hold", 
  collapse = TRUE, 
  eval = TRUE,
  fig.pos = 'h', 
  fig.align = 'center'
)

## -----------------------------------------------------------------------------
library(surveil)
data(msa)
msa2 <- aggregate(cbind(Count, Population) ~ Year + Race, data = msa, FUN = sum)
head(msa2)

## -----------------------------------------------------------------------------
# refresh = 0 will silence some printing
fit <- stan_rw(msa2, time = Year, group = Race, iter = 1e3, refresh = 0)

## -----------------------------------------------------------------------------
unique(msa2$Race)

## -----------------------------------------------------------------------------
gd <- group_diff(fit, target = "Black or African American", reference = "White")
print(gd, scale = 100e3)

## ----fig.width = 7, fig.height = 2.5------------------------------------------
plot(gd, scale = 100e3)

## ----eval = FALSE-------------------------------------------------------------
#  # figure not shown
#  plot(gd, scale = 100e3, PAR = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  fit_list <- list(group_a = fit_sr_a, group_b = fit_sr_b)

## ----eval = FALSE-------------------------------------------------------------
#  diff <- group_diff(fit_list)

## -----------------------------------------------------------------------------
Ts <- theil(fit)
print(Ts)

## ----fig.width = 4, fig.height = 3--------------------------------------------
plot(Ts)

