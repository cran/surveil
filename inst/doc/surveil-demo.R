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

## ----message=FALSE, warning=FALSE, eval=T-------------------------------------
library(surveil)
library(ggplot2)
library(knitr)

## ----eval=T-------------------------------------------------------------------
head(msa) |>
  kable(booktabs = TRUE, 
        caption = "Glimpse of colorectal cancer incidence data (CDC Wonder)") 

## ----message = FALSE, warn = FALSE, eval = T----------------------------------
msa2 <- aggregate(cbind(Count, Population) ~ Year + Race, data = msa, FUN = sum)

## ----eval = T-----------------------------------------------------------------
head(msa2) |>
  kable(booktabs = TRUE, 
        caption = "Glimpse of aggregated Texas metropolitan CRC cases, by race and year")

## -----------------------------------------------------------------------------
fit <- stan_rw(msa2, time = Year, group = Race, iter = 1e3)

## -----------------------------------------------------------------------------
print(fit, scale = 100e3)

## -----------------------------------------------------------------------------
head(fit$summary)

## ----fig.width = 4.75, fig.height = 3.5---------------------------------------
plot(fit, scale = 100e3)

## ----fig.width = 7, fig.height = 3.5------------------------------------------
fig <- plot(fit, scale = 100e3, base_size = 11, size = 0)
fig +
  theme(legend.position = "right") +
  labs(title = "CRC incidence per 100,000",
       subtitle = "Texas MSAs, 50-79 y.o.")

## ----fig.width = 4.5, fig.height = 3.5----------------------------------------
plot(fit, scale = 100e3, base_size = 11, style = "lines")

## -----------------------------------------------------------------------------
fit_pc <- apc(fit)

## -----------------------------------------------------------------------------
head(fit_pc$apc)

## -----------------------------------------------------------------------------
head(fit_pc$cpc)

## ----fig.width = 7, fig.height = 3.5------------------------------------------
plot(fit_pc, cumulative = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  fit <- stan_rw(msa2, time = Year, group = Race, cor = TRUE)

