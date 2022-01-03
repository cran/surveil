## ---- include = FALSE---------------------------------------------------------
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
## packages required for the analysis
library(surveil)
library(dplyr)
library(ggplot2)
theme_set(theme_classic())
## for the vignette
library(knitr)

## ----eval=T-------------------------------------------------------------------
head(msa) %>%
  kable(booktabs = TRUE, 
        caption = "Glimpse of colorectal cancer incidence data (CDC Wonder)") 

## ----message = FALSE, warn = FALSE, eval = T----------------------------------
tx.msa <- msa %>%
  group_by(Year, Race) %>%
  summarise(Count = sum(Count),
            Population = sum(Population))

## ----eval = T-----------------------------------------------------------------
head(tx.msa) %>%
  kable(booktabs = TRUE, 
        caption = "Glimpse of aggregated Texas metropolitan CRC cases, by race and year")

## -----------------------------------------------------------------------------
fit <- stan_rw(tx.msa, time = Year, group = Race)

## -----------------------------------------------------------------------------
samples <- fit$samples
class(samples)

## ----fig.width = 4, fig.height = 3.5------------------------------------------
rstan::stan_mcse(samples)

## ----fig.width = 4, fig.height = 3.5------------------------------------------
rstan::stan_rhat(samples)

## ----fig.width = 4, fig.height = 3.5------------------------------------------
plot(fit, scale = 100e3)

## ----fig.width = 4, fig.height = 3.5------------------------------------------
fig <- plot(fit, scale = 100e3)
fig + theme_bw() + theme(legend.position = "bottom")

## ----fig.width = 4, fig.height = 3.5------------------------------------------
plot(fit, scale = 100e3, style = "lines")

## -----------------------------------------------------------------------------
gd <- group_diff(fit, target = "Black or African American", reference = "White")
print(gd, scale = 100e3)

## ----fig.width = 7, fig.height = 3--------------------------------------------
plot(gd, scale = 100e3)

## ----fig.width = 7, fig.height = 3--------------------------------------------
plot(gd, scale = 100e3, PAR = FALSE, style = "lines")

## -----------------------------------------------------------------------------
Ts <- theil(fit)
print(Ts)

## ----fig.width = 4, fig.height = 3--------------------------------------------
plot(Ts)

