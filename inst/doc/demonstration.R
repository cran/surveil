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
library(surveil)
library(dplyr)
library(ggplot2)
library(knitr)

## ----eval = FALSE-------------------------------------------------------------
#   # here the code is displayed only, not run; for package development reasons
#  options(mc.cores = parallel::detectCores())

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
fit <- stan_rw(tx.msa,
               time = Year,
	       group = Race,
	       iter = 1500,
	       chains = 2  #, for speed only; use default chains=4
	       # cores = 4 # for multi-core processing
               )

## -----------------------------------------------------------------------------
samples <- fit$samples
class(samples)

## ----fig.width = 4, fig.height = 3.5------------------------------------------
rstan::stan_mcse(samples)

## ----fig.width = 4, fig.height = 3.5------------------------------------------
rstan::stan_rhat(samples)

## ----fig.width = 4.5, fig.height = 3.5----------------------------------------
plot(fit, scale = 100e3, base_size = 11)

## ----fig.width = 7, fig.height = 3.5------------------------------------------
fig <- plot(fit, scale = 100e3, base_size = 11, size = 0)
fig +
  theme(legend.position = "right") +
  labs(title = "CRC incidence per 100,000",
       subtitle = "Texas MSAs, 50-79 y.o.")

## ----fig.width = 4.5, fig.height = 3.5----------------------------------------
plot(fit, scale = 100e3, base_size = 11, style = "lines")

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

