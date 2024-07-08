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
data(cancer)
head(cancer)

## -----------------------------------------------------------------------------
data(standard)
print(standard)

## -----------------------------------------------------------------------------
cancer2 <- subset(cancer, grepl("50-54|55-59|60-64|65-69", Age))
head(cancer2)

## -----------------------------------------------------------------------------
fit <- stan_rw(cancer2, time = Year, group = Age,
               refresh = 0,# silences some printing	       
               iter = 2e3,
               chains = 2) # for demo speed only. Use the default chains = 4

## ----fig.height = 4.5, fig.width = 6.5----------------------------------------
plot(fit, 
     facet = TRUE,          # plot small multiples
     facet_scales = "free", # y-axes vary across plots
     base_size = 10,      # control text size 
     size = 0,            # removes crude rates from the plots
     scale = 100e3        # plot rates per 100,000
     )

## ----fig.height = 4.5, fig.width = 6.5----------------------------------------
fit_apc <- apc(fit)
plot(fit_apc, 
     base_size = 10,
     cum = TRUE)

## -----------------------------------------------------------------------------
fit_sr <- standardize(fit,
                        label = standard$age,
                        standard_pop = standard$standard_pop)

## ----fig.height = 4, fig.width = 5--------------------------------------------
# load ggplot2 to enable additional plot customization
library(ggplot2)
plot(fit_sr, scale = 100e3, base_size = 10) + 
  labs(title = "US age-standardized cancer incidence per 100,000",
  subtitle = "Ages 50-69")

## -----------------------------------------------------------------------------
print(fit_sr, scale = 100e3)

## -----------------------------------------------------------------------------
fit_sr_pc <- apc(fit_sr)

## -----------------------------------------------------------------------------
plot(fit_sr_pc, cum = TRUE)

