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
cancer <- cancer[grep("40-44|45-49|50-54|55-59|60-64", cancer$Age),]
standard <- standard[10:14,]
head(cancer)

## -----------------------------------------------------------------------------
print(standard)

## -----------------------------------------------------------------------------
fit <- stan_rw(cancer,
               time = Year,
	       group = Age,
	       iter = 1500,
	       chains = 2  #, for speed only; use default chains=4	       
	       )

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
  subtitle = "Ages 40-64")

## -----------------------------------------------------------------------------
print(fit_sr)

## -----------------------------------------------------------------------------
cancer_a <- cancer 
cancer_b <- cancer
## set the case count equal to 0.75 times that of group A
cancer_b$Count <- round(cancer_a$Count * 0.75) +  
  rpois(n = nrow(cancer), lambda = cancer_a$Count * 0.1) # adds a little noise to the data
## set the population at risk to 0.6 times that of group A
cancer_b$Population <- round(cancer_a$Population * 0.6) + 
  rpois(n = nrow(cancer), lambda = cancer_a$Population * 0.1)

## -----------------------------------------------------------------------------
fit_b <- stan_rw(cancer_b, time = Year, group = Age, 
                 refresh = 0, # silences some printing
		 iter = 1500,
	         chains = 2  # for speed only; use default chains=4
	         # cores = 4 # for multi-core processing
		 )

## -----------------------------------------------------------------------------
fit_sr_b <- standardize(fit_b,
                        label = standard$age,
                        standard_pop = standard$standard_pop)

## -----------------------------------------------------------------------------
fit_sr_list <- list(B = fit_sr_b, A = fit_sr)
ineq <- group_diff(fit_sr_list)

## ----fig.width = 7, fig.height = 2.5------------------------------------------
plot(ineq, base_size = 10)

## -----------------------------------------------------------------------------
print(ineq)

