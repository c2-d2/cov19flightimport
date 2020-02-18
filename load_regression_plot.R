# libraries
library(tidyverse)
library(doParallel)
pkgs <- c('doParallel', 'foreach')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

# functions
# inspired by: https://stackoverflow.com/questions/17922637/prediction-intervals-for-poisson-regression-on-r
boot_pi <- function(model, pdata, n, p) {
                odata <- model$data
                lp <- (1 - p) / 2
                up <- 1 - lp
                set.seed(2020)
                seeds <- round(runif(n, 1, 1000), 0)
                boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
                                set.seed(seeds[i])
                                bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
                                bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
                                rpois(length(bpred), lambda = bpred)
                }
                boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
                return(data.frame(pred = predict(model, newdata = pdata, type = "response"), lower = boot_ci[, 1], upper = boot_ci[, 2]))
}

# load data for cases + flight volume
load(file="~/Desktop/Corona/epidata.Rdata") # WRONG DATA FOR SWEDEN
#
# load ghs data
df_ghs <- read_csv(file="~/Desktop/Corona/ghs_index.csv",col_names = F) %>% 
                rename(Country=X1,ghs=X2) %>% 
                left_join( tibble(Country=c("United States",
                                            "United Kingdom",
                                            "United Arab Emirates",
                                            "Sweden",
                                            "New Zealand",
                                            "South Korea"),
                                  Country_new=c("US",
                                                "UK",
                                                "UAE",
                                                "Sweeden",
                                                "New Zeland",
                                                "Rep  Korea") ) ) %>% 
                mutate( Country=ifelse(is.na(Country_new),Country,Country_new) ) %>% select(-Country_new)
# bind datasets 
df_all0 <- full_join( df_ghs,df, by="Country" ) %>% mutate( Cases_lm=replace_na(Cases_lm,replace=0),
                                                           Wuhan_airtravel=replace_na(Wuhan_airtravel,replace=0)) 
# settings
min_airtravel <- 1.5 
ghs_quantile <- 0.75
# setting air travel volume to min_airtravel
# giving binary label according to GHS index
ghs_limit <- df_all0$ghs %>% quantile(ghs_quantile)  
df_all <- df_all0 %>% 
                mutate( Wuhan_airtravel_gamma=ifelse(Wuhan_airtravel<3,
                                                     min_airtravel,
                                                     Wuhan_airtravel) ) %>% 
                mutate( ghs_high=as.numeric(ghs>ghs_limit) )



# fit the data
df_fit <- df_all %>% filter(ghs_high==1) # rnows=49
mdl <- glm(Cases_lm ~ 1+offset( log(Wuhan_airtravel_gamma) ), data = df_fit, family=poisson( link="log" ) )
new_data <- df_all
# compute the prediction intervals
PI<-boot_pi(mdl, new_data, 50000, 0.95)

# visualisation
df_all$fit<-PI$pred
df_all$lwr<-PI$lower
df_all$upr<-PI$upper
(p1 <- ggplot(df_all)+
                geom_vline(xintercept = 0, color="lightgrey") +
                geom_point(aes(x=Wuhan_airtravel,y=Cases_lm,color=as.factor(ghs_high)),
                           size=2, alpha=1, show.legend = F )+
                geom_line(aes(x=Wuhan_airtravel,y=fit),size=1, colour="black")+
                stat_smooth(aes(x=Wuhan_airtravel,y=lwr),size=1, colour="grey80", linetype=2,
                            method = lm, formula = y ~ poly(x, 2), se = FALSE)+
                stat_smooth(aes(x=Wuhan_airtravel,y=upr),size=1, colour="grey80", linetype=2,
                            method = lm, formula = y ~ poly(x, 2), se = FALSE)+
                labs(x="Daily air travel volume (number of passengers per day)",
                     y="Number of imported cases of 2019-nCoV"))

