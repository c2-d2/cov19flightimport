# libraries
library(tidyverse)
library(doParallel)
pkgs <- c('doParallel', 'foreach')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

# functions
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

# load dataset
load(file="~/Desktop/Corona/epidata.Rdata")
df <- df %>% mutate(Wuhan_airtravel_1=Wuhan_airtravel+1) # add a small constant to travel volume

# fit the data
mdl <- glm(Cases_lm ~ 1+offset( log(Wuhan_airtravel_1) ), data = df, family=poisson( link="log" ) )
new_data <- df

# compute the prediction intervals
PI<-boot_pi(mdl, new_data, 50000, 0.95)

# visualisation
df$fit<-PI$pred
df$lwr<-PI$lower
df$upr<-PI$upper
ggplot(df)+
                geom_point(aes(x=Wuhan_airtravel,y=Cases_lm),size=3, alpha=1)+
                geom_line(aes(x=Wuhan_airtravel,y=fit),size=1, colour="grey80")+
                geom_line(aes(x=Wuhan_airtravel,y=lwr),size=1, colour="darkred", linetype=2)+
                geom_line(aes(x=Wuhan_airtravel,y=upr),size=1, colour="grey80", linetype=2)+
                geom_text(aes(x=Wuhan_airtravel,y=Cases_lm, label=Country), hjust=0.5 , vjust=-0.8, size=3.8)+
                ylab("Reported cases")+
                xlab("Average number passanger/day")
