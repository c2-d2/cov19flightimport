# Generate sample of proportion of observed zeros among lowly connected 
# countries
pred_dist_0pct = sapply(1:10000, function(i){
                y_pred = rpois(32, 1.5*0.0325) 
                return(mean(y_pred == 0))
})

# Posterior predictive distribution of proportions of zero reported cases 
# among countries with 1.5 travel volume
# Change bins
df <- tibble(post_prob_zeros=pred_dist_0pct)
df %>% ggplot( aes(x=post_prob_zeros) ) +
                geom_histogram(aes(y=..density..),binwidth=0.03,fill="grey") +
                geom_vline(xintercept = 0.969, color="black") +
                scale_x_continuous(breaks=seq(from=0,to=1,by=0.05),
                                   labels = scales::percent_format(accuracy = 1)) +
                theme_minimal() +
                theme(axis.text.y = element_blank()) +
                labs(x="Proportion of zero counts",
                     y="Density")
ggsave(filename = "~/Desktop/Corona/Out/hist_zeroprop.pdf",width = 10*1.2,height = 6*1.2,unit = "cm" )
df %>% summarise( mean(post_prob_zeros>=0.969) )
