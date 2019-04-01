myres <- tibble(H1=clusterStocks(mylist, k=10, horizon=10),
                H2=clusterStocks(mylist, k=10, horizon=11),
                H3=clusterStocks(mylist, k=10, horizon=12)
                )


library(dplyr)
library(ggplot2)

tibble(x=rnorm(10)) %>% mutate(y=x+5) %>% ggplot(aes(x=x, y=y)) + geom_point()



