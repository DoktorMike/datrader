myres <- tibble(H1=clusterStocks(mylist, k=10, horizon=10),
                H2=clusterStocks(mylist, k=10, horizon=11),
                H3=clusterStocks(mylist, k=10, horizon=12)
                )
