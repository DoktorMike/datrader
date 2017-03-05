library(datrader)
library(quantmod)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(damodel)

mypath <- '/home/michael/Dropbox/Development/trading'
mylist <- loadExistingInstruments(mypath)

# Analyse one stock

# A strategy of following the trend if positive within a horizon of days
# x is given as an xts class with open, high, low, close, volume, adj.
trendStrategy<-function(x, horizon=30){
  tmpdf <- x %>% as.data.frame() %>% rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>% tail(horizon) %>% mutate(cnt = 1:n())
  mylm <- lm(close ~ cnt + I(cnt ^ 2), data = tmpdf)
  b <- coef(mylm)
  xloc <- -b[2] / (2 * b[3])
  minima <- ifelse(b[3]<0, FALSE, TRUE)

  # Decide to invest or not
  invest<-FALSE
  if(minima){ # We have a minima function
    if(xloc < horizon){ # Minima has happened and curve is going up
      invest <- TRUE
    }else{ # Minima will most likely happen but it's in the future and we only have a negative trend
      invest <- FALSE
    }
  }else{ # We have a maxima
    if(xloc < horizon){ # Maxima has happened and curve is going down
      invest <- FALSE
    }else{ # Maxima will most likely happen but it's in the future and we only have a positive trend
      invest <- TRUE
    }
  }
  ret <- list(Invest=invest, Model=mylm)
  return(ret)
}

trend3Strategy<-function(x, horizon=30){
  tmpdf <- x %>% as.data.frame() %>% rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>% tail(horizon) %>% mutate(cnt = 1:n())
  mylm <- lm(close ~ cnt + I(cnt ^ 2) + I(cnt ^ 3), data = tmpdf)
  b <- coef(mylm)
  xloc <- -b[2] / (2 * b[3])
  minima <- ifelse(b[3]<0, FALSE, TRUE)

  # Decide to invest or not
  invest<-FALSE
  if(minima){ # We have a minima function
    if(xloc < horizon){ # Minima has happened and curve is going up
      invest <- TRUE
    }else{ # Minima will most likely happen but it's in the future and we only have a negative trend
      invest <- FALSE
    }
  }else{ # We have a maxima
    if(xloc < horizon){ # Maxima has happened and curve is going down
      invest <- FALSE
    }else{ # Maxima will most likely happen but it's in the future and we only have a positive trend
      invest <- TRUE
    }
  }
  ret <- list(Invest=invest, Model=mylm)
  return(ret)
}

tmpdf<-mylist$ABB.ST %>% as.data.frame() %>% rownames_to_column("date") %>%
  mutate(date=as.Date(date)) %>% tail(40) %>% mutate(cnt=1:n())
mylm<-lm(close~cnt+I(cnt^2), data=tmpdf)
b<-coef(mylm)
xloc <- -b[2]/(2*b[3])

if(minima){ # We have a minima function
  if(xloc < horizon){ # Minima has happened and curve is going up

  }else{ # Minima will most likely happen but it's in the future and we only have a negative trend

  }
}else{ # We have a maxima
  if(xloc < horizon){ # Maxima has happened and curve is going down

  }else{ # Maxima will most likely happen but it's in the future and we only have a positive trend

  }
}
