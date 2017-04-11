library(datrader)
library(quantmod)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(damodel)

mypath <- '/home/michael/Dropbox/Development/trading'
mylist <- loadExistingInstruments(mypath)

# Getting derivative parameters from a polynomial of the form
# f(x) = b0+b1*x1+b2*x²+b3*x³+...+bn*x^n
polyDeriv <- function(b) { a<-tail(b, length(b)-1); a*(1:length(a)) }

# Getting minima and maxima positions from a polynomial of the form
# f(x) = b0+b1*x1+b2*x²+b3*x³+...+bn*x^n
polyTurnpoints <- function(b) Re(polyroot(polyDeriv(b)))

prepInstr<-function(x, horizon) x %>% as.data.frame() %>% rownames_to_column("date") %>%
  mutate(date = as.Date(date)) %>% tail(horizon) %>% mutate(cnt = 1:n())

# Find the best polymomial size to use. Best here means an improvement in R² of at least
# "cutoff". This is quite shaky statistically speaking but fuck it.
FindBestPoly<-function(x, horizon=30, cutoff=0.1, debug=FALSE) {
  tmpdf <- prepInstr(x, horizon)
  mylm1<-lm(close~poly(cnt, degree = 1, raw = T), data=tmpdf)
  mylm2<-lm(close~poly(cnt, degree = 2, raw = T), data=tmpdf)
  mylm3<-lm(close~poly(cnt, degree = 3, raw = T), data=tmpdf)
  mylm4<-lm(close~poly(cnt, degree = 4, raw = T), data=tmpdf)

  a<-c(Poly1=1,
       Poly2=summary(mylm2)$r.squared/summary(mylm1)$r.squared,
       Poly3=summary(mylm3)$r.squared/summary(mylm2)$r.squared,
       Poly4=summary(mylm4)$r.squared/summary(mylm3)$r.squared)
  # browser()
  if(debug) print(a)
  b<-tail(which(a-1 > cutoff), 1)
  ifelse(length(b)>0, b, 1)
}

trend1Strategy<-function(x, horizon=30){
  tmpdf <- prepInstr(x, horizon)
  mylm <- lm(close ~ cnt, data = tmpdf)
  b <- coef(mylm)

  # Decide to invest or not
  if(b[2]>0) ret <- list(Invest=TRUE, Model=mylm)
  else ret <- list(Invest=FALSE, Model=mylm)

  return(ret)
}

# A strategy of following the trend if positive within a horizon of days
# x is given as an xts class with open, high, low, close, volume, adj.
trend2Strategy<-function(x, horizon=30){
  tmpdf <- prepInstr(x, horizon)
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
  # browser()
  degree <- 3
  tmpdf <- prepInstr(x, horizon)
  mylm <- lm(close ~ cnt + I(cnt ^ 2) + I(cnt ^ 3), data = tmpdf)
  b <- coef(mylm)
  xloc <- polyTurnpoints(b)
  bp2 <- polyDeriv(polyDeriv(b))
  # Is the last turnpoint a minima?
  tmpdf2 <- poly(tmpdf$cnt, degree = degree, raw = T) %>% as.data.frame()
  xloclast <- tail(xloc, 1)
  # Clac 2nd derivative at last turnpoint
  sndderiv<-tail(tail(xloc, 1)^(c(seq(1, degree, 1))), degree-2) * tail(bp2, degree-2) + bp2[1]
  lastturnmin <- ifelse(sndderiv<0, FALSE, TRUE)

  # Decide to invest or not
  invest<-FALSE
  if(lastturnmin){ # We have a minima function
    if(xloclast < horizon){ # Minima has happened and curve is going up
      invest <- TRUE
    }else{ # Minima will most likely happen but it's in the future and we only have a negative trend
      invest <- FALSE
    }
  }else{ # We have a maxima
    if(xloclast < horizon){ # Maxima has happened and curve is going down
      invest <- FALSE
    }else{ # Maxima will most likely happen but it's in the future and we only have a positive trend
      invest <- TRUE
    }
  }
  ret <- list(Invest=invest, Model=mylm)
  return(ret)
}

x<-mylist$NFLX; h<-60; res<-trend3Strategy(x, h); chartSeries(tail(x, h)); plotPrediction2(res$Model, interval = "pred"); polyTurnpoints(coef(res$Model)); res$Invest
