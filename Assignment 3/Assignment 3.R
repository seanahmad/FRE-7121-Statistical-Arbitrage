library(quantmod)

getCloses <- function(sym) {
  ohlc <- getSymbols(sym, from="2009-01-01", to="2011-01-01",
                     auto.assign=FALSE, return.class="zoo")
  Cl(ohlc)
}

olsHedgeRatio <- function(p, q) {
  m <- lm(p ~ q)
  coef(m)[2]
}

tlsHedgeRatio <- function(p, q) {
  r <- princomp( ~ p + q)
  r$loadings[1,1] / r$loadings[2,1]
}


closes <- merge(AAPL=getCloses("AAPL"),
                GOOG=getCloses("GOOG"), all=FALSE)
with(closes, {
  cat("OLS for AAPL vs. GOOG =", olsHedgeRatio(AAPL,GOOG), "\n")
  cat("OLS for GOOG vs. AAPL =", olsHedgeRatio(GOOG,AAPL), "\n")
  cat("TLS for AAPLvs. GOOG =", tlsHedgeRatio(AAPL,GOOG), "\n")
  cat("TLS for GOOG vs. AAPL =", tlsHedgeRatio(GOOG,AAPL), "\n")
})


closes <- merge(IBM=getCloses("IBM"),
                SPY=getCloses("SPY"), all=FALSE)
with(closes, {
  cat("OLS for IBM vs. SPY =", olsHedgeRatio(IBM,SPY), "\n")
  cat("OLS for SPY vs. IBM =", olsHedgeRatio(SPY,IBM), "\n")
  cat("TLS for IBM vs. SPY =", tlsHedgeRatio(IBM,SPY), "\n")
  cat("TLS for SPY vs. IBM =", tlsHedgeRatio(SPY,IBM), "\n")
})


closes <- merge(DIA=getCloses("DIA"),
                SPY=getCloses("SPY"), all=FALSE)
with(closes, {
  cat("OLS for DIA vs. SPY =", olsHedgeRatio(DIA,SPY), "\n")
  cat("OLS for SPY vs. DIA =", olsHedgeRatio(SPY,DIA), "\n")
  cat("TLS for DIA vs. SPY =", tlsHedgeRatio(DIA,SPY), "\n")
  cat("TLS for SPY vs. DIA =", tlsHedgeRatio(SPY,DIA), "\n")
})



