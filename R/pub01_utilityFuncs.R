



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  check TS,TSF,TSFR object -----------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' check.colnames
#' 
#' check colnames of an object with data.frame class, eg. TS, TSF, TSFR, ... Return error when the elements of coltest is not contained in the colnames of data
#' @param data a dataframe or matrix,with some colnames
#' @param coltest a character vector containing the colnames to be checked
#' @return NULL
#' @author Ruifei.Yin
#' @export
#' @examples 
#' df <- data.frame(a=1,b=2)
#' check.colnames(df,c("a","b"))
#' check.colnames(df,c("a","c"))
check.colnames <- function (data,coltest) {
  if(length(dim(data)) != 2L){
    stop("Here need an object of class dataframe or matrix or other object with 2 dims !")
  }
  colnames <- colnames(data)
  if (!all(coltest %in% colnames)){
    stop(paste('the data must contain the colnames:', paste(coltest,collapse=",")))
  }
}

#' @rdname check.colnames
#' @export
usualcols <- function(){
  c("date","stockID","date_end","periodrtn","factorscore","sector","wgt","rptDate")
}
#' @rdname check.colnames
#' @export
is_usualcols <- function(cols){
  re <- cols %in% usualcols() | substr(cols,1,6)=="prdrtn"
}
#' @rdname check.colnames
#' @export
#' @examples 
#' df <- data.frame(a=1,b=2,c=3,date=9,factorscore=0)
#' guess_factorNames(df,no_factorname = "c",is_factorname = "factorscore")
guess_factorNames <- function(df,no_factorname = NULL,is_factorname="factorscore",silence=FALSE){
  factorNames <- names(df)[!is_usualcols(names(df)) & !names(df)%in%no_factorname | names(df)%in%is_factorname]
  if(!silence){
    cat(paste("The guessed factorNames are: ",paste(factorNames,collapse = ","),".\n",sep = ""))
  }
  return(factorNames)
}


#' @rdname check.colnames
#' @export
check.TS <- function(TS){
  coltest <- c("date","stockID")
  check.colnames(TS,coltest)
}
#' @rdname check.colnames
#' @export
check.rptTS <- function(rptTS){
  coltest <- c("rptDate","stockID")
  check.colnames(rptTS, coltest)
  check.rptDate(rptTS$rptDate)
}
#' @rdname check.colnames
#' @export
check.TSF <- function(TSF){
  coltest <- c("date","stockID","factorscore")
  check.colnames(TSF,coltest)
}
#' @rdname check.colnames
#' @export
check.TSFR <- function(TSFR){
  coltest <- c("date","stockID","factorscore","periodrtn","date_end")
  check.colnames(TSFR,coltest)
}

#' @rdname check.colnames
#' @export
check.Port <- function(port){
  coltest <- c("date","stockID","wgt")
  check.colnames(port,coltest)
}
#' @rdname check.colnames
#' @export
check.SP <- function(SP){
  coltest <- c("stockID","begT","endT")
  check.colnames(SP,coltest)
}
#' @rdname check.colnames
#' @export
check.TSS <- function(TSS){
  coltest <- c("date","stockID","sector")
  check.colnames(TSS,coltest)
}

#' check.name_exist
#' 
#' check if a object have names
#' @param obj any object to be checked
#' @export
#' @author Ruifei.Yin
check.name_exist <- function(obj){
  nm <- names(obj)
  if(is.null(nm) || any(is.na(nm))){
    stop("The object must have none-NA names!")
  }
}






# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------  Performance analyzing functions   --------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' hitRatio
#'
#' get the hit ratio of the return series.
#' 
#' @param rtn a vector or a matrix(or a matrix-like object,eg.xts,zoo,timeSeries,..each column gives a rtn series of an asset)
#' @param satisfied a numeric, indicating how much return could be called a "hit"
#' @return the hit ratio, a matrix with dims 1*NCOL(rtn)
#' @author Ruifei.Yin
#' @export
#' @examples 
#' rtn <- matrix(runif(100,0,0.05),50,2)
#' hit <- hitRatio(rtn,0.02)
hitRatio <- function(rtn,satisfied=0){
  rtn <- as.matrix(rtn)
  hit <- apply(rtn,2,function(x,sat){length(x[x>sat])/length(x)},sat=satisfied)
  dim(hit) <- c(1, NCOL(rtn))
  colnames(hit) <- colnames(rtn)
  rownames(hit) <- "hit_ratio"
  return(hit) 
}



#' Calculates weighted returns for a portfolio of assets
#' 
#' These are wraped functions of \code{\link[PerformanceAnalytics]{Return.portfolio}} and \code{\link[PerformanceAnalytics]{Return.rebalancing}} in package \code{PerformanceAnalytics}, with some bug fixed and more result returned.
#' @param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param weights a time series or a vector containing asset weights. Note that the sum of weights is not required to be 1. If the sum is smaller to 1,the remnant weight is set to 'cash',and the portfolio is treated as an unfull-position portfolio;if the sum is larger than 1, the extra weight is set as liability, and the portfolio is treated as a leveraged portfolio. See detail in examples.
#' @param rebFreq Default "na", alternatively "day", "weeks", "month", "quarter", and "year". to specify calendar-period rebalancing supported by \code{\link[xts]{endpoints}}.
#' @param geometric a logical. generate geometric (TRUE) or simple (FALSE) returns, default TRUE
#' @param fee.buy a single-row matrix or a vector containing buying fee of each assets. It also could be a numeric scalar, which means all the assets have the same fee. 
#' @param fee.sell a single-row matrix or a vector containing selling fee of each assets. It also could be a numeric scalar, which means all the assets have the same fee.
#' @param output a vector of character string, with default of c("rtn","turnover","wgt_EOP","wgt_BOP","contribution","weights","rebtrade")
#' @param warning.wgtsum a logical. wheather put out a warning when sum of the weights is not equal to 1 ?
#' @return a list with items:
#'   \itemize{ 
#'   \item rtn: a time series of the portfolio return.
#'   \item turnover: a time series of turnover.
#'   \item wgt_EOP: End of Period (BOP) Weight for each asset, with dims of \code{c(nrow(R), ncol(weights)+1)} .
#'   \item wgt_BOP: Beginning of Period (BOP) Weight for each asset, with dims of \code{c(nrow(R), ncol(weights)+1)} .
#'   \item contritution: The per period contribution to portfolio return of each asset, with dims of \code{c(nrow(R), ncol(weights)+1)}
#'   \item weights: weights
#'   \item rebtrade: a time series of the rebalancing trading of each asset, with dims of \code{c(nrow(weights), ncol(weights)+1)}, a  positive element means buying,and vice versa.
#'   }
#' @author Ruifei.yin
#' @export
#' @examples
#' R <- zoo(matrix(rnorm(12,0,0.1),4,3),Sys.Date()+1:4)
#' colnames(R) <- letters[1:3]
#' w1 <- (c(0.2,0.3,0.5))
#' w2 <- (c(0.2,0.3,0.1))
#' w3 <- (c(0.2,0.3,-0.5))
#' w4 <- (c(1,0.5,0.5))
#' Return.backtesting(R,w1) # full position portfolio
#' Return.backtesting(R,w2) # unfull position portfolio
#' Return.backtesting(R,w3) # long-short portfolio
#' Return.backtesting(R,w4) # leveraged portfolio
#' # examples from \code{\link[PerformanceAnalytics]{Return.portfolio}}
#' require(PerformanceAnalytics)
#' data(edhec)
#' re <- Return.backtesting(edhec["1997",1:5], rebFreq="quarters") 
#' # with a weights object
#' data(weights) # rebalance at the beginning of the year to various weights through time
#' chart.StackedBar(weights)
#' x <- Return.backtesting(edhec["2000::",1:11], weights=weights)
#' chart.CumReturns(x$returns)
#' chart.StackedBar(x$BOP.Weight)
#' chart.StackedBar(x$BOP.Value) 
Return.backtesting <- function(R, 
                               weights=NULL,
                               rebFreq=NA,
                               fee.buy=0, fee.sell=0,
                               output=c("rtn","turnover","wgt_EOP","wgt_BOP","contribution","weights","rebtrade"),
                               geometric = TRUE, 
                               warning.wgtsum=TRUE){
  
  # --- check the retrun data 
  R = PerformanceAnalytics::checkData(R, method = "xts")
  if (!nrow(R) >= 1) {
    stop("no data passed for R(eturns)")
  }
  if (any(is.na(R))) {
    warning("NA's detected, filling NA's with zeros")
    R[is.na(R)] <- 0
  }
  
  # ---- check the weights data
  rebalance_on = rebFreq
  freq = xts::periodicity(R)
  switch(freq$scale, seconds = {
    stop("Use a returns series of daily frequency or higher.")
  }, minute = {
    stop("Use a returns series of daily frequency or higher.")
  }, hourly = {
    stop("Use a returns series of daily frequency or higher.")
  }, daily = {
    time_unit = "day"
  }, weekly = {
    time_unit = "week"
  }, monthly = {
    time_unit = "month"
  }, quarterly = {
    time_unit = "quarter"
  }, yearly = {
    time_unit = "year"
  })
  if (time_unit == "quarter") {
    start_date = zoo::as.yearqtr(seq(as.Date(zoo::index(R)[1]), length = 2,by = paste("-3", "month"))[2])
  }  else {
    start_date = seq(as.Date(zoo::index(R)[1]), length = 2, by = paste("-1",   time_unit))[2]
  }
  if (is.null(weights)) {
    weights = rep(1/NCOL(R), NCOL(R))
    warning("weighting is null, it will be set to equal-weights")
  }
  if (is.vector(weights)||nrow(weights)==1) {
    if (is.na(rebalance_on)) { # no rebalance
      weights = xts(matrix(weights, nrow = 1), order.by = as.Date(start_date))
    } else { # rebalance on rebFreq
      weight_dates = c(as.Date(start_date), zoo::index(R[xts::endpoints(R, on = rebalance_on)]))
      weights = xts(matrix(rep(weights, length(weight_dates)),ncol = NCOL(R), byrow = TRUE), order.by = as.Date(weight_dates))
    }
    colnames(weights) = colnames(R)
  } else { # rebalance on weights series
    weights = PerformanceAnalytics::checkData(weights, method = "xts")
    if (NCOL(R) != NCOL(weights)) {
      if (NCOL(R) > NCOL(weights)) {
        R = R[, 1:NCOL(weights)]
        warning("number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.")
      } else {
        stop("number of assets is greater than number of columns in returns object")
      }
    }
  }
  NCOLs <- ncol(weights) 
  
  # --- check the time windows of R and weights
  if (as.Date(end(R)) < trday.nearby(as.Date(start(weights)),by = 1) ) {
    stop(paste("The last date of return series", as.Date(end(R)),"occurs before beginning of first rebalancing period",  trday.nearby(as.Date(start(weights)),by = 1) ))
  }
  # if (as.Date(start(R)) < as.Date(zoo::index(weights[1, ]))) {
  #   R <- R[paste0(as.Date(zoo::index(weights[1, ])) + 1, "/")]
  # }
  # if (as.Date(start(R)) > trday.nearby(as.Date(start(weights)),1)) {
  #   warning(paste("Return series start on", as.Date(start(R)), ", which is after the first rebalancing period", trday.nearby(as.Date(start(weights)),1),". The first rebalancing point will be supposed to",as.Date(start(R))-1))
  # }
  if (as.Date(end(R)) < as.Date(end(weights))+1 ) {
    warning(paste("Return series end on", as.Date(end(R)), ", which is before the last rebalancing period", trday.nearby(as.Date(end(weights)),by = 1),". The last rebalancing period will be ignored"))
  }  
    
  # --- check the fee data
  if(length(fee.buy)==1){
    fee.buy <- rep(fee.buy,NCOLs)    
  } 
  if(length(fee.sell)==1){
    fee.sell <- rep(fee.sell,NCOLs)
  }  
  if(is.vector(fee.buy)){
    fee.buy <- matrix(fee.buy, nrow = 1)
  }
  if(is.vector(fee.sell)){
    fee.sell <- matrix(fee.sell, nrow = 1)
  }
  if(!all(dim(fee.buy)==c(1,NCOLs))|!all(dim(fee.sell)==c(1,NCOLs))){
    stop("The fee.buy or fee.sell has wrong dims!")
  }
  
  # ---- deal with the weights not sum as 1 by adding a column 'cash' . 
  R.cash <- xts(rep(0,nrow(R)),zoo::index(R))
  colnames(R.cash) <- "cash"
  R <- cbind(R,R.cash)
  weights.cash <- matrix(1-rowSums(weights),ncol = 1)
  if(warning.wgtsum && any(weights.cash < -0.001)){
    warning("The sum of the weights is larger than 1. The extra weight is set to 'cash' as liability. The portfolio is treated as a leveraged portfolio!")
  }
  if(warning.wgtsum && any(weights.cash > 0.001)){
    warning("The sum of the weights is smaller than 1. The remnant weight is set to 'cash'. The portfolio is treated as an unfull position portfolio!")
  }
  weights <- cbind(weights,weights.cash)
  colnames(weights)[NCOLs+1] <- "cash"
  fee.buy <- cbind(fee.buy,0)
  fee.sell <- cbind(fee.sell,0)
  
  # --- loop rebalancing 
  for (row in 1:nrow(weights)) {
    weight_i <- weights[row, ]
    from = as.Date(zoo::index(weight_i)) + 1
    if (row == nrow(weights)) {
      to = as.Date(end(R))
    } else {
      to = as.Date(zoo::index(weights[(row + 1), ]))
    }
    tmpR <- R[paste(from, to, sep = "/"), ]
    if (nrow(tmpR) >= 1) {
      list_i = Return.portfolio_yrf(tmpR, weights = weight_i, geometric=geometric)
      rtn_i <- xts::try.xts(list_i[["rtn"]])
      wgt_EOP_i <- list_i[["wgt_EOP"]] 
      wgt_BOP_i <- list_i[["wgt_BOP"]]
      contribution_i <- list_i[["contribution"]]
      # --- get the trading seri
      if(row == 1){
        trade_i <- xts(zoo::coredata(xts::first(wgt_BOP_i)),zoo::index(weight_i))   # trade_i <- matrix(0,1,NCOLs)  # which one is better??
        last_wgt_EOP <- zoo::coredata(xts::last(wgt_EOP_i))
      } else {  
        trade_i <- xts(zoo::coredata(xts::first(wgt_BOP_i)) - last_wgt_EOP, zoo::index(weight_i)) 
        last_wgt_EOP <- zoo::coredata(xts::last(wgt_EOP_i))
      } 
      # --- cut fees from rtn due to reblancing (cut rtn of the next day of rebalancing) 
      buyfee_i <- trade_i[,trade_i>=0,drop=FALSE] %*% t(fee.buy[,trade_i>=0,drop=FALSE])
      sellfee_i <- -trade_i[,trade_i<0,drop=FALSE] %*% t(fee.sell[,trade_i<0,drop=FALSE]) 
      zoo::coredata(rtn_i[1,]) <- zoo::coredata(rtn_i[1,])-buyfee_i-sellfee_i
      # --- stacking
      if (row == 1) {
        rtn = rtn_i
        wgt_EOP <- wgt_EOP_i
        wgt_BOP <- wgt_BOP_i
        contribution <- contribution_i
        rebtrade <- trade_i
      } else {
        rtn = rbind(rtn, rtn_i)
        wgt_EOP <- rbind(wgt_EOP,wgt_EOP_i)
        wgt_BOP <- rbind(wgt_BOP,wgt_BOP_i)
        contribution <- rbind(contribution,contribution_i)
        rebtrade <- rbind(rebtrade,trade_i)
      }
    } else {
      warning(paste("There is no return data during the rebalance period of",as.Date(zoo::index(weight_i)),"!"))
    }
  }
  
  
  # --- the turnover seri
  turnover.buy <- apply(rebtrade, MARGIN=1, function(x){sum(x[x>0])})
  turnover.sell <- apply(rebtrade, MARGIN=1, function(x){sum(x[x<0])})
  turnover.net <- turnover.buy+turnover.sell
  turnover.avg <- (turnover.buy-turnover.sell)/2
  turnover_seri <- cbind(turnover.buy, turnover.sell, turnover.net, turnover.avg)
  colnames(turnover_seri) <- c("buy","sell","net","avg")
  
  # --- result building
  rtn <- xts::reclass(rtn, R)
  wgt_EOP <- xts::reclass(wgt_EOP,R)
  wgt_BOP <- xts::reclass(wgt_BOP,R)
  rebtrade <- xts::reclass(rebtrade,weights)
  turnover_seri <- xts::reclass(turnover_seri,weights)
  result <- list(rtn=rtn,
                 turnover=turnover_seri,
                 wgt_EOP=wgt_EOP,
                 wgt_BOP=wgt_BOP,
                 contribution=contribution,
                 weights=weights,
                 rebtrade=rebtrade)
  return(result[output])
}

  
Return.portfolio_yrf <- function (R, weights, geometric=TRUE) {    
  # here, weights is one row timeseries, and R is a timeseries.
  if (!geometric) {
    rtn = R %*% as.vector(weights)
    wgt_EOP <- kronecker(matrix(1,nrow(R),1),weights)
    wgt_BOP <- wgt_EOP
    contribution <- wgt_BOP*R     
  }  else {
    wealthindex.assets = cumprod(1 + R)    
    wealthindex.weighted <- wealthindex.assets*kronecker(matrix(1,nrow(R),1),weights) # value_EOP    
    wealthindex = as.matrix(rowSums(wealthindex.weighted,na.rm=TRUE),ncol=1)
    wgt_EOP  <- wealthindex.weighted/kronecker(matrix(1,1,ncol(R)),wealthindex)   # wgt_EOP 
    wgt_BOP <- xts(rbind(zoo::coredata(weights),zoo::coredata(wgt_EOP)[-nrow(R),]),zoo::index(R))    # wgt_BOP
    contribution <- wgt_BOP*R     # contribution
    wealthindex <- xts(wealthindex,zoo::index(R))     
    wealthindex <- rbind(xts(1,start(wealthindex)-1),wealthindex)
    rtn <- Returns(wealthindex,trim=TRUE)  # rtn
  }
  rownames(rtn) <- NULL
  colnames(rtn) <- "portfolioReturns"
  rtn <- xts::reclass(rtn, R)  
  # wgt_EOP <- wgt_EOP[,-ncol(wgt_EOP)] # remove the col of cash
  result <- list(rtn = rtn,
                 wgt_EOP = wgt_EOP,
                 wgt_BOP = wgt_BOP,
                 contribution = contribution)
  return(result)
}






#' periodicity_Ndays
#' 
#' Estimate the periodicity of a time-series-like object by calculating the mean time between observations in days.
#' @param x a timeseries object or a Date vector
#' @return a numeric
#' @export
#' @seealso \link[xts]{periodicity}
periodicity_Ndays <- function(x) {
  if("Date" %in% class(x)){
    idx <- x
  } else{
    idx <- zoo::index(x)
    if(class(idx)!="Date"){
      stop("The index must be class of Date")
    }
  }
  p_Ndays <- mean(diff(idx),na.rm = TRUE)
  if (is.na(p_Ndays)) {
    warning("can not calculate periodicity of 1 observation. Return NA.")
  }
  return(as.numeric(p_Ndays))
}


#' scale_esti
#'
#' Estimate the scale of a time-series-like object
#' @return a numeric
#' @export
scale_esti <- function(R){
  re <- 365/periodicity_Ndays(R)
  return(re)
}


#' annualized functions
#' 
#' calculate an annualized return, stddev, turnover.
#' @rdname annulized
#' @name annulized
#' @aliases Turnover.annualized
#' @param R a return, stddev, turnover series, an object of class timeSeries,zoo or xts
#' @param scale number of periods in a year (daily scale = 252, monthly scale = 12, quarterly scale = 4)
#' @return a vector or scalar depending on the dim of seri
#' @author Ruifei.Yin
#' @export
#' @examples
#' #-- turnover.annulized
#' seri <- zoo(runif(30,0,1),seq(Sys.Date(),by="month",length.out=30))
#' re <- Turnover.annualized(seri)
#' seri <- zoo(matrix(runif(60,0,1),30,2),seq(Sys.Date(),by="month",length.out=30))
#' re <- Turnover.annualized(seri)
Turnover.annualized <- function(R, scale=scale_esti(R)){
  subFun <- function(x){
    result <- mean(x,na.rm = TRUE) * scale
    return(result)
  }
  if(is.null(dim(R))){
    re <- subFun(R)
    return(re)
  }else{
    re = apply(R, 2, subFun)
    dim(re) = c(1, NCOL(R))
    colnames(re) = colnames(R)
    rownames(re) = "ann_turnover"
    return(re)
  }
}



#' @rdname annulized
#' @export
#' @param geometric a logical. generate geometric (TRUE) or simple (FALSE) returns, default TRUE
#' @examples 
#' #-- return.annulized
#' #- monthly
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+(1:100)*30)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' Return.annualized(rtn) 
#' PerformanceAnalytics::Return.annualized(rtn) 
#' #- 10day
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+(1:100)*10)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' Return.annualized(rtn) # right
#' PerformanceAnalytics::Return.annualized(rtn) # wrong!
#' scale_esti <- 365/periodicity_Ndays(rtn)
#' PerformanceAnalytics::Return.annualized(rtn,scale = scale_esti) # right
Return.annualized <- function (R, scale=scale_esti(R),geometric = TRUE) {
  subFun <- function(x, geometric){
    x = na.omit(x)
    if (geometric) {
      result = prod(1 + x)^(scale/length(x)) - 1
    }
    else {
      result = mean(x) * scale
    }
    result
  }
  if(is.null(dim(R))){
    re <- subFun(R, geometric)
    return(re)
  }else{
    re = apply(R, 2, subFun, geometric = geometric)
    dim(re) = c(1, NCOL(R))
    colnames(re) = colnames(R)
    rownames(re) = "ann_rtn"
    return(re)
  }
  
  # simplyfied :
  # re <- PerformanceAnalytics::Return.annualized(R = R,scale = scale,geometric = geometric)
  # return(re)
  
}

#' @rdname annulized
#' @export
StdDev.annualized <- function(R, scale=scale_esti(R)){
  subFun <- function(x){
    result <- sqrt(scale) * sd(x, na.rm = TRUE)
    return(result)
  }
  if(is.null(dim(R))){
    re <- subFun(R)
    return(re)
  }else{
    re = apply(R, 2, subFun)
    dim(re) = c(1, NCOL(R))
    colnames(re) = colnames(R)
    rownames(re) = "ann_sd"
    return(re)
  }
  
  # simplyfied :
  # re <- PerformanceAnalytics::StdDev.annualized(x = R,scale = scale)
  # return(re)
}


#' @rdname annulized
#' @export
IC.annualized <- function(R, scale=scale_esti(R)){
  subFun <- function(x){
    result <- sqrt(scale) * mean(x, na.rm = TRUE)
    return(result)
  }
  if(is.null(dim(R))){
    re <- subFun(R)
    return(re)
  }else{
    re = apply(R, 2, subFun)
    dim(re) = c(1, NCOL(R))
    colnames(re) = colnames(R)
    rownames(re) = "ann_IC"
    return(re)
  }
}

#' @rdname annulized
#' @export
SharpeRatio.annualized <- function (R, Rf = 0, scale=scale_esti(R), geometric = TRUE) {
  subFun <- function(x, geometric){
    x = na.omit(x)
    if (geometric) {
      rtn = prod(1 + x)^(scale/length(x)) - 1
    }
    else {
      rtn = mean(x) * scale
    }
    stdev <- sqrt(scale) * sd(x, na.rm = TRUE)
    result <- (rtn-Rf)/stdev
    return(result)
  }
  if(is.null(dim(R))){
    re <- subFun(R, geometric)
    return(re)
  }else{
    re = apply(R, 2, subFun, geometric = geometric)
    dim(re) = c(1, NCOL(R))
    colnames(re) = colnames(R)
    rownames(re) = "ann_Sharpe"
    return(re)
  }
}



#' @rdname annulized
#' @export
Table.Annualized <- function (R, scale=scale_esti(R), Rf = 0, geometric = TRUE, digits = 4){
  y = PerformanceAnalytics::checkData(R)

  columns = ncol(y)
  columnnames = colnames(y)
  
  for (column in 1:columns) {
    z = c(Return.annualized(y[, column, drop = FALSE],scale=scale, geometric = geometric), 
          StdDev.annualized(y[, column, drop = FALSE],scale=scale), 
          SharpeRatio.annualized(y[,column, drop = FALSE],scale=scale, Rf = Rf, geometric = geometric))
    znames = c("ann_rtn",
               "ann_sd",
               "ann_Sharpe")
    if (column == 1) {
      resultingtable = data.frame(Value = z, row.names = znames)
    }
    else {
      nextcolumn = data.frame(Value = z, row.names = znames)
      resultingtable = cbind(resultingtable, nextcolumn)
    }
  }
  colnames(resultingtable) = columnnames
  ans = base::round(resultingtable, digits)
  return(ans)
}




#' Returns
#' 
#' Compute financial returns from prices or indexes.
#' 
#' This is a wrapped function of function \code{\link[timeSeries]{returns}} in package \code{timeSeries} which is used for timeSeries objects
#' @param prices price or index series, an object of class timeSeries,zoo or xts
#' @param geometric use geometric (TRUE) or simple (FALSE) returns, default TRUE
#' @param na.rm a logical value. Should NAs be removed? By Default TRUE.
#' @param trim a logical value. Should the time series be trimmed? By Default TRUE.
#' @return the returns series of the same class of \code{prices}
#' @seealso \code{\link{WealthIndex}},\code{\link[timeSeries]{returns}}
#' @export
#' @author Ruifei.Yin
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' prices <- WealthIndex(rtn)
#' rtn2 <- Returns(prices,trim=FALSE) # Is rtn2 the same as rtn?
Returns <- function(prices,geometric=TRUE,na.rm = TRUE, trim = TRUE){
  if(geometric) method <- "simple" else method <- "compound"
  dm <- dim(prices)
  prices <- PerformanceAnalytics::checkData(prices,method="zoo")
  rtn <- as.zoo(timeSeries::returns(timeSeries::as.timeSeries(prices),method=method,na.rm=na.rm,trim=trim))
  zoo::index(rtn) <- if(trim) zoo::index(prices)[-1] else zoo::index(prices)
  if(is.null(dm)){
    dim(rtn) <- NULL
  }   
  rtn <- xts::reclass(rtn,prices)
  return(rtn)
}


#' WealthIndex
#' 
#' Compute cumulated financial series, e.g. prices or indexes, from financial returns.
#' 
#' This is a wrapped function of function \code{\link[timeSeries]{cumulated}} in package \code{timeSeries} which is used for timeSeries objects
#' @param rtn return series, an object of class timeSeries,zoo or xts
#' @param geometric get geometric (TRUE) or simple (FALSE) returns, default TRUE
#' @return the wealth index series of the same class of \code{rtn}
#' @seealso \code{\link{Returns}},\code{\link[timeSeries]{cumulated}}
#' @export
#' @author Ruifei.Yin
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' prices <- WealthIndex(rtn)
WealthIndex <- function(rtn,geometric=TRUE){
  if(geometric) method <- "simple" else method <- "compound"
  dm <- dim(rtn)
  rtn_ <- PerformanceAnalytics::checkData(rtn,method="zoo")
  wealthIndex <- as.zoo(timeSeries::cumulated(timeSeries::as.timeSeries(rtn_),method=method))
  zoo::index(wealthIndex) <- zoo::index(rtn_)
  if(is.null(dm)){
    dim(wealthIndex) <- NULL
  }  
  wealthIndex <- xts::reclass(wealthIndex,rtn)
  return(wealthIndex)
}


#' rtn.summary
#' 
#' get the summary infomation of the rtn series,including Annualized Return,Annualized Std Dev,Annualized Sharpe,HitRatio,Worst Drawdown
#' @param rtn an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param hitFreq,indicating the interval when computing the hitRatio of rtn. An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param hitSatisfied a numeric, indicating how much return could be called a "hit".See \code{\link{hitRatio}} for detail.
#' @param Rf risk free rate, in same period as your returns
#' @return a matrix, giving the summary infomation of the rtn series,including Annualized Return,Annualized Std Dev,Annualized Sharpe,HitRatio,Worst Drawdown 
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' rtn.summary(rtn)
rtn.summary <- function(rtn,hitFreq="day",hitSatisfied=0,Rf=0){
  rtn <- as.xts(rtn)   
  annual <- as.matrix(Table.Annualized(rtn,Rf=Rf))
  rtn.aggr <- aggregate(rtn,as.Date(cut(zoo::index(rtn),hitFreq)),PerformanceAnalytics::Return.cumulative)
  hit <- hitRatio(rtn.aggr,satisfied=hitSatisfied)
  dim(hit) <- c(1, NCOL(rtn))
  colnames(hit) <- colnames(rtn)
  rownames(hit) <- if(hitFreq=="day") "hit_ratio" else paste("hit_ratio (of ",hitFreq,")",sep="")
  maxDD <- PerformanceAnalytics::maxDrawdown(rtn)
  dim(maxDD) <- c(1, NCOL(rtn))
  colnames(maxDD) <- colnames(rtn)
  rownames(maxDD) <- "max_drawdown"
  result <- rbind(annual,hit,maxDD) 
  return(result)
}





#' rtn.stats
#' 
#' Returns a basic set of statistics of the rtn series. It's a wraped function of \code{\link[PerformanceAnalytics]{table.Stats}} in package \code{PerformanceAnalytics} adding by the hitRatio.
#' @param rtn an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param hitSatisfied a numeric, indicating how much return could be called a "hit".See \code{\link{hitRatio}} for detail.
#' @return a matrix  
#' @seealso \code{\link[PerformanceAnalytics]{table.Stats}}
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' rtn.stats(rtn)
rtn.stats <- function(rtn,hitSatisfied=0){
  rtn <- as.xts(rtn)
  result.PApkg <- as.matrix(PerformanceAnalytics::table.Stats(rtn))
  hitRatio <- hitRatio(rtn,hitSatisfied)
  result <- rbind(hitRatio,result.PApkg)
  return(result)
}

#' rtn.periods
#' 
#' get a return of different periods of the rtn series(including overall cumulative and annualized return). 
#' @param rtn an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param freq An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param from starting dates, a vector of class Date,timeDate or character.See \code{\link[timeSeries]{fapply}} for detail.
#' @param to  ending dates  a vector of class Date,timeDate or character.See \code{\link[timeSeries]{fapply}} for detail.
#' @return a matrix  
#' @seealso \code{\link[timeSeries]{fapply}}
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' rtn.periods(rtn,"month")
#' rtn.periods(rtn,from=c("2010-02-03","2010-03-04"),to=c("2010-03-09","2010-03-30"))
rtn.periods <- function(rtn,freq="year",from,to) {  
  if(missing(from)||missing(to)){
    from <- unique(cut.Date2(zoo::index(rtn),freq,lab.side="begin"))
    to <- unique(cut.Date2(zoo::index(rtn),freq,lab.side="end"))
  }
  rtn <- as.zoo(rtn)
  # ---- periods cumulative rtn
  table.periods <- timeSeries::fapply(timeSeries::as.timeSeries(rtn),from,to,FUN=PerformanceAnalytics::Return.cumulative)
  table.periods <- as.matrix(table.periods)
  rownames(table.periods) <- paste(from,to,sep=" ~ ")  
  # ---- overall cumulative rtn and annnualized rtn
  table.overall <- PerformanceAnalytics::Return.cumulative(rtn)
  table.annual <- Return.annualized(rtn)  
  result <- rbind(table.periods,table.overall,table.annual)
  return(result)
}


#' rtn.lastperiods
#' 
#' get a return of different last periods of the rtn series(including overall cumulativereturn). 
#' @param rtn an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param periods  a list, with elements of class "Period",eg.list(months(1),months(3)), giving the last periods.
#' @return a matrix  
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' rtn.lastperiods(rtn,list(months(3),months(6),years(1)))
rtn.lastperiods <- function(rtn,periods=list(months(1),months(3),months(6),years(1),years(3),years(5))){
  rtn <- xts::try.xts(rtn)
  to <- rep(end(rtn),length(periods))
  from <- vector()
  for(ii in 1:length(periods)){
    from[ii] <- to[ii]-periods[[ii]]
  }
  class(from) <- class(to)
  re <- timeSeries::fapply(timeSeries::as.timeSeries(rtn),from,to,FUN=PerformanceAnalytics::Return.cumulative)
  re <- as.matrix(re)
  period.char <- plyr::laply(periods,function(x){substring(capture.output(x),5)})
  rownames(re) <- paste("Last",period.char)
  # ---- add overall cumulative rtn
  overall <- PerformanceAnalytics::Return.cumulative(rtn)
  rownames(overall) <- "Overall"
  re <- rbind(re,overall)
  return(re)
}


#' aggr.rtn
#' 
#' aggregate the return series into specific freq
#' @param rtn an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param freq An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.  
#' @return a aggregated return series, of the same class of \code{rtn}.
#' @export
#' @author Ruifei.yin
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' aggr.rtn(rtn,"month")
aggr.rtn <- function(rtn,freq){
  dm <- dim(rtn)
  colnm <- colnames(rtn)
  rtn <- PerformanceAnalytics::checkData(rtn,method="zoo")
  by <- cut.Date2(zoo::index(rtn),freq)
  re <- aggregate(rtn,as.Date(by),PerformanceAnalytics::Return.cumulative)
  colnames(re) <- colnm
  if(is.null(dm)){
    dim(re) <- NULL
  }
  # re <- xts::reclass(re,rtn)
  return(re)
}

#' aggr.quote
#' 
#' aggregate the asset quote series into specific freq.
#' 
#' The function could be used in quote of "open","close","high","low","vol",by specify different \code{var} parametre
#' @param quote an xts, vector, matrix, data frame, timeSeries or zoo object of asset price
#' @param freq An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.  
#' @param var a character string, indicating the aggregated variable,could be:"open","close","high","low","vol",by default "close". 
#' @return a aggregated quote series, of the same class of \code{quote}.
#' @export
#' @author Ruifei.yin
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' price.close <- WealthIndex(rtn)
#' aggr.quote(price.close,"month")
aggr.quote <- function(quote,freq,var="close"){
  dm <- dim(quote)
  colnm <- colnames(quote)
  quote <- PerformanceAnalytics::checkData(quote,method="zoo")
  by <- cut.Date2(zoo::index(quote),freq)
  re <- switch(var,
               close = aggregate(quote,as.Date(by),tail,1),
               open  = aggregate(quote,as.Date(by),head,1),
               high  = aggregate(quote,as.Date(by),max,na.rm=TRUE),
               low   = aggregate(quote,as.Date(by),min,na.rm=TRUE),
               vol   = aggregate(quote,as.Date(by),sum,na.rm=TRUE))
  re <- as.xts(re)
  colnames(re) <- colnm
  if(is.null(dm)){
    dim(re) <- NULL
  }
  # re <- xts::reclass(re,quote)
  return(re)  
}

#' rollingPerformance
#' 
#' get the rolling performance of the return series
#' @param rtn  an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param FUN any function that can be evaluated using a single set of returns (e.g., rolling CAPM.beta won't work, but Return.annualized will)
#' @param width see \code{\link[zoo]{rollapply}} for detail
#' @param by see \code{\link[zoo]{rollapply}} for detail
#' @param align  see \code{\link[zoo]{rollapply}} for detail
#' @param ... other arguments to function FUN or \code{\link[zoo]{rollapply}}
#' @return A object of class xts with the results of the rolling function
#' @export
#' @author Ruifei.Yin
#' @examples 
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' re <- rollingPerformance(rtn, width=20, by=5)
rollingPerformance <- function(rtn,FUN="Return.annualized",width=365,by=30,align = "right",...){
  x = xts::try.xts(rtn)
  columns = ncol(x)
  columnnames = colnames(x)
  funargs <- list(...)
  funargs$width = width
  funargs$by = by
  funargs$FUN = FUN
  funargs$align = align
  for (column in 1:columns) {
    rollargs <- c(list(data = na.omit(x[, column, drop = FALSE])), 
                  funargs)
    column.Return.calc <- do.call(rollapply, rollargs)
    if (column == 1) 
      Return.calc = xts(column.Return.calc)
    else Return.calc = merge(Return.calc, column.Return.calc)
  }
  return(Return.calc)
}

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ---------------------  Plotting utility functions       --------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' multiplot
#' 
#' put multiple graphs on one page.
#' 
#' It can take any number of plot objects as arguments, or if it can take a list of plot objects passed to plotlist. First, set up the plots and store them, but don't render them yet. The details of these plots aren't important; all you need to do is store the plot objects in variables.Once the plot objects are set up, we can render them with \code{multiplot}. See details in \code{http://wiki.stdout.org/rcookbook/Graphs/Multiple}
#' @param ... Plot objects
#' @param plotlist A list of plot objects
#' @param ncol Number of columns of plots
#' @return a page of plots
#' @author Ruifei.Yin
#' @export
#' @examples
#' p1 <- qplot(y=1:4)
#' p2 <- qplot(1:5,10:14)
#' p3 <- qplot(4:30)
#' p4 <- qplot(3:9,4:10)
#' multiplot(p1, p2, p3, p4, ncol=2)
multiplot <- function(..., plotlist=NULL, ncol=1) {   
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)  
  numPlots = length(plots)  
  # Make the panel
  plotCols = ncol                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from ncol  
  # Set up the page
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    grid::viewport(layout.pos.row = x, layout.pos.col = y)  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }  
  re <- recordPlot()
}

#' multiplot_facet
#' 
#' put multiple ggplot objects on one page with facets.
#' 
#' @param plotlist A list of ggplot objects
#' @param ncol Number of columns of plots
#' @param nrow Number of rows of plots
#' @return a ggplot object, with factets of elements of plotlist. The naming rules of the facet label is: if plotlist have names, then use them; else if the element of plotlist have titles, then use them; else, use "plot_1" to "plot_n".
#' @author Ruifei.Yin
#' @export
#' @examples
#' p1 <- qplot(y=1:4)
#' p2 <- qplot(1:5,10:14)
#' p3 <- qplot(4:30)
#' p4 <- qplot(3:9,4:10)
#' multiplot_facet(list(p1, p2, p3, p4), ncol=2, scales="free")
multiplot_facet <- function(plotlist, 
                            nrow = NULL, ncol = NULL, scales = "fixed", 
                            shrink = TRUE, as.table = TRUE, drop = TRUE){
  plots <- plotlist
  pan <- paste("plot",1:length(plots),sep="_")
  for(i in 1:length(pan)){
    title <- plots[[i]]$labels$title
    if(!is.null(title)) pan[i] <- title
    nm <- names(plots[i])
    if(!is.null(nm) && nm != "") pan[i] <- nm
  }  
  p <- ggplot(data=data.frame(pan=pan)) + facet_wrap(~pan, nrow = nrow, ncol = ncol, scales = scales, 
                                                     shrink = shrink, as.table = as.table, drop = drop)
  for(i in 1: length(plots)){
    plot_i <- plots[[i]]
    layer_i <- plot_i$layers
    for(j in 1:length(layer_i)){
      layer_ij <- layer_i[[j]]
      
      data_ij1 <- if(is.list(plot_i$data) && !is.data.frame(plot_i$data)) unlist(plot_i$data) else plot_i$data
      data_ij2 <- if(is.list(layer_ij$data) && !is.data.frame(layer_ij$data)) unlist(layer_ij$data) else layer_ij$data
      # if data in layer is null, then get data from plot
      data_ij <- if(is.null(data_ij2)) data_ij1 else data_ij2 
      # if data in layer and plot are both null, then ...
      data_ij <- if(is.null(data_ij) || length(data_ij)==0) data.frame(pan=pan[i]) else data.frame(data_ij, pan=pan[i])
      layer_i[[j]]$data <- data_ij  # -- NOTE!! this modify the proto object(inherited from plotlist) globally! 
      
      tmpflag <- setdiff(names(plot_i$mapping), names(layer_ij$mapping))
      mapping_ij <- c(plot_i$mapping[tmpflag], layer_ij$mapping) 
      layer_i[[j]]$mapping <- mapping_ij   # -- NOTE!! this modify the proto object(inherited from plotlist) globally!
    }
    p <- p + layer_i    
  }
  return(p)
}




#' ggplots.PerformanceSummary
#' 
#' A wrapper to create a set of charts to demonstrating the performance of the return series,including wealth index chart,drawdown chart,and bar chart for per-period performance.
#' @param rtn  an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param var.cum a list,with elements of integer (variable position) or string (variable name) vecter.This parametre indicate which columns of rtn's wealth index will be layout in which plots. The first element of the var.cum will be layout in the first plot,and so on.By default,all the cols will be layout in one plot
#' @param var.dd a list, indicating which columns of rtn's drawdown will be layout in which plots.
#' @param var.bar a list, indicating which columns of rtn's per-period performance will be layout in which plots.
#' @param bar.freq the freq of the per-period performance bar chart,which is passed into function \code{cut.Date}.An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @param main title of the chart
#' @return Print a wrapped plots of class ggplot,and return a recordedplot object.
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn1 <- zoo(rnorm(300,0.001,0.02),as.Date("2010-01-01")+1:300)
#' rtn2 <- zoo(rnorm(300,0.001,0.02),as.Date("2010-01-01")+1:300)
#' rtn3 <- zoo(rnorm(300,0.001,0.02),as.Date("2010-01-01")+1:300)
#' rtn <- merge(rtn1,rtn2,rtn3)
#' ggplots.PerformanceSummary(rtn)
#' ggplots.PerformanceSummary(rtn,var.cum=list(c(1,2),3),var.dd=list(c(1,2)),var.bar=list(c(1,2,3)))
ggplots.PerformanceSummary <- function(rtn,
                                       var.cum=list(colnames(rtn)),
                                       var.dd=list(colnames(rtn)),
                                       var.bar=list(colnames(rtn)),
                                       bar.freq="month",
                                       main="Charts of Performance Summary"){  
  lp.cum <- c(0.1,0.9) # legend.position
  lp.dd <- c(0.1,0.9)
  lp.bar <- c(0.1,0.9)
  lks <- 0.2 # legend.key.size
  lbs <- 0.2 # legend.background.size   
  # ---- WealthIndex charts
  chart.cum <- list()
  if(length(var.cum)>0){
    for(i in 1:length(var.cum)){
      if(i==1){ # set the title of the whole chart,remove the x axis
        chart.cum[[i]] <- ggplot.WealthIndex(rtn[,var.cum[[i]],drop=FALSE],main=main,size=1)+
          ylab("Wealth")+
          theme(legend.position=lp.cum,legend.key.size = grid::unit(lks, "cm"))+
          #         theme(legend.background = theme_rect(fill="gray90", size=lbs, linetype="dotted"))+
          theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())
      } else { # remove the x axis
        chart.cum[[i]] <- ggplot.WealthIndex(rtn[,var.cum[[i]],drop=FALSE],main=NULL,size=1)+
          ylab("Wealth")+
          theme(legend.position=lp.cum,legend.key.size = grid::unit(lks, "cm"))+
          #         theme(legend.background = theme_rect(fill="gray90", size=lbs, linetype="dotted"))+
          theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())
      }
    }
  }

  # ---- Drawdown charts
  chart.dd <- list()
  if(length(var.dd)>0){
    for(i in 1:length(var.dd)){
      if(i==length(var.dd)){ # remove the x axis title
        chart.dd[[i]] <- ggplot.Drawdown(rtn[,var.dd[[i]],drop=FALSE],main=NULL)+
          ylab("Drawdown")+
          theme(legend.position=lp.dd,legend.key.size = grid::unit(lks, "cm"))+
          #         theme(legend.background = theme_rect(fill="gray90", size=lbs, linetype="dotted"))+
          theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())
      } else { # remove the whole x axis
        chart.dd[[i]] <- ggplot.Drawdown(rtn[,var.dd[[i]],drop=FALSE],main=NULL)+
          ylab("Drawdown")+
          theme(legend.position=lp.dd,legend.key.size = grid::unit(lks, "cm"))+
          #         theme(legend.background = theme_rect(fill="gray90", size=lbs, linetype="dotted"))+
          theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())
      }
    }
  }

  # ---- Return bar charts
  chart.bar <- list()
  freq.lab = if(bar.freq=="day") "Period" else paste(bar.freq,"ly",sep="")  
  if(length(var.bar)>0){
    for(i in 1:length(var.bar)){
      if(i==length(var.bar)){ # maintain the x axis 
        chart.bar[[i]] <- ggplot.rtnBar(rtn[,var.bar[[i]],drop=FALSE],freq=bar.freq,main=NULL)+
          ylab(paste(freq.lab,"return"))+
          theme(legend.position=lp.bar,legend.key.size = grid::unit(lks, "cm"))
        #         theme(legend.background = theme_rect(fill="gray90", size=lbs, linetype="dotted"))
      } else { # remove the x axis
        chart.bar[[i]] <- ggplot.rtnBar(rtn[,var.bar[[i]],drop=FALSE],freq=bar.freq,main=NULL)+
          ylab(paste(freq.lab,"return"))+
          theme(legend.position=lp.bar,legend.key.size = grid::unit(lks, "cm"))+
          #         theme(legend.background = theme_rect(fill="gray90", size=lbs, linetype="dotted"))+
          theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())
      }
    }    
  }
  
  chartlist <- c(chart.cum,chart.dd,chart.bar)
  re <- multiplot(plotlist=chartlist,ncol=1)
  return(re)
}


ggplots.PerformanceSummary2 <- function(rtn,
                                        var.cum=list(colnames(rtn)),
                                        var.dd=list(colnames(rtn)),
                                        var.bar=list(colnames(rtn)),
                                        bar.freq="month",
                                        main="Charts of Performance Summary"){    
  # ---- WealthIndex charts
  chart.cum <- list()
  if(length(var.cum)>0){
    for(i in 1:length(var.cum)){
      chart.cum[[i]] <- ggplot.WealthIndex(rtn[,var.cum[[i]],drop=FALSE],size=1)+
        ggtitle("Wealth")
    }
  }

  # ---- Drawdown charts
  chart.dd <- list()
  if(length(var.dd)>0){
    for(i in 1:length(var.dd)){
      chart.dd[[i]] <- ggplot.Drawdown(rtn[,var.dd[[i]],drop=FALSE])+
        ggtitle("Drawdown")
    }
  }

  # ---- Return bar charts
  chart.bar <- list()
  freq.lab = if(bar.freq=="day") "Period" else paste(bar.freq,"ly",sep="")  
  if(length(var.bar)>0){
    for(i in 1:length(var.bar)){
      chart.bar[[i]] <- ggplot.rtnBar(rtn[,var.bar[[i]],drop=FALSE],freq=bar.freq)+
        ggtitle(paste(freq.lab,"return"))
    } 
  }
     
  chartlist <- c(chart.cum,chart.dd,chart.bar)
  re <- multiplot_facet(plotlist=chartlist,ncol=1,scales="free_y")
  return(re)
}


#' ggplots.RollingPerformance
#' 
#' A wrapper to create a rolling annualized returns chart(of class ggplot), rolling annualized standard deviation chart, and a rolling annualized sharpe ratio chart.

#' @param rtn an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param width see \code{\link[zoo]{rollapply}} for detail
#' @param by see \code{\link[zoo]{rollapply}} for detail
#' @param align  see \code{\link[zoo]{rollapply}} for detail
#' @param ... other arguments to function \code{\link{rollingPerformance}}
#' @return Print a wrapped plots of class ggplot,and return a recordedplot object. 
#' @seealso \code{\link{rollingPerformance}}
#' @author Ruifei.Yin
#' @export
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' ggplots.RollingPerformance(rtn)
ggplots.RollingPerformance <- function(rtn,width=250,by=30,...){
  rtn <- xts::try.xts(rtn)
  rtn.annu <- rollingPerformance(rtn,FUN="Return.annualized",width=width,by=by,...)
  stdev.annu <- rollingPerformance(rtn,FUN="StdDev.annualized",width=width,by=by,...)
  sharp.annu <- rollingPerformance(rtn,FUN="SharpeRatio.annualized",width=width,by=by,...)
  
  rtn.annu <- na.omit(rtn.annu)
  stdev.annu <- na.omit(stdev.annu)
  sharp.annu <- na.omit(sharp.annu)
  
  freq.lab = xts::periodicity(rtn)$label
  main <- paste("Rolling", width, paste(freq.lab,"s",sep=""), "Performance", sep = " ")
  p1 <- ggplot.ts.line(rtn.annu,main=main,size=1)+ 
    ylab("Annu. Return")+
    theme(legend.position=c(0.1, 0.7))+
#     theme(legend.background = theme_rect(fill="gray90", size=.5, linetype="dotted"))+
    theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())+
    scale_y_continuous(labels=scales::percent)
  p2<- ggplot.ts.line(stdev.annu,size=1)+ 
    ylab("Annu. stdev")+
    theme(legend.position=c(0.1, 0.8))+
#     theme(legend.background = theme_rect(fill="gray90", size=.5, linetype="dotted"))+
    theme(axis.text.x= element_blank(),axis.title.x= element_blank(),axis.ticks.x= element_blank())+
    scale_y_continuous(labels=scales::percent)
  p3<- ggplot.ts.line(sharp.annu,size=1)+
    ylab("Annu. Sharp")+
    theme(legend.position=c(0.1, 0.8))
#     theme(legend.background = theme_rect(fill="gray90", size=.5, linetype="dotted"))  
  re <- multiplot(p1,p2,p3,ncol=1)  
  return(re)
}


#' ggplot.Drawdown
#' @export ggplot.Drawdown
ggplot.Drawdown <- function(rtn,geometric=TRUE,main=NULL,...){
  dd <- PerformanceAnalytics:::Drawdowns(rtn,geometric=geometric)
  ggplot.ts.area(dd,main=main,position="identity",...)+
    ylab("Drawdown")+
    scale_y_continuous(labels=scales::percent)
}
#' ggplot.WealthIndex
#' @export ggplot.WealthIndex
ggplot.WealthIndex <- function(rtn,geometric=TRUE,facet=FALSE,main=NULL,...){
  wealth <- WealthIndex(rtn,geometric=geometric)
  ggplot.ts.line(wealth,facet=facet,main=main,...)+
    ylab("Wealth") +
    coord_trans(y="log")
}

#' ggplot.rtnBar
#' @export ggplot.rtnBar
ggplot.rtnBar <- function(rtn,freq="month",main=NULL,...){
  rtn <- aggr.rtn(rtn,freq=freq)
  freq.lab = xts::periodicity(rtn)$scale
  ggplot.ts.bar(rtn,main=main,...)+
    ylab(paste(freq.lab,"Return"))+
    scale_y_continuous(labels=scales::percent)    
}

#' ggplot.rtnHist
#' @export ggplot.rtnHist
ggplot.rtnHist <- function(rtn,main = NULL,bins=NULL, ncol=NULL, ...){
  ggplot.ts.hist(rtn, main=main, bins=bins, ncol=ncol, colour = "white", fill = "black", ...)
}



#' ggplot.ts.line
#' 
#' @param ts a zoo, timeSeries or xts object
#' @param main chart title , character string
#' @param ... other arguments passed to function \code{geom_line}
#' @return a line chart of class \code{ggplot}
#' @author Ruifei.Yin
#' @export ggplot.ts.line
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' ggplot.ts.line(rtn)
ggplot.ts.line <- function(ts, facet=FALSE, main = NULL,...){
  ts <- as.xts(ts)
  ts.df <- data.frame(time=time(ts),zoo::coredata(ts))
  ts.melt <- reshape2::melt(ts.df,id.vars="time")
  if(facet){
    ggplot(ts.melt, aes(x=time, y=value)) +
      ggtitle(main) +
      geom_line(...)+
      theme(legend.title=element_blank())+
      facet_wrap(~variable)
  } else {
    ggplot(ts.melt, aes(x=time, y=value, color=variable)) +
      ggtitle(main) +
      geom_line(...)+
      theme(legend.title=element_blank())
  }
}
  
#' ggplot.ts.area
#' 
#' @param ts a zoo, timeSeries or xts object
#' @param main chart title , character string
#' @param ... other arguments passed to function \code{geom_area}
#' @return a area chart of class \code{ggplot}
#' @author Ruifei.Yin
#' @export ggplot.ts.area
#' @examples
#' rtn.long <- zoo(rnorm(100,0.001,0.02),as.Date("2010-01-01")+1:100)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' ggplot.ts.area(rtn)
ggplot.ts.area <- function(ts, main = NULL,...){
  ts <- as.xts(ts)
  ts.df <- data.frame(time=time(ts),zoo::coredata(ts))
  ts.melt <- reshape2::melt(ts.df,id.vars="time") 
  ggplot(ts.melt, aes(x=time, y=value, fill=variable)) +
    ggtitle(main) +
    geom_area(...)+
    theme(legend.title=element_blank())
}

#' ggplot.ts.bar
#' 
#' @param ts a zoo, timeSeries or xts object
#' @param main 
#' @param ... other arguments passed to function \code{geom_bar}
#' @return a bar chart of cloass \code{ggplot}
#' @author Ruifei.Yin
#' @export ggplot.ts.bar
#' @examples
#' rtn.long <- zoo(rnorm(10,0.001,0.02),as.Date("2010-01-01")+1:10)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' ggplot.ts.bar(rtn)
ggplot.ts.bar <- function(ts,main = NULL,...){
  ts <- as.xts(ts)
  ts.df <- data.frame(time=time(ts),zoo::coredata(ts))
  ts.melt <- reshape2::melt(ts.df,id.vars="time")
  ggplot(ts.melt, aes(x=time, y=value, fill=variable)) +
    ggtitle(main) +
    geom_bar(position="dodge",stat="identity",...)+
    theme(legend.title=element_blank())  
}

#' @export ggplot.ts.hist
ggplot.ts.hist <- function(ts, main = NULL, bins=NULL, ncol=NULL, colour = "white", fill = "black", ...){
  ts <- as.xts(ts)
  ts.df <- data.frame(time=time(ts),zoo::coredata(ts))
  ts.melt <- reshape2::melt(ts.df,id.vars="time")
  ggplot(ts.melt, aes(value)) +
    ggtitle(main) +
    geom_histogram(colour =colour, fill = fill, bins = bins, ...)+
    facet_wrap(~variable, ncol=ncol)+
    theme(legend.title=element_blank())
}

#' ggplot.corplot
#' 
#' @param corr is correlation matrix or a list of correlation matrix ,see \code{\link[stats]{cor}}.
#' @param show show which part of the correlation matrix? "all","lower" or "upper".
#' @export ggplot.corr
ggplot.corr <- function(corr, show=c("all","lower","upper")) {
  show <- match.arg(show)
  subfun <- function(corr){
    corr <- round(corr,digits = 2)
    if(show =="lower"){
      corr[upper.tri(corr)] <- NA
    } else if (show == "upper") {
      corr[lower.tri(corr)] <- NA
    } 
    corr <- reshape2::melt(corr, na.rm = TRUE)
    colnames(corr) <- c("fname","fnamecor",'value')
    return(corr)
  }
  
  if(is.matrix(corr)){
    cordf <- subfun(corr)
    ggplot(data=cordf,aes(fname,fnamecor,fill=value))+geom_tile()+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white")+
      geom_text(aes(fname,fnamecor, label = value), color = "black")+
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
    
  }else{
    N <- floor(sqrt(length(corr)))
    cordf <- plyr::ldply(corr,subfun,.id = 'date')
    
    ggplot(data=cordf,aes(fname,fnamecor,fill=value))+geom_tile()+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white")+
      geom_text(aes(fname,fnamecor, label = value), color = "black")+facet_wrap(~ date,ncol=N)+
      theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
    
  }
  
}




#' melt.ts
#' 
#' melt the multi-columns timeseries to single-column dataframe for easy ggploting. 
#' @param ts a zoo, timeSeries or xts object
#' @return a data frame object melted from the ts,with cols:time,variable,value
#' @author Ruifei.Yin
#' @export melt.ts
#' @examples
#' rtn.long <- zoo(rnorm(10,0.001,0.02),as.Date("2010-01-01")+1:10)
#' rtn.short <- rtn.long + rnorm(100,-0.001,0.003)
#' rtn <- merge(rtn.long,rtn.short)
#' df <- melt.ts(rtn)
melt.ts <- function(ts){
  ts <- as.xts(ts)
  ts.df <- data.frame(time=time(ts),zoo::coredata(ts))
  ts.melt <- reshape2::melt(ts.df,id.vars="time")
  return(ts.melt)
}

numericFormatRow <- function(df, percentRows = c(1:nrow(df))){  
  for(i in 1:ncol(df)){
    temp <- as.numeric(df[,i])
    temp2 <- rep("",length(temp))
    for(j in 1:length(temp2)){
      temp2[j] <- formatC(temp[j], format = "f", digits = 2)
    }
    for(j in percentRows){
      temp2[j] <- paste(formatC(100 * temp[j], format = "f", digits = 1), "%", sep = "")
    }
    df[,i] <- temp2    
  }
  return(df)
}

numericFormatCol <- function(df, percentCols = c(1:ncol(df))){  
  for(i in 1:ncol(df)){
    temp <- as.numeric(df[,i])
    temp2 <- rep("",length(temp))
    for(j in 1:length(temp2)){
      temp2[j] <- formatC(temp[j], format = "f", digits = 2)
    }
    if(is.numeric(df[,i])){
      df[,i] <- temp2
    }    
  }
  for(i in percentCols){
    temp <- as.numeric(df[,i])
    temp2 <- rep("",length(temp))
    for(j in 1:length(temp2)){
      temp2[j] <- paste(formatC(100 * temp[j], format = "f", digits = 1), "%", sep = "")
    }
    df[,i] <- temp2
  }
  return(df)
}





# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===================== Public used functions              =============
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


#' @export
rdate2int <- function(rdate){
  if(class(rdate)!="Date"){
    stop("The rdate is not of class 'Date'!")
  }
  intdate <- as.integer(as.character(rdate,"%Y%m%d"))
  return(intdate)
}

#' @export
intdate2r <- function(intdate){
  rdate <- as.Date(as.character(intdate),"%Y%m%d")
  return(rdate)
}



#' Quote a character string
#' @export
QT <- function(x,sym=1){
  if(sym==1){
    y <- paste("'",x,"'",sep="")
  } else if(sym==2){
    y <- paste('"',x,'"',sep="")
  }  
  return(y)
}

#' bracket a series of string
#'
#' @author Andrew Dow
#' @param series is a series object
#' @return a string object with bracket surrounded the series object.
#' @examples
#' series <- c('EQ000001','EQ000002')
#' brkQT(series)
#' @export
brkQT <- function(series){
  tmp <- paste(series,collapse = "','")
  tmp <- paste("('",tmp,"')",sep='')
  return(tmp)
}

#' read from or write to clipboard
#'
#' @description \code{read.clipboard} read data from clipboard
#' @description \code{write.clipboard} write data to clipboard
#' @name read_write_clipboard
#' @rdname read_write_clipboard
#' @export
#' @examples
#' re <- read.clipboard()
#' write.clipboard(re)
read.clipboard <- function(header = TRUE, colClasses = NA, sep = "\t", ...) {
  read.table(file = "clipboard",colClasses = colClasses, sep = sep, header=header, stringsAsFactors = FALSE, ...)
}

#' @rdname read_write_clipboard
#' @export
write.clipboard <- function(x, row.names=FALSE, col.names=TRUE,...) {
  write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=col.names, ...)
}


#' vlookup
#' 
#' @param x a vector
#' @param table a matrix, data frame or other matrix like object
#' @param by a vector of column names or column index
#' @param ret a vector of column names or column index
#' @return a matrix, data frame or vector, corresponding with the class of \code{table} and length of \code{ret}.
#' @export
#' @examples
#' x <- c(2,4,3)
#' table <- data.frame(1:4,LETTERS[1:4])
#' vlookup(x,table,by=1,ret=2)
vlookup <- function(x, table, by, ret){
  re <- table[match(x, table[ ,by]), ret]
  return(re)
}


#' vloolup.df
#' 
#' @param x a data frame
#' @param table a data frame
#' @param by a vector of character string
#' @param ret a vector of character string or integer
#' @param exclude.x a logical. If the \code{x} shoud be inculded in the result?
#' @return a data frame (or a vector if \code{exclude.x} is TRUE and length of \code{ret} is 1).
#' @export vlookup.df
#' @examples
#' x <- data.frame(m1=1:3,m2=c("a","b","c"))
#' table <- data.frame(K1=c(2,3,1,5,6),K2=c("b","c","a","f","x"),v1=LETTERS[1:5],v2=LETTERS[8:12])
#' vlookup.df(x,table,c("K1","K2"))
#' 
#' y <- data.frame(m1=c(1,3,1),m2=c("a","c","a"))
#' vlookup.df(y,table,c("K1","K2"))
#' 
#' z <- data.frame(m1=c(1,5,3),m2=c("a","a","c"))
#' vlookup.df(z,table,c("K1","K2"))
#' vlookup.df(z,table,c("K1","K2"), exclude.x=FALSE)
vlookup.df <- function(x, table, by=colnames(x), ret=setdiff(colnames(table),by), exclude.x=TRUE){
  x <- renameCol(x, src=colnames(x), tgt=by)
  if("PK_" %in% colnames(x)) stop ("Name conflict!")
  x$PK_ <- 1:NROW(x)
  x <- data.table::data.table(x, key=by)
  table <- data.table::data.table(table, key=by)
  re <- merge(x, table, by=by, all.x=TRUE)
  re <- as.data.frame(re)
  re <- dplyr::arrange(re, PK_)
  re$PK_ <- NULL
  if(exclude.x){
    re <- re[ ,ret]
  }  
  return(re)
}


#' merge.x
#' 
#' Just same with \code{merge}, except that: (1) the order of merging result is kept same with \code{x};(2)speed up by using \code{merge.data.table}.
#' @param x a data frame
#' @param y a data frame
#' @param by a vector of shared column names in x and y to merge on.
#' @param mult a character string: one of "all","first","last". See detail in \code{\link[data.table]{data.table}}
#' @return a new data.frame based on the merged data tables, sorted by the x order.
#' @export merge.x
#' @author Ruifei.Yin
#' @examples
#' x <- data.frame(k1=c(1,5,3),k2=c("a","a","c"),V=11:13)
#' y <- data.frame(k1=c(2,3,1,5,6),k2=c("b","c","a","f","x"),v1=LETTERS[1:5],v2=LETTERS[8:12])
#' # compare the following result:
#' merge.x(x,y)
#' merge(x,y)
#' merge(x,y,all.x=TRUE)
#' dplyr::left_join(x,y)
merge.x <- function(x, y, by = intersect(colnames(x),colnames(y)), mult="all"){
  if("PK_" %in% colnames(x)) stop ("Name conflict!")
  cols_x <- colnames(x)
  cols_y <- colnames(y)
  x$PK_ <- 1:NROW(x)
  x <- data.table::data.table(x,key=by)
  y <- data.table::data.table(y,key=by)
  re <- y[x, mult=mult]
  re <- as.data.frame(re)
  re <- dplyr::arrange(re,PK_)
  re <- dplyr::select(re,dplyr::one_of(c(cols_x,setdiff(cols_y,by))))
  return(re)
}


#' ls.class
#'
#' list the names of objects with certain class
#' @param class :character string or a character vector, giving the class of objects to list
#' @param pattern :an optional regular expression. 
#' @return a character vector,giving the names of the objects in the global environment with the given classes 
#' @author Ruifei.Yin
#' @export
#' @examples 
#' ls.class(c("function","character"))
ls.class <- function(class,pattern=""){
  allobj <- ls(pos=1,pattern=pattern)
  objclass <- lapply(allobj,function(x) class(get(x))) # use lapply instead of lapply because one object could have more than one classes
  flag <- sapply(objclass,function(x) any(class %in% x))
  result <- allobj[flag]
  return(result)
}









#' lag.m
#' 
#' Methods for computing lags of matrix or vector objects.
#' @param m a matrix or a vector
#' @param k the number of lags (in units of observations).Note the sign of k: a series lagged by a positive k is shifted earlier,and a negtive k is shifted later.
#' @param na.pad logical. If TRUE it adds any rows that would not otherwise have been in the result with a value of NA. If FALSE those rows are dropped.
#' @return a matirx or a vector
#' @export lag.m
#' @author Ruifei.yin
#' @examples
#' m <- matrix(1:10,5,2)
#' rownames(m) <- letters[1:5]
#' colnames(m) <- LETTERS[1:2]
#' lag.m(m,1)
#' lag.m(m,-1)
#' lag.m(m,1,TRUE)
#' v <- c(a=1,b=2,c=3,d=4)
#' lag.m(v,-2,TRUE)
lag.m <- function(m,k=1,na.pad=FALSE){
  if (k == 0) return(m)
  if(is.null(dim(m))){
    flag.vector <- TRUE
    m <- as.matrix(m)
  } else{
    flag.vector <- FALSE
  }
  colnm <-colnames(m)
  rownm <- rownames(m)
  ncols <- ncol(m)
  nrows <- nrow(m)  
  if (k != round(k)) {
    k <- round(k)
    warning("k is not an integer")
  }    
  if (abs(k) > nrows) k <- nrows
  kk <- abs(k)    
  if(!na.pad){
    if(k>0){
      re <- m[1:(nrows-kk),,drop=FALSE]
      rownm <- rownm[(kk+1):nrows] 
    } else {
      re <- m[(kk+1):nrows,,drop=FALSE]
      rownm <- rownm[1:(nrows-kk)]
    }
  } else {
    if(k>0){
      re <- rbind(matrix(NA,kk,ncols),m[1:(nrows-kk),,drop=FALSE])
    } else {
      re <- rbind(m[(kk+1):nrows,,drop=FALSE],matrix(NA,kk,ncols))
    }    
  }    
  if(flag.vector){
    dim(re) <- NULL
    names(re) <- rownm
  } else{
    colnames(re) <- colnm
    rownames(re) <- rownm
  }
  return(re)  
}






#' lag.df
#' 
#' Methods for computing lags of data frame objects.
#' @param df a data frame object
#' @param k the number of lags (in units of observations).Note the sign of k: a series lagged by a positive k is shifted earlier,and a negtive k is shifted later.
#' @param na.pad logical. If TRUE it adds any rows that would not otherwise have been in the result with a value of NA. If FALSE those rows are dropped.
#' @return a data frame object 
#' @export lag.df
#' @author Ruifei.yin
#' @examples
#' m <- matrix(1:10,5,2)
#' rownames(m) <- letters[1:5]
#' colnames(m) <- LETTERS[1:2]
#' df <- as.data.frame(m)
#' lag.df(df,1)
#' lag.df(df,-1)
#' lag.df(df,1,TRUE)
lag.df <- function(df,k=1,na.pad=FALSE){  
  if (k == 0) return(df)
  colnm <-colnames(df)
  rownm <- rownames(df)
  ncols <- ncol(df)
  nrows <- nrow(df)  
  if (k != round(k)) {
    k <- round(k)
    warning("k is not an integer")
  }   
  if (abs(k) > nrows) k <- nrows
  kk <- abs(k)  
  if(na.pad){
    pad <- as.data.frame(matrix(NA,kk,ncols))
    colnames(pad) <- colnm
    if(k>0){
      re <- rbind(pad,df[1:(nrows-kk),,drop=FALSE])
      rownames(re) <- rownm
    } else {
      re <- rbind(df[(kk+1):nrows,,drop=FALSE],pad)
      rownames(re) <- rownm
    }
  } else {
    if(k>0){
      re <- df[1:(nrows-kk),,drop=FALSE]
      rownames(re) <- rownm[(kk+1):nrows]
    } else {
      re <- df[(kk+1):nrows,,drop=FALSE]
      rownames(re) <- rownm[1:(nrows-kk)]
    }    
  }  
  return(re)
}


#' cut.Date2
#' 
#' a wrapped function of \code{\link{cut.Date}} by adding a parametr \code{lab.side},which indicating if the level of the result is the "end" or "begin" of the real interval.
#' @param x an object inheriting from class "POSIXt" or "Date".
#' @param lab.side a character string("end" or "begin"),indicating if the level of the result is the "end" or the "begin" of the interval
#' @param ... other params passed to funcion cut.Date.
#' @return A factor is returned, unless labels = FALSE which returns the integer level codes.
#' @export cut.Date2
#' @author Ruifei.Yin
cut.Date2 <- function(x, breaks, lab.side="end",labels = NULL, start.on.monday = TRUE, right = FALSE, ...){
  x1 <-cut(x,breaks=breaks, labels = labels, start.on.monday = start.on.monday, right = right, ...) 
  if(lab.side=="end"){    
    level.new <- aggregate(x,list(x1),max,na.rm=TRUE)[[2]]   
  } else if (lab.side=="begin"){
    level.new <- aggregate(x,list(x1),min,na.rm=TRUE)[[2]]
  }
  x1 <- factor(x1)
  levels(x1) <- level.new
  return(x1)
}

#' substrRight
#' @seealso \code{\link{substr}}
#' @export
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#' findInterval.rightClosed
#' @seealso \code{\link{findInterval}}
#' @export
findInterval.rightClosed <- function(x, vec, ...) {
  fi <- findInterval(x, vec, ...)
  fi - (x==vec[fi])
}




#' Get bivariate random variables
#' 
#' Compute a bivariate sample distribution drawn from a population with a given . It either computes two random variables, or it takes one existing variable (passed as parameter x) and creates a second variable with the desired correlation. 
#' 
#'  Get from a forum post: \url{http://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variable/15035#15035}
#' @note this is only an approximate solution, i.e., the empirical correlation is not exactly equal to rho.
#' @param n a integer. number of observations
#' @param rho a numeric. the desired correlation
#' @param mar.fun the marginal distribution function
#' @param x a vector. the existing variable.
#' @param drop.x logical. If true, then return a vector of the generated variable.
#' @param ... other arguments to mar.fun
#' @return  a data frame of two variables which correlate with a population correlation of rho, or a vector when drop.x is true.
#' @export
#' @examples
#' # -- generate two random variables
#' df <- getBiCop(100,0.2)
#' cor(df)
#' # -- generate another variable with desired correlation on a existing variable
#' x <- rnorm(100)
#' df2 <- getBiCop(100,0.2,x=x)
#' cor(df2)
getBiCop <- function(n, rho, mar.fun=rnorm, x = NULL, drop.x=FALSE, ...) {
  if (!is.null(x)) {X1 <- x} else {X1 <- mar.fun(n, ...)}
  if (!is.null(x) & length(x) != n) warning("Variable x does not have the same length as n!")  
  C <- matrix(rho, nrow = 2, ncol = 2)
  diag(C) <- 1  
  C <- chol(C)  
  X2 <- mar.fun(n, ...)
  X <- cbind(X1,X2)  
  # induce correlation (does not change X1)
  df <- X %*% C  
  ## if desired: check results
  #all.equal(X1,X[,1])
  #cor(X)  
  if (!is.null(x) & drop.x) {
    return(df[,2])
  } else {
    return(df)  
  }  
}


#' Rename columns in a matrix or a dataframe.
#' @param indata  A dataframe or a matrix 
#' @param src  Source: Vector of names of columns in 'indata' to be renamed. Can also be a vector of column numbers.
#' @param tgt  Target: Vector with corresponding new names in the output.
#' @return A dataframe if 'indata' is a dataframe; a matrix in 'indata' is a matrix.
#' @note This function is imported from \code{\link[doBy]{renameCol}}.
#' @export
#' @examples
#' CO2 <- data.frame(a=1,b=2)
#' renameCol(CO2, 1:2, c("kk","ll"))
#' renameCol(CO2, c("a","b"), c("kk","ll"))
renameCol <- function (indata, src, tgt) {
  if (inherits(indata, "data.frame")) {
    isDF <- TRUE
    dfnames <- names(indata)
  }
  else {
    if (inherits(indata, "matrix")) {
      isDF <- FALSE
      dfnames <- colnames(indata)
    }
    else {
      stop("'indata' must be either a dataframe or a matrix")
    }
  }
  if (length(src) != length(unique(src))) {
    stop("A src name has been repeated")
  }
  if (length(tgt) != length(unique(tgt))) {
    stop("A tgt name has been repeated")
  }
  if (length(src) != length(tgt)) {
    stop("length of src not equal to length of tgt")
  }
  if (is.numeric(src)) {
    idx <- src
    iii <- intersect(seq_along(dfnames), src)
    iii <- setdiff(src, iii)
    if (length(iii) > 0) {
      sss <- sprintf("Column(s) %s are not in 'indata'", 
                     toString(iii))
      stop(sss)
    }
  }
  else {
    idx <- match(src, dfnames)
    if (any(is.na(idx))) {
      sss <- sprintf("Column names %s are not in 'indata'", 
                     toString(src[is.na(idx)]))
      stop(sss)
    }
  }
  ans <- indata
  if (isDF) {
    names(ans)[idx] <- tgt
  }
  else {
    colnames(ans)[idx] <- tgt
  }
  return(ans)
}



# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# ===================== Event Driven Models               ==============
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

#' getEventRtn
#' 
#' Given a event data(ussually a TS object), calculate the extra returns driven by the event.
#' @aliases getEventRtn getEventRtns
#' @param TS a TS object.
#' @param holdingday a integer, giving the holding day.
#' @param bmk a character string,giving the stockID of the benchmark index,eg. "EI000300".
#' @param fee a mumeric, giving the one side fee of trading.
#' @param tradeType a character string("close","nextavg","nextopen"),indicating the trading type.See detail in \code{\link{getPeriodrtn}}
#' @return getEventRtn return a data frame with cols:"stockID","date","endT","rtn","bmkRtn","exRtn".
#' @export
#' @author Ruifei.Yin
#' @examples
#' eventRtn <- getEventRtn(TS,20)
getEventRtn <- function(TS,holdingday,bmk="EI000300",fee=0,tradeType=c("close","nextavg","nextopen")){
  check.TS(TS)
  endT <- trday.nearby(TS$date,by= holdingday)
  if(holdingday>0){
    rtn.event <- getPeriodrtn(stockID=TS$stockID,begT=TS$date,endT=endT,tradeType=tradeType)-fee*2
    rtn.bmk <- getPeriodrtn(stockID=bmk,begT=TS$date,endT=endT,tradeType="close",datasrc='ts')
  } else if(holdingday<0){
    rtn.event <- getPeriodrtn(stockID=TS$stockID,begT=endT,endT=TS$date,tradeType=tradeType)-fee*2
    rtn.bmk <- getPeriodrtn(stockID=bmk,begT=endT,endT=TS$date,tradeType="close",datasrc='ts')
  } else {
    rtn.event <- 0
    rtn.bmk <- 0
  }
  rtn.ex <- rtn.event-rtn.bmk
  re <- data.frame(TS,endT=endT,rtn=rtn.event,bmkRtn=rtn.bmk,exRtn=rtn.ex)
  return(re)
}
#' @rdname getEventRtn
#' @param holdingdays a vector of integers,giving the list of holding days
#' @return getEventRtns return a data frame(with cols same as return of getEventRtn),which is a rbind of returns of getEventRtn by different holdingdays. 
#' @examples
#' eventRtns <- getEventRtns(head(TS,10),c(-10,-5,0,5,10))
getEventRtns <- function(TS,holdingdays,bmk="EI000300",fee=0,tradeType=c("close","nextavg","nextopen")){  
  for(i in 1:length(holdingdays)){
    re.sub <- getEventRtn(TS=TS,holdingday=holdingdays[i],bmk=bmk,fee=fee,tradeType=tradeType)
    re.sub$holdingday <- holdingdays[i]
    if(i==1){
      re <- re.sub
    } else {      
      re <- rbind(re,re.sub)
    }     
  }
  return(re)
}

eventRtn.stats <- function(eventRtn){
  
}






