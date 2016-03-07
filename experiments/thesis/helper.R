

getcolV <- function(s1,s2,r) {
    tmp <- apply(idl[idl[,1]==r  & idl[,2]==s1 & idl[,3]==s2,6:261],2,var)
      ##tmp <- idl0[idl0[,1]==r  & idl0[,2]==s1 & idl0[,3]==s2,6:261]
      return(matrix(tmp,ncol=2))
}

getcolM <- function(s1,s2,r) {
    tmp <- colMeans(idl[idl[,1]==r  & idl[,2]==s1 & idl[,3]==s2,6:261])
      ##tmp <- idl0[idl0[,1]==r  & idl0[,2]==s1 & idl0[,3]==s2,6:261]
      return(matrix(tmp,ncol=2))
}

readCI <- function(phz,ecc) 
  return(read.csv(paste('~/Box Documents/shawn/data/CI/s21_', phz, '_', ecc, '.csv', sep=''),header=F))

cmpCI <- function(idl) {
  ##n <- 7000
  n <- 4000
  ##idl <- read.csv('/Users/sb/Downloads/code/idl10/idl_0.csv',header=F)
  b <- c(0,1)
  cmbs <- expand.grid(s1=b,s2=b,r=b+1)
  colM <- with(cmbs,mapply(getcolM,s1,s2,r,SIMPLIFY=FALSE))
  colV <- with(cmbs,mapply(getcolV,s1,s2,r,SIMPLIFY=FALSE))
  cmdf <- data.frame(colM)
  cvdf <- data.frame(colV)
  cmm <- as.matrix(cmdf)
  cmv <- as.matrix(cvdf)
  w <- c(1,1,1,-1,-1,1,-1,-1,-1,-1,-1,1,1,-1,1,1)
  cM <- cmm %*% w
  cV <- rowMeans(cmv)
  return(cbind(cM,cV))
}

cmpRn <- function(cM,cV,p) {
  cM.upper <- cM+cV
  cM.lower <- cM-cV
  
  xs <- seq(1,128); w <- .05; f <- 4; sz <- length(xs); cen <- median(xs)
  sig <- w*cos(f*2*pi*((seq(1:sz)-cen)/sz)+p)
  ##cdf <- data.frame(cm=cM,xs=xs,upper=cM.upper,lower=cM.lower,n=n)
  upr <- matrix(cM.upper,ncol=16)
  lwr <- matrix(cM.lower,ncol=16)
  lgcMu <- as.vector(apply(upr,2,function(x) x == max(x)))
  lgcMl <- as.vector(apply(lwr,2,function(x) x == min(x)))
  
  if (!lgcMu[1] && !lgcMl[1])  
    lgcMu[1] <- lgcMl[1] <- TRUE
  if (!lgcMl[128] && !lgcMu[128]) 
    lgcMu[128] <- lgcMl[128] <- TRUE
  
  cdf <- data.frame(lwr = lwr[lgcMl], upr = upr[lgcMu], lxs = xs[lgcMl], uxs = xs[lgcMu])
  return(cdf)
}

##ggplot() + geom_polygon(data=cdf,aes(x=c(lxs,rev(lxs)),y=c(upr,rev(lwr))),fill='gray60') +
##geom_line(aes(xs,sig),col='darkorange') + geom_line(aes(xs,cM),col='gray80') + ggtitle('cos 5 deg - 2k')
