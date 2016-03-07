
mkrndtms <- function(pdb,ns=c(2,3,3,1,1,1)) {
  tm.names <- 1:ntms
  ns <- ns  + sample(c(rep(0,2),rep(1,4))) # add a few extra players
  z <- mapply(function(x,y)
              mysort(pdb[pdb$position==x,]$median)[1:y],posnms,ntms*ns)
  stms <- sapply(ns,function(x) sample(rep(tm.names,x)))
  xs <- mapply(function(x,y) pdb[pdb$position==x,]$X[y], posnms,z)
  for (i in 1:npos) pdb[xs[[i]],of] = stms[[i]]
  return(pdb)
}

# rearrange the database with a column for each week
byweek <- function(pdb) {
  fnames <- unique(pdb$fullname)
  maxwk <- max(pdb$week)
  fw <- function(fname,wn)
    with(pdb,projectedpoints[fullname==fname & week==wn][1])
  mat <- sapply(1:maxwk, function(x) sapply(fnames,fw,wn=x))
  pos <- sapply(fnames, function(x) with(pdb,position[fullname==x][1]))
  pdb.out <- data.frame(cbind(paste(fnames),paste(pos),mat))
  names(pdb.out) <- c('fullname','position',paste('week',1:maxwk))  
  return(pdb.out)
}

getactpts <- function(plr,wk) {
  pts <- 0
  if (plr > 0) 
    pts <- try(with(adb, fantasyPoints[fullname == plr & week == wk]))
  return(pts[1])
}

getstrs <- function(rndx,pndx,wk) {
  pn <- posns[pndx]
  ln <- length(rndx)
  if (pn == ln) ret <- rndx   
  else if (ln < pn) ret <- c(rndx,rep(0,pn-ln))
  else ret <- rndx[1:pn]
  return(ret)
}

getrndx <- function(pdb,pndx,tndx,wk) pdb[pdb$position == posnms[pndx] & pdb[,of+wk] == tndx,1]

proj.sort <- function(pdb,rndx,wk) {
  lndx=!is.na(pdb[rndx,wk+wf]>0)  # is.na logical index
  prj <- sort(pdb[rndx,wk+wf],decreasing=T,index.return=T)
  list(proj=prj$x,ix=pdb[rndx,1][lndx][prj$ix])
}

domatchup <- function(pdb,lu,schd,rcd,wk) {
  for (i in 1:5) {
    rcd <- setrcd(pdb,lu,rcd,schd[i,1],schd[i,2],wk) }
  return(rcd)
}

teampts <- function(pdb,lu,tm,wk) {
  strs <- lu[[tm]][[1]]
  tpts <- sapply(strs,function(x) sapply(x, function(y) ifelse(y[!is.na(y)]>0,
                                         getactpts(as.character(pdb[y,2]),wk),0)))
  return(sum(unlist(tpts),na.rm=T))
}

setrcd <- function(pdb,lu,rcd,tm1,tm2,wk) {
  scr <- c(teampts(pdb,lu,tm1,wk),teampts(pdb,lu,tm2,wk))
  if (scr[1] == scr[2]) print('tie')
  if (scr[1] > scr[2]) {
    rcd[tm1,1] <- rcd[tm1,1] + 1
    rcd[tm2,2] <- rcd[tm2,2] + 1
  } else {
    rcd[tm1,2] <- rcd[tm1,2] + 1
    rcd[tm2,1] <- rcd[tm2,1] + 1
  }
  return(rcd)
}
  
cmbstrs <- function(pdb,x,tnum,wk) {  # combine starters by position
  rix <- getrndx(pdb,x,tnum,wk)  # row index
  srtd <- proj.sort(pdb,rix,wk)  # row indexes sorted by projection
  strs <- getstrs(srtd$ix,x,wk)    # starters
  return(strs)
}

getbench <- function(pdb,x,strs,tnum,wk) {
  rix <- getrndx(pdb,x,tnum,wk)  # row index
  bench <- rix[!rix %in% strs]  # bench
  return(bench)
}

mklineup <- function(pdb,wk,tnum) {
  strs <- lapply(1:npos,cmbstrs,pdb=pdb,tnum=tnum,wk=wk)
  bench <- lapply(1:npos, function(x) getbench(pdb,x,strs[[x]],tnum,wk))
  flx <- proj.sort(pdb,unlist(bench[2:4]),wk)$ix  # flex
  strs[[7]] <- flx[!is.na(flx>0)][1]
  bench <- lapply(1:npos, function(x) bench[[x]][bench[[x]] != strs[[7]]])  # remove flex from the bench
  return(list(strs,bench))
}

getrulewaiver <- function(pids)  
  sapply(1:npos,function(x) sum(pids==x))

getwaiver <- function(lu) {
  sl <- lapply(lu,function(x) x[[1]])  # list of starters 
  min.waiver <- sapply(1:ntms, function(y) sapply(sl[[y]],function(x) sum((x==0 | is.na(x)))))
  return(min.waiver)
}

getruledrop <- function(plrs,wk,thresh,mu,sd) {
  p <- unlist(plrs[!is.na(plrs)])
  ptdifs <- getptdiff(p,wk,mu,sd)
  #ptdifs.p <- ptdifs[ptdifs > 0]
  nmd <- names(ptdifs[(ptdifs * rbeta(length(ptdifs),5,2) * log(wk+1) > thresh)])
  return(getpid(nmd))
}

dowaiver <- function(pdb,waiver,wk,lu,tm,dflg=1) {
  wvrordr <- rank(rcd[,1],ties.method='random')
  wvr <- waiver[,wvrordr]
  bench <- lapply(lu,function(x) x[[2]])
  while (sum(wvr) > 0) {
    for (i in 1:ntms) { 
      wpos <- posnmsF[as.logical(wvr[,i])]
      if (length(wpos)>0) {
        for (j in 1:length(wpos)) {
          ix <- wvrordr[i]
          pdb <- plradd(pdb,wpos[j],ix,wk,tm)
          if (dflg) pdb <- drop1plr(pdb,bench[[ix]],wk,tm)
          lu[[ix]] <- mklineup(pdb,wk,i)
          bench[[ix]] <- lu[[ix]][[2]]
        }
      }
    }
    wvr[wvr>0] <- wvr[wvr>0]-1
  }
  return(pdb)
}

mysort <- function(x)
  sort(x,index.return=T,decreasing=T)$ix

plradd <- function(pdb,pos,tnum,wk,tm) {
  if (pos == 'FLEX') pos <- ifelse(runif(1) > .5,'RB','WR') # arbitrarily assign flex position
  pid0 <- proj.sort(pdb,pdb[pdb$position == pos & pdb[,of+wk] == 0,1],wk)$ix
  pid <- pid0[ifelse(rbeta(1,5,2)>.5,1,2)]
  if (length(pid0)>1) {
    pdb[pid,of+wk] <- tnum
    if(tnum == tm) print(paste('add',pdb[pid,2]))
  }
  return(pdb)
}

getptdiff <- function(plrs,wk,mu=0,sd) {
  ulb0 <- unlist(plrs);             # flatten the list
  ulb <- ulb0[!is.na(ulb0) & ulb0>0]         # remove NAs 
  if (length(ulb)>0) {
    pnms <- sapply(ulb,getfname)      # use fullname since there is no player id 
    ix <- ifelse(wk == 1,2,wk)        # average prior points (correct for week 1)
    mpts <- sapply(pnms,function(x) try(mean(adb[adb$fullname == x & adb$week < ix,9])))
    mpts[is.na(mpts)] <- 0; if (length(mpts)==0) mpts <- -10
    pix <- unlist(lapply(pl <- lapply(names(mpts),getpos),getposid))
    ifelse(mu==0,mp<-sapply(pl,getposmean,wk=ix),mp<-mu[pix]) 
    ifelse(mu==0,sdp <- sapply(pl,getpossd,wk=ix),sdp<-sd[pix]) # get the position average for a particular week
    r <- (mp-mpts)/sdp*rbeta(1,4,2)
  } else r <- 0
  return(r)              # normalize by the standard deviation
}

drop1plr <- function(pdb,bench,wk,tm) {
  ulb0 <- unlist(bench); 
  ulb <- ulb0[!is.na(ulb0) & ulb0>0]
  ptdiffs <- getptdiff(ulb,wk)
  dnm0 <- names(ptdiffs[order(ptdiffs,decreasing=T)])
  dnm <- dnm0[!is.na(dnm0)][1]
  
  if (length(dnm)>0) { 
  
    if (pdb[getpid(dnm),of+wk] == tm) 
      print(paste('drop',pdb[getpid(dnm),2]))      
    try(pdb[getpid(dnm),of+wk] <- 0)
  } 
  return(pdb)
}

droplr <- function(pdb,pid,wk,tm) {
    for (i in 1:length(pid)) {
      if(pdb[pid[i],of+wk] == tm) 
        print(paste('drop',pdb[pid[i],2]))
    }
  pdb[pid,of+wk] = 0
  return(pdb)
}

getfname <- function(pid)
  as.character(pdb[pid,2])

getpos <- function(plr)
   as.character(adb[adb$fullname == plr,]$position[1])

getposid <- function(pos) {
  z <- as.numeric(levels(pdb$position) == pos)*1:6
  return(z[z>0])
}

getposnum <- function(pid)
  posndx[as.numeric(pdb[pid,3])]

getposmean <- function(pos,wk)    
  mean(c(as.matrix(pdb[pdb$position == pos & pdb[,of] != 0,wf+1:wk+wf])),na.rm=T)

getpossd <- function(pos,wk)      
  sd(c(as.matrix(pdb[pdb$position == pos & pdb[,of] != 0,wf+1:wk+wf])),na.rm=T)

getpid <- function(plr)
  pdb[pdb$fullname == plr,1]

getmode <- function(pdb,x) {
  r <- as.numeric(pdb[x,18:29])
  t<-tapply(r,r,length)
  return(as.numeric(names(t)[which.max(t)]))
}

doseason <- function(seas,tm,t,rule) {
  for (wk in 1:12) {
    print('')
    print(paste('season',seas,'week',wk))
    pdb[,of+wk] <- pdb[,of+wk-1]      # set the current week owners to previous week owners 
    lu0 <- lapply(1:ntms,function(x) mklineup(pdb,wk,x))  # weekly lineup  
    #if (wk==1) print(lu0[[1]][[2]])  
    wvr <- getwaiver(lu0)
    pdb <- dowaiver(pdb,wvr,wk,lu0,tm)
    lu <- lapply(1:ntms,function(x) mklineup(pdb,wk,x))  # weekly lineup after applying the min-waiver  
    if (rule) {
      ix <- ifelse(wk==1,2,wk)
      mu <- unlist(lapply(levels(pdb$position),getposmean,wk=ix))
      sd <- unlist(lapply(levels(pdb$position),getpossd,wk=ix))
      dl <- lapply(lu,getruledrop,wk=wk,mu=mu,sd=sd,thresh=t)           # list of player ids to drop
      pl <- lapply(dl,getposnum)
      if (length(unlist(pl))>0) {
        pdb <- droplr(pdb,unlist(dl),wk,tm)
        rwvr <- sapply(pl,getrulewaiver)                     # make a new waiver matrix
        lu2 <- lapply(1:ntms,function(x) mklineup(pdb,wk,x))  
        pdb <- dowaiver(pdb,rwvr,wk,lu2,tm,0)
        lu <- lapply(1:ntms,function(x) mklineup(pdb,wk,x)) 
      }
    }
    b <- seq(1,length(schd),2)[wk]   # schedule index
    rcd <- domatchup(pdb,lu,schd[,b:(b+1)],rcd,wk)
  }
  modes <- sapply(1:nrow(pdb),function(x) getmode(pdb,x))
  pnames <- pdb[modes==tm,2]
  print(paste('SEASON',seas,'WINS',rcd[tm,1]))
  print('')
  return(list(wins=rcd[tm,1],pnms=pnames))
}

dosim <- function(n=100,tm,t=0,rl=1) {
  r <- list(); 
  for (i in 1:n) {
    r <- c(r,doseason(i,tm,t,rl))
    if ((i %% 2) == 0)
      dohist(r,i)
    }
  xs <- sort(unlist(r[seq(1,i*2,2)]),index.return=T)
  print('original starters:')
  print(lapply(lu0[[tm]][[1]],getfname))
  print('top 5 overall teams:')
  print(r[xs$ix[(i-5):i]*2])
  return(r)
}

dohist <- function(r,i) {
  ys <- unlist(r[seq(1,i*2,2)])
  hist(ys,xlab='wins',main=paste('Win count after',i,'simulations'))
}
# globals 
 # database of actual points
 adb <- read.csv('pointsdb.csv')
 # database of projections
 pdb <- read.csv('weekdb12.csv') 
 posnms <- c("QB","RB","WR","TE","DST","PK")
 #posix <- c(3,4,6,5,1,2)
 posndx <- c(5,6,1,2,4,3)
 posnmsF <- c(posnms,"FLEX")
 posns <- c(1,2,2,1,1,1)
 npos <- length(posns)
 schd0 <- matrix(c(1,2,3,4,9,5,6,7,8,10,1,2,4,6,7,10,3,5,8,9,2,3,5,1,4,8,9,6,7,10,
                  1,2,4,5,6,3,9,7,8,10,1,2,3,6,8,9,5,4,7,10,1,3,4,5,7,2,6,9,10,8,
                  1,2,3,5,6,4,10,8,7,9,1,2,3,7,8,6,4,5,10,9,1,2,3,4,5,8,7,10,6,9),nrow=5)
 schd <- cbind(schd0,schd0[,1:6])
 rcd <- matrix(0,10,2)
 ntms <- 10; seas <- 2012; of <- 17; wf <- 3; wk <- 1
 pdb <- mkrndtms(pdb)
 #pdb[,of+wk] <- pdb[,of+wk-1]
 lu0 <- lapply(1:ntms,function(x) mklineup(pdb,wk,x))  # weekly lineup  




