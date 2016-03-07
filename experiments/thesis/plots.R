 



## exp1
load('/Users/sb/phaseExp.RData')
ggplot(expR,aes(x=ecc,y=r,color=id)) + geom_boxplot(na.rm=T,outlier.size=1) + xlab('degrees of eccentricity') + ylab('odd/even ratio') + geom_line(aes(y=c(1.2,2.2,6.2),x=c(1,4,6),col='Bennett & Banks',colour='black'))


cbPalette <- c("darkorange", "#56B4E9", "#009E73", "#CC0000", "#999999","#0072B2", "#CC79A7","#F0E442")

## exp4
p1 < ggplot() + geom_polygon(data=cdf,aes(x=c(lxs,rev(lxs)),y=c(upr,rev(lwr))),fill='gray60') + geom_line(aes(xs,sig),col='darkorange') + geom_line(aes(xs,cM),col='gray80') + ggtitle('cos 5 deg - 5k')
grid.arrange(p1,p2,p3,p4,f1,f2,f3,f4,nrow=2,heights=c(2,1))
dev.copy2pdf(file='s14_CI.pdf')


## polar plots
idl <- readCI(90,3)
out <- cmpCI(idl)
cM4 <- out[,1]
X.k <- fft(cM4)
df4 <- data.frame(amp=Mod(X.k),phz=Arg(X.k))
label=c('f','2f','3f','4f')
g4 <- ggplot(df4,aes(x=phz)) + stat_bin(fill='gray50') + geom_segment(aes(yend=amp[2:5]*7,x=phz[2:5],xend=phz[2:5],y=0,colour=label),size=1) + coord_polar() + opts(title='s21 sine 3 deg') 

## stim plots
xs <- seq(1,128)
cen <- median(xs)
w0 <- dnorm(xs,cen,54.35)
wt <- w0/max(w0)
f0 <- 4; w <- .15; sz <- length(xs); cen <- median(xs)
f <- (w*cos(f0*pi*((seq(1:sz)-cen)/sz)))*wt
f2c <- (w*cos(f0*2*pi*((seq(1:sz)-cen)/sz)))*wt
f2s <- (w*cos(f0*2*pi*((seq(1:sz)-cen)/sz)+pi/2))*wt

par(mfrow=c(1,4))

mc <- matrix(rep(f+f2c,128),nrow=128)
image(z = z <- mc, col  = gray((32:96)/128),xaxt='n',yaxt='n',main='f + 2f_cos')
lines(xs/128-(1/128),f+f2c+.5,col="white")
lines(xs/128-(1/128),f2c+.5,col="gray80",lty=2)
lines(xs/128-(1/128),f+.5,col="gray80",lty=2)

mcn <- matrix(rep(f-f2c,128),nrow=128)
image(z = z <- mcn, col  = gray((32:96)/128),xaxt='n',yaxt='n',main='f - 2f_cos')
lines(xs/128-(1/128),(f-f2c)+.5,col="white")
lines(xs/128-(1/128),f+.5,col="gray80",lty=2)
lines(xs/128-(1/128),.5-f2c,col="gray80",lty=2)

ms <- matrix(rep(f+f2s,128),nrow=128)
image(z = z <- ms, col  = gray((32:96)/128),xaxt='n',yaxt='n',main='f + 2f_sine')
lines(xs/128-(1/128),f+f2s+.5,col="white")
lines(xs/128-(1/128),f2s+.5,col="gray80",lty=2)
lines(xs/128-(1/128),f+.5,col="gray80",lty=2)

msn <- matrix(rep(f-f2s,128),nrow=128)
image(z = z <- msn, col  = gray((32:96)/128),xaxt='n',yaxt='n',main='f - 2f_sine')
lines(xs/128-(1/128),f-f2s+.5,col="white")
lines(xs/128-(1/128),.5-f2s,col="gray80",lty=2)
lines(xs/128-(1/128),f+.5,col="gray80",lty=2)


## stim 2
hist(f+f2c,xlab='contrast',ylab='frequency',main='f + 2f_cos',xlim=c(-.3,.3),ylim=c(0,30),border='gray40');
hist(f[1:64]+f2c[1:64],add=TRUE,col='gray70',border='gray40'); box(col='gray60')
hist(f-f2c,xlab='contrast',ylab='',main='f - 2f_cos',xlim=c(-.3,.3),ylim=c(0,30),border='gray40');
hist(f[1:64]-f2c[1:64],add=TRUE,col='gray70',border='gray40'); box(col='gray60')
ret <- hist(f+f2s,xlab='contrast',ylab='',main='f + 2f_sine',xlim=c(-.3,.3),ylim=c(0,30),border='gray40');
hist(f[1:64]+f2s[1:64],add=TRUE,col='gray70',border='gray40',breaks=ret$breaks); box(col='gray60')
hist(f-f2s,xlab='contrast',ylab='',main='f - 2f_sine',xlim=c(-.3,.3),ylim=c(0,30),border='gray40');
hist(f[1:64]-f2s[1:64],add=TRUE,col='gray70',border='gray40',breaks=ret$breaks); box(col='gray60')
legend('topright',c('left','right'),border='gray40',fill=c('gray100','gray70'),inset=.05)

## stim 2q
hist(f+f2c,xlab='',ylab='frequency',main='f + 2f_cos',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]+f2c[1:64],add=TRUE,col='gray70',border='gray40'); box(col='gray60')
hist(f-f2c,xlab='',ylab='',main='f - 2f_cos',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]-f2c[1:64],add=TRUE,col='gray70',border='gray40'); box(col='gray60')
ret <- hist(f+f2s,xlab='contrast',ylab='frequency',main='f + 2f_sine',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]+f2s[1:64],add=TRUE,col='gray70',border='gray40',breaks=ret$breaks); box(col='gray60')
hist(f-f2s,xlab='contrast',ylab='',main='f - 2f_sine',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]-f2s[1:64],add=TRUE,col='gray70',border='gray40',breaks=ret$breaks); box(col='gray60')
legend('topright',c('left','right'),border='gray40',fill=c('gray100','gray70'),inset=.05)

## stim 2q (revised)
par(mfrow=c(2,2), mar=c(3.5,3.2,2,2), mgp = c(2.2,1,0) )
hist(f+f2c,xlab='',ylab='frequency',main='f + 2f_cos',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]+f2c[1:64],add=TRUE,col='gray70',border='gray40'); box(col='gray60')
hist(f-f2c,xlab='',ylab='',main='f - 2f_cos',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]-f2c[1:64],add=TRUE,col='gray70',border='gray40'); box(col='gray60')
ret <- hist(f+f2s,xlab='contrast',ylab='frequency',main='f + 2f_sine',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]+f2s[1:64],add=TRUE,col='gray70',border='gray40',breaks=ret$breaks); box(col='gray60')
hist(f-f2s,xlab='contrast',ylab='',main='f - 2f_sine',xlim=c(-.3,.3),ylim=c(0,30),border='gray40',las=1,cex.axis=.9);
hist(f[1:64]-f2s[1:64],add=TRUE,col='gray70',border='gray40',breaks=ret$breaks); box(col='gray60')
legend('topright',c('left','right'),border='gray40',fill=c('gray100','gray70'),inset=.05)

## consist plot
sub[,5] <- (2*sub$V4/sqrt(sub$V4))/100
ggplot() + geom_errorbar(aes(x=clvl,ymin=pc-V5,ymax=pc+V5),width=.04,color='gray80',data=sub) + geom_line(aes(xs,psym),col='gray60',data=psyR) + opts(title='cos 0 deg - 2k',background='white') + scale_x_log10() + xlab('contrast') + ylab('pc') + geom_point(aes(x=clvl,y=pc),data=sub,color='gray60')

## stim 2 revised
pcA <- f+2fc
ncA <- f-f2c
psA <- f+f2s
nsA <- f-f2s
pc <- data.frame(contrast=pcA,side=as.factor(c(rep('left',64),rep('right',64))),tb=as.factor(ifelse(pcA>0,'top','bottom')))
nc <- data.frame(contrast=ncA,side=as.factor(c(rep('left',64),rep('right',64))),tb=as.factor(ifelse(ncA>0,'top','bottom')))
ps <- data.frame(contrast=psA,side=as.factor(c(rep('left',64),rep('right',64))),tb=as.factor(ifelse(psA>0,'top','bottom')))
ns <- data.frame(contrast=nsA,side=as.factor(c(rep('left',64),rep('right',64))),tb=as.factor(ifelse(nsA>0,'top','bottom')))

p1 <- ggplot(pc,aes(x=contrast)) + geom_density(aes(color=side)) + scale_color_manual(values=c("darkorange", "#56B4E9", "#009E73", "#CC0000")) + opts(title='f + 2f_cos', panel.background=theme_rect(fill='white'),legend.position="none") + xlim(c(-.25,.25))
p2 <- ggplot(nc,aes(x=contrast)) + geom_density(aes(color=side)) + scale_color_manual(values=c("darkorange", "#56B4E9", "#009E73", "#CC0000")) + opts(title='f - 2f_cos', panel.background=theme_rect(fill='white'),legend.position=c(.4,.7),legend.key.size=unit(10,"points")) + xlim(c(-.25,.25))
p3 <- ggplot(ps,aes(x=contrast)) + geom_density(aes(color=side)) + scale_color_manual(values=c("darkorange", "#56B4E9", "#009E73", "#CC0000")) + opts(title='f + 2f_sine', panel.background=theme_rect(fill='white'),legend.position="none")
p4 <- ggplot(ns,aes(x=contrast)) + geom_density(aes(color=side)) + scale_color_manual(values=c("darkorange", "#56B4E9", "#009E73", "#CC0000")) + opts(title='f - 2f_sine', panel.background=theme_rect(fill='white'), legend.position="none")
grid.arrange(p1,p2,p3,p4)


## simulation
w10 <- c(.1148,4.07,.0134,75.91,.0358)
w2 <- c(.076512,1.075522,.002744,141.52,.033)
disc <- c(.2,3502,.0512,1.106,.005834)
sim <- data.frame(rbind(cbind(dblexp(disc),fct[1]),cbind(dblexp(w2),fct[2]),cbind(dblexp(w10),fct[3])))
names(sim) <- c('ie','task')
sim$task <- as.factor(sim$task)
levels(sim$task) <- c('1-of-10 ID','1-of-2 ID','PR Discrim')
ggplot(sim) + geom_line(aes(x=kie,y=ie,group=task,color=task)) + opts(title='ideal observer + int noise',panel.background=theme_rect(fill='white'),legend.position=c(.7,.7) + scale_color_manual(values=c("darkorange", "#56B4E9", "#009E73"))


## ie ratio
dfI <- data.frame(
ie = c(11.7923,11.1992,6.2699,4.5757,0.39996,1.0565,2.5165,3.3669,4.3597, 4.1462, 3.055, 2.6989, 0.56323, 1.1799, 1.9401, 2.1201),
ie.sd = c(7.0879,5.9976,3.5258,2.4305,0.054669,0.38426,1.5624,1.9399,1.7828, 1.3534, 1.0716, 0.90983, 0.078231, 0.28688, 0.81014, 0.80255),
sid = c(14,14,14,14,21,21,21,21,14,14,14,14,21,21,21,21),
ecc = c(0,5,0,5,0,3,0,3,0,5,0,5,0,3,0,3),
theta = c(0,0,90,90,0,0,90,90,0,0,90,90,0,0,90,90),
cond = as.factor(c(rep('PR',8),rep('ID',8))))

ggplot(dfI,aes(x=ecc,y=ie,group=cond,color=interaction(theta,cond))) + geom_errorbar(aes(ymin=ie-ie.sd,ymax=ie+ie.sd),width=.5) + facet_grid(. ~ sid) + ylab('ie') + scale_color_manual(values=c("darkorange", "#56B4E9", "#009E73","#CC0000")) + opts(legend.position=c(.8,.7)) + geom_point() + geom_line(aes(group=interaction(theta,cond)),lty=2)

plot(xs,f+f2c,type="n",ylim=c(-.3,.3),yaxt='n',ylab='',xlab='',xaxt='n',main='cosine pair')
polygon(x=c(xs,rev(xs)),y=c(f+f2c,rev(f-f2c)),angle=45,density=7,col='gray60')


ggplot(ds) + geom_line(aes(x=x,y=values,group=cond,linetype=cond, color=cond),size=.8) + scale_linetype_manual(values=c(rep(1:2,4))) + scale_color_manual(values=colz) + xlab('') + ylab('') + theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(), panel.background = element_rect(fill = "white"))
