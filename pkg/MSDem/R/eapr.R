eapr <- function(x, ass.edu.dt, maxeapr5) {
  x <- data.table(x)
  es <- levels(factor(x$edu))
  nedu <- length(es)
  
  #end of the period values for all eapr, education transitions, and ages
  eapr.t1 <- ass.edu.dt[areasex == unique(x$areasex) & period == unique(x$period) & edu == unique(edu), value]
  eapr.edu.t1 <- ass.edu.dt[areasex == unique(x$areasex) & period == unique(x$period) & edu == unique(edu), edu]
  eapr.age.t1 <- ass.edu.dt[areasex == unique(x$areasex) & period == unique(x$period) & edu == unique(edu), age]
  min.eapr.age <- min(eapr.age.t1)
  max.eapr.age <- max(eapr.age.t1)
  nage <- length(unique(eapr.age.t1))#ages with transitions
  
  #t0, t1, t2, t3 (projection time-step)
  pop.t0 <- data.table(x)
  pop.eduage.plus5.t0 <- pop.t0[age %in%  (min.eapr.age-5):max.eapr.age,]
  pop.eduage.plus5.t0 <- as.matrix(dcast(pop.eduage.plus5.t0,age~edu,value.var="pop1") )
  row.names(pop.eduage.plus5.t0) <- pop.eduage.plus5.t0[, 1]
#   #sanity check:
#   if (any(rowSums(pop.eduage.plus5.t0[, -1]) <= 0)) {
#        stop(c("In ", unique(pop.t0$period), ", there are no inhabitants of age ", 
# paste(names(which(rowSums(pop.eduage.plus5.t0[, -1]) <= 0)), collapse = ", "), " for pattern ", unique(pop.t0$areasex), ".\n
# Education transitions cannot be computed. Please check your base data for empty cells."))
#      }
  pop.eduage.plus5.t0[pop.eduage.plus5.t0 == 0] <- 1e-10
  rownames(pop.eduage.plus5.t0) <- pop.eduage.plus5.t0[,"age"] 
  pop.eduage.plus5.t0 <- pop.eduage.plus5.t0[,-1]#get rid of age line
  cumsum.rev <- function(df) rev(cumsum(rev(df)))
  cumpop.eduage.plus5.t0 <- t(apply(pop.eduage.plus5.t0, 1, cumsum.rev))
  eapr.eduage.plus5.t0 <- cumpop.eduage.plus5.t0[, -1] / cumpop.eduage.plus5.t0[, -6]
  
  temp1 <- matrix(0, nage, nedu-1) 
  rownames(temp1) <- seq(min.eapr.age-5,length=nage,by=5) #for each age-group transition from from e1. 
  colnames(temp1) <- eapr.edu.t1
  neweapr.t3 <- neweapr.t2 <- neweapr.t1 <- temp1
  
  #compare to maxeapr by residence (if available):
  maxeapr <- maxeapr5[grep(paste(names(maxeapr5), collapse = "|"), unique(x$areasex))]
  if (length(maxeapr) == 0L) maxeapr <- maxeapr5["total"]
  
  max.ages.tra = 3 

  eapr.t1[nedu-1] <- max(eapr.eduage.plus5.t0[nedu-1,nedu-1], min(maxeapr, eapr.t1[nedu-1]))#bounding projected eapr5   
  neweapr.t1[cbind(match(eapr.age.t1-5,rownames(neweapr.t1)),match(eapr.edu.t1,colnames(neweapr.t1)))] <- eapr.t1
  
  #t2
  eapr.t2 <- ass.edu.dt[areasex == unique(x$areasex) & period == unique(x$period) + 5 & edu == unique(edu), value] #1.03ms
  eapr.t2[nedu-1] <- max(eapr.eduage.plus5.t0[nedu-1, nedu-1], min(maxeapr, eapr.t2[nedu-1]))
  neweapr.t2[cbind(match(eapr.age.t1-5,rownames(neweapr.t1)),match(eapr.edu.t1,colnames(neweapr.t1)))] <- eapr.t2
  
  #t+15
  eapr.t3 <- ass.edu.dt[areasex == unique(x$areasex) & period == unique(x$period) + 10 & edu == unique(edu), value] #1.03ms
  eapr.t3[nedu-1] <- max(eapr.eduage.plus5.t0[nedu-1, nedu-1], min(maxeapr, eapr.t3[nedu-1]))
  neweapr.t3[cbind(match(eapr.age.t1-5,rownames(neweapr.t1)),match(eapr.edu.t1,colnames(neweapr.t1)))] <- eapr.t3
  
  neweaprs <- rbind(neweapr.t1, neweapr.t2, neweapr.t3)
  #for one step transition possibility
  for(edu in eapr.edu.t1){
    edu.pos <- match(edu,eapr.edu.t1)
    age1 <- eapr.age.t1[match(edu,eapr.edu.t1)]
    age1.pos <- match(age1,unique(eapr.age.t1))
    neweaprs[c((age1.pos+1):nage),edu.pos] = eapr.eduage.plus5.t0[c((age1.pos+1):nage),edu.pos]  
    if(age1.pos < 2) next
    ages.pre.tra <- age1.pos-1
    if(ages.pre.tra>(max.ages.tra-1)) ages.pre.tra = ages.pre.tra -1 
    for(age2 in ages.pre.tra:1){#number of ages to fill
      ult.val <- neweaprs[4*age2+age1.pos,edu.pos]
      val.age2.t0 <- eapr.eduage.plus5.t0[as.character(age1-5),edu.pos]
      val.age1.t0 <- eapr.eduage.plus5.t0[as.character(age1),edu.pos]
      neweaprs[4*(age2-1)+age1.pos-1,edu.pos] =  val.age2.t0 * ifelse(ult.val/val.age1.t0 > 1,1,ult.val/val.age1.t0)
      if(age2 == 2){
        ult.val <- neweaprs[4*(age2-1)+age1.pos-1,edu.pos]
        val.age2.t0 <- eapr.eduage.plus5.t0[as.character(age1-10),edu.pos]
        val.age1.t0 <- eapr.eduage.plus5.t0[as.character(age1-5),edu.pos]
        neweaprs[4*(age2-2)+age1.pos-2,edu.pos] =  val.age2.t0 * ifelse(ult.val/val.age1.t0 > 1,1,ult.val/val.age1.t0)
      }# age 
    }#for age2
    if(age1.pos>3) if(eapr.eduage.plus5.t0[as.character(age1-15),edu.pos]>0){
            neweaprs[age1.pos-3,edu.pos] <-eapr.eduage.plus5.t0[as.character(age1-15),edu.pos]}
    }# age 
  #}#edu

  neweapr.t1 <- neweaprs[1:nage,]
  cumedu.t1 <- rbind(rep(0, ncol(neweapr.t1)),rep(0, ncol(neweapr.t1)), t(apply(neweapr.t1, 1, cumprod)))#no edu-distribution among 10-14 years old
  cumedu.t1 <- cbind(rep(1, nrow(cumedu.t1)), cumedu.t1, rep(0, nrow(cumedu.t1)))
  diff.rev <- function(x) rev(diff(rev(x)))
  eduprop.aget0.t1 <- t(apply(cumedu.t1, 1, diff.rev))
  pop.0.29.aget0.t1 <- x[age %in% 0L:25L, .(popsum = sum(pop1)), by = age, nomatch = 0] #to make sure that the new 10-14 years (5-10, t0) have no edu distribution
  edupop.0.29.aget0.t1 <- data.table(age = rep(seq(0, 25, 5), ncol(eduprop.aget0.t1)), 
                     edu = rep(paste("e", 1:6, sep = ""), each = nrow(eduprop.aget0.t1)), 
                     pop2 = c(eduprop.aget0.t1 * pop.0.29.aget0.t1[, popsum])) #0.56ms
  pop.eduproj.t0 <- rbindlist(list(edupop.0.29.aget0.t1, x[age >= 30, .(age, edu, pop1), nomatch = 0L]), use.names = FALSE)
  x[pop.eduproj.t0, on = .(age, edu)] # 
}
