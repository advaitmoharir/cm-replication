
# Functions used in the replication exercise

optimal.iv<-function(y,x1,x2,...){
  n<-length(y)
  y<-matrix(y,n,1)
  x1<-matrix(x1,n,1)
  x2<-matrix(x2,n,1)
  #W<-as.matrix(W)
  X1<-x2#cbind(x2,W)
  Z1<-x1#cbind(x1,W)
  pi1<-solve(t(Z1)%*%X1)%*%t(Z1)%*%y
  pi2<-solve(t(X1)%*%Z1)%*%t(X1)%*%y
  e1<-y-X1%*%pi1
  e2<-y-Z1%*%pi2
  #What<-W%*%solve(t(W)%*%W)%*%t(W)
  K1<-solve(t(x1)%*%x2)%*%t(x1) #solve(t(x1)%*%x2-t(x1)%*%What%*%x2)%*%(t(x1)-t(x1)%*%What)
  K2<-solve(t(x2)%*%x1)%*%t(x2) #solve(t(x2)%*%x1-t(x2)%*%What%*%x1)%*%(t(x2)-t(x2)%*%What)
  b1<-K1%*%y
  b2<-K2%*%y; 
  v1<-K1%*%diag(as.vector(e1^2))%*%t(K1) #var(e1)*K1%*%t(K1)
  v2<-K2%*%diag(as.vector(e2^2))%*%t(K2) #var(e2)*K2%*%t(K2)
  c12<-K1%*%diag(as.vector(e2*e1))%*%t(K2) #cov(e1,e2)*K1%*%t(K2)
  lambda<-(v2-c12)/(v1+v2-2*c12)
  bopt<-lambda*b1+(1-lambda)*b2
  sb<-sqrt(lambda^2*v1+(1-lambda)^2*v2+2*lambda*(1-lambda)*c12)
  cl<-bopt-1.96*sb
  cu<-bopt+1.96*sb
  return(list(b1=b1,b2=b2,bopt=bopt,pi1=pi1,pi2=pi2,lambda=lambda,cl95=cl,cu95=cu, sb=sb))
}

ghat<-function(theta,X,...){
  # Moment conditions for the estimation problem (Tables 2 and 4)
  y<-X[,1];x1<-X[,2];x2<-X[,3]
  x1<-as.matrix(x1);x2<-as.matrix(x2)
  g1hat<-x1*(y-x2*as.numeric(theta)) # Moment condition 1 
  g2hat<-x2*(y-x1*as.numeric(theta)) # Moment condition 2 
  ghat<-cbind(g1hat,g2hat)
  return(ghat)
}

ghat2<-function(theta,X,...){
  # Moment conditions for the comparison of elasticities (Tables 3 and 5)
  x1<-as.matrix(X[,(ncol(X)-1)])
  x2<-as.matrix(X[,(ncol(X)-0)])
  Y<-X[,-c(ncol(X)-1,ncol(X))]
  X1<-matrix(x1,nrow=nrow(Y), ncol = ncol(Y),byrow = FALSE)
  X2<-matrix(x2,nrow=nrow(Y), ncol = ncol(Y),byrow = FALSE)
  g1hat <-X1*(Y-x2%*%t(theta)) # Moment conditions 1 and 2
  g2hat <-X2*(Y-x1%*%t(theta)) # Moment conditions 3 and 4
  ghat<-cbind(g1hat,g2hat)
  return(ghat)
}

replicate<-function(x){
  # takes a dataframe as input
  
  # Define empty list to store relevant output
  results<-list()
  
  # We estimate four specifications:
  # 1. OLS of 12 crime variables on UCR and ASG measures of sworn police
  # 2. IV with ASG measure as instrument ('forward') and UCR as instrument ('reflected')
  # 3. GMM estimated by stacking the two IV measures
  # 4. Optimal two sample IV as per Anderson and Moen (2016).
  
  # 1 and 2 are estimated using year and stateyear FE. #3 and #4 only use
  # stateyear FE. All variables are in growth rates (log-diffs).
  
  
  # The variables used below are defined as follows:
  # Y1-Murder, Y2-Rape, Y3-robbery, Y4-assault, Y5-burglary,
  # Y6-theft, Y7-motor vehicle theft, Y8-violent crimes,
  # Y9-property crimes, Y10-cost-weighted sum of violent crimes,
  # Y11- cost-weighted sum of property crimes, Y12-cost weighted sum of all
  # crimes, S-sworn officers (UCR), Z-sworn officers (ASG), C1-population (UCR),
  # C2- population (ASG), W-weights
  
  # All estimations are done using fixest package.
  
  #------------------OLS-------------------------------------------------
  
  # with ASG as independent var.
  results$ols.Z<-feols(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12)~Z+C1+C2|
                         sw(year,stateyear),data=x, weights=x$W, 
                       vcov = "hetero")
  # with UCR as independent var
  results$ols.S<-feols(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12)~S+C1+C2|
                         sw(year,stateyear), data=x, weights=x$W,
                       vcov = "hetero")
  #----------------------------IV---------------------------------
  # forward
  results$iv.forward<-feols(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12)~C1+C2|
                              sw(year,stateyear)|S~Z,data=x, 
                            weights=x$W, vcov = "hetero")
  # reflected
  results$iv.reflected<-feols(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12)~C1+C2|
                                sw(year,stateyear)|Z~S,data=x,
                              weights=x$W, vcov = "hetero")
  # --------------------------------GMM------------------------------------
  # to reduce number of moment conditions, we run residualized regressions
  # to filter out covariates via the Frisch-Waugh-Lovell Theorem
  
  # Defining residuals
  rY<-resid(feols(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12)~-1+C1+C2|
                    stateyear, weights=x$W,data=x))
  rS<-resid(feols(S~-1+C1+C2|stateyear, weights=x$W,data=x))
  rZ<-resid(feols(Z~-1+C1+C2|stateyear, weights=x$W,data=x))
  rSw<-sqrt(x$W)*rS;rZw<-sqrt(x$W)*rZ;rYw<-sqrt(x$W)*rY # residualized vars.
  
  # GMM function
  
  X0<-cbind(rYw,rSw,rZw) # binding residualized dep and indep vars

  # Define empty matrix, to be populated with GMM estimates for each of
  # 12 vars, and corresponding SE.
  GMM<-matrix(NA,12,3)
  for (i in 1:12){
    X <- X0[,c(i,13,14)]
    out0<-gmm(g=ghat,x=X,t0=-0.666,method="BFGS",vcov = "iid",type = "iterative")
    GMM[i,]<-c(out0$coefficients,sqrt(out0$vcov), 
               2*pnorm(abs(out0$coefficients/sqrt(out0$vcov)),lower.tail = FALSE))
  }
  colnames(GMM)<-c("estimate", "standard error", "p value")
  results$GMM<-round(GMM,digits = 3) # attaching results of GMM to the list
  #----------------------------Anderson-Moen-Optimal IV------------------
  
  # Same procedure as GMM 
  opt.iv<-matrix(NA,12,2)
  
  # Get parameters (bopt) and SEs(sb) top populate opt.iv
  for(i in 1:12){
    opt.iv0<-optimal.iv(X0[,i],X0[,13],X0[,14])
    opt.iv[i,]<-c(opt.iv0$bopt,opt.iv0$sb)
  }
  colnames(opt.iv)<-c("estimate", "standard error")
  results$opt.iv<-round(opt.iv, digits=3)
  return(results) # asks function to return all results
}

comparison <- function(x){
  # This function tests pairwise equality of elasticities of crime by stacking
  # the moment conditions associated with two crimes. The output is a dataframe
  # containing in each row a crime pair and the p value for the corresponding 
  # t test.
  
  # --------------------------------GMM------------------------------------
  # To reduce number of moment conditions, we run residualized regressions.
  # To filter out covariates via the Frisch-Waugh-Lovell Theorem.
  
  # Defining residuals
  ols.Y <- feols(c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12)~-1+C1+C2|
                   stateyear, weights=x$W,data=x)
  rY<-resid(ols.Y)
  k <- length(unique(x$stateyear))+2 # +2 because of C1 and C2
  n <- nrow(rY)
  rS<-resid(feols(S~-1+C1+C2|stateyear, weights=x$W,data=x))
  rZ<-resid(feols(Z~-1+C1+C2|stateyear, weights=x$W,data=x))
  # residualized vars
  rSw<-sqrt(x$W)*rS;rZw<-sqrt(x$W)*rZ;
  rYw<-sqrt(x$W)*rY[,1:9] # remove the cost-weighted variabls
  
  crimes<-c("Murder","Rape","Robbery","Assault","Burglary","Larceny",
            "Motor vehicle theft","Sum of violent crimes","Sum of property crimes")
  colnames(rYw)<-crimes
  
  out<-data.frame()
  for (i in 1:(length(crimes)-1)) {
    for (j in (i+1):length(crimes)) {

      if(((crimes[i]=="Murder" | crimes[i]=="Rape" | crimes[i]=="Robbery" |
           crimes[i]=="Assault") & crimes[j]=="Sum of violent crimes")==TRUE){next}
      if(((crimes[i]=="Burglary" | crimes[i]=="Larceny" | 
           crimes[i]=="Motor vehicle theft") & crimes[j]=="Sum of property crimes")==TRUE){next}
      crime.pairs <- c(crimes[i],crimes[j])
      X1<-rYw[,crime.pairs]
      X2<-cbind(rSw,rZw)
      X<-cbind(X1,X2)
      gmm.est<-gmm(g=ghat2,x=X,t0=rep(0.1,ncol(X1)),method="L-BFGS-B",
                   vcov = "MDS",type = "twoStep", centeredVcov=TRUE)

      # # Windmeijer correction 
      # gmm.est.1<- gmm(g=ghat2,x=X,t0=rep(0.1,ncol(X1)),method="L-BFGS-B",vcov="TrueFixed",weightsMatrix=diag(1,4))
      # V1 <- gmm.est.1$vcov
      # g1 <- gmm.est.1$gt
      # Omega1 <- crossprod(g1)/n
      # G1 <- gmm.est.1$G
      # V2 <- solve(t(G1)%*%solve(Omega1)%*%G1)/n
      # dg.1 <- -cbind(rSw*rZw,rSw*rZw,0,0); dg.2 <- -cbind(0,0,rSw*rZw,rSw*rZw)
      # dOmega.1 <- crossprod(g1,dg.1)/n+crossprod(dg.1,g1)/n
      # dOmega.2 <- crossprod(g1,dg.2)/n+crossprod(dg.2,g1)/n
      # g2 <- gmm.est$gt
      # D.1 <- V2%*%t(G1)%*%(solve(Omega1)%*%dOmega.1%*%solve(Omega1))%*%colMeans(g2)
      # D.2 <- V2%*%t(G1)%*%(solve(Omega1)%*%dOmega.2%*%solve(Omega1))%*%colMeans(g2)
      # D <- cbind(D.1,D.2)
      # Vw <- V1 + D%*%V2 + t(D%*%V2) + D%*%V1%*%t(D)
      # ################################

      v.HC1<- (n/(n-k))*gmm.est$vcov
      t.tests.HC1 <- (gmm.est$coefficients[1]-gmm.est$coefficients[2])/sqrt(v.HC1[1,1]+v.HC1[2,2]-2*v.HC1[1,2])
      t.tests <- (gmm.est$coefficients[1]-gmm.est$coefficients[2])/sqrt(gmm.est$vcov[1,1]+
                  gmm.est$vcov[2,2]-2*gmm.est$vcov[1,2])
      p.values <- as.matrix(round(2*pnorm(abs(t.tests),lower.tail = FALSE),digits = 3))
      p.values.HC1  <- as.matrix(round(2*pnorm(abs(t.tests.HC1),lower.tail = FALSE),digits = 3))
      out0<-c(crime.pairs,p.values,p.values.HC1 )
      out<-rbind(out,out0)
    }
  }
  colnames(out)<-c("crime 1","crime 2","p value","p value HC1")
  return(out)
  
}
