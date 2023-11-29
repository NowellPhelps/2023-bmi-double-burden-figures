## Spaghetti plot for age patterns

print('job started')
library(tidyverse)

outdir_folder    <- paste0(outdir, "Age trends/")
dir.create(outdir_folder, showWarnings = F)
indir            <- paste0(parentdir,"Model", modelnum, ", ", mod_dir_name, "/Results/",variable,"_",sex,"/")
covardir         <- paste0(parentdir, "Model", modelnum, ", ", mod_dir_name, "/Covariates/")

filename_save <- paste(paste0("Model",modelnum),sex,variable,sep="_")
com_name <- paste0("Model", modelnum, "_", sex, "_", variable, "_Combined.RDS")

# Load data
m0 <- modelnum
a <- readRDS(paste0(indir,com_name))
attach(a)
attach(a$subset)
attach(a$covar)
modelnum <- m0

filename <- paste0("Model", modelnum, "_", sex, "_", variable)

transform <- pnorm

axis_range <- c(0,1)
years <- c(plot.start.year, plot.end.year) - centring.year




############ (A) AGE SPECIFIC TRAJECTORIES AND ESTIMATES #######################

##### Baseline matrix of country-year estimates for middle age year-olds #########
print("Calculating baseline matrix")
baseline <- matrix(NA, length(burnt), T*J)
for(k in 1:J){
  print(k)
  
  country.val 				<- as.character(country.match$name[k])
  print(paste0('Country: ',country.val))
  
  country.val.num 		<- country.match$number[country.match$name==country.val]
  region.val 				  <- as.character(unique(covar$Region[covar$Country==country.val]))
  region.val.num 			<- region.match$number[region.match$name==region.val]
  sregion.val 				<- (unique(covar$Superregion[covar$Country==country.val]))
  sregion.val.num     <- sregion.match$number[sregion.match$name==sregion.val]
  
  #sregion coefficient matrix for estimates
  sregion_pred 				                  <- matrix(0, L, T)
  sregion.time_pred		                  <- matrix(0, L, T)
  sregion_pred[sregion.val.num,] 		    <- 1
  sregion.time_pred[sregion.val.num,] 	<- t
  
  #region coefficient matrix for estimates
  region_pred 					                <- matrix(0, K, T)
  region.time_pred 			                <- matrix(0, K, T)
  region_pred[region.val.num,] 		      <- 1
  region.time_pred[region.val.num,]     <- t
  region_pred				                    <- region_pred[multipleRegionsInSregion,]
  region.time_pred			                <- region.time_pred[multipleRegionsInSregion,]
  
  #region coefficient matrix for estimates
  country_pred 							            <- matrix(0, J, T)
  country.time_pred 		                <- matrix(0, J, T)
  country_pred[country.val.num,] 		    <- 1
  country.time_pred[country.val.num,] 	<- t
  
  F_pred <- rbind(rep(1, T), 		       # Maps to global intercept
                  t, 		               # Maps to global slope
                  sregion_pred, 		   # Maps to sregion intercept
                  sregion.time_pred,	 # Maps to sregion intercept
                  region_pred, 			   # Maps to region slope
                  region.time_pred,    # Maps to region intercept
                  country_pred,			   # Maps to country slope
                  country.time_pred,   # Maps to country intercept
                  matrix(0, N, T),     # SSRE maps - set to 0
                  matrix(0, 4, T),     # Subnational offsets
                  matrix(0, 4, T))     # Community offsets
  
  
  baseline[,((k-1)*T+1):(k*T)] <- theta[,] %*% F_pred +   # Linear component
    u[, country.time.match$country == country.val]  +     # nonlinear walk country component
    v[, region.time.match$region == region.val]     +     # nonlinear walk region component
    sv[, sregion.time.match$sregion == sregion.val] +     # nonlinear walk sregion component
    w[,]                                                  # nonlinear walk global component
  
}

print('Finished producing baseline matrix')

# Add age model terms to baseline means
StandardPop 			<- read.csv(paste0(covardir, standardpop.file.name)) ## age groups
age.vals 			    <- StandardPop$age_mean - middle.age

ageValMinus45Plus <- age.vals + middle.age - knot1
ageValMinus45Plus[age.vals < knot1 - middle.age] <- 0

ageValMinus60Plus <- age.vals + middle.age - knot2
ageValMinus60Plus[age.vals < knot2 - middle.age] <- 0

ageMat.val <- cbind(age.vals, age.vals^2, age.vals^3, ageValMinus45Plus^3, ageValMinus60Plus^3)

pdf(paste0(outdir_folder, filename, "_AgeTrends_byRegion.pdf"), height=8, width=36)
par(mfrow=c(2,8))
ylab.val <- variable

gamma.hat <- colMeans(gamma)

for (year in years) {
    for (sr in sregion_order_old) {
        plot(range(age.vals)+middle.age, axis_range, pch='',xlab='Age', ylab=ylab.val, main=paste(sr, '\n', variable, sex, year + centring.year))
        for(country.val in countrylistold$Country[countrylistold$Superregion==sr]) { 
            
            country.val.num <- country.match$number[country.match$name==country.val]
            region.val      <- as.character(country.names$Region[country.names$Country==country.val])
            region.val.num  <- region.match$number[region.match$name==region.val]
            sregion.val     <- as.character(country.names$Superregion[country.names$Country==country.val])
            sregion.val.num <- sregion.match$number[sregion.match$name==sregion.val]
            region.val.num  <- match(region.val.num, index_multiRinSR)
          
            y.hat <- mean(baseline[,country.time.match$country==country.val])
            
            R.age.val <- cbind(ageMat.val, matrix(0, length(age.vals), ng*J), matrix(0, length(age.vals), ng*Z), matrix(0, length(age.vals), ng*L))
            R.age.val[,ng + country.val.num+J*(0:(ng-1))] <- ageMat.val
            
            if (!is.na(region.val.num)) {
              R.age.val[,ng + J*ng + region.val.num + Z*(0:(ng-1))] <- ageMat.val
            }
            
            R.age.val[,ng +J*ng+Z*ng + sregion.val.num+L*(0:(ng-1))] <- ageMat.val
            
            
            
            lines(age.vals+middle.age,transform(y.hat + R.age.val %*% gamma.hat),col=sregion_col_old[sr])
        }
        
        y.hat <- mean(baseline[,country.time.match$country %in% unique(country)])
        R.age.val <- cbind(ageMat.val, matrix(0, length(age.vals),ng*J), matrix(0, length(age.vals),ng*Z), matrix(0, length(age.vals),ng*L))
        lines(age.vals+middle.age, transform(y.hat + R.age.val %*% gamma.hat), lwd=3)
        box()
    }
}
dev.off()
