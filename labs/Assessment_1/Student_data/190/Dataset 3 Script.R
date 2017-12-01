library(gvlma)
library(rcompanion)
### Dataset 3: HIV viral load and within-patient population dynamics ###
# setwd() to the directory you wish to work from and start
Dataset3 <- read.table("190_part3.tdf", header = T)
str(Dataset3)
attach(Dataset3)
plotNormalHistogram(viral_load) # No skew on data, normally distributed
boxplot(viral_load~Tissue, xlab="Tissue", ylab="Viral Load", col="steelblue") # One outlier found in brain results

BackwardsModel <- step(lm(viral_load~Tissue*Distance*CD4*Shannon_diversity), direction="backward")
ForwardModel <- step(lm(viral_load~Tissue*Distance), 
                     scope = (~Tissue*CD4*Shannon_diversity*Distance), direction="forward")
# One of many forward/backwards combinations, these ones produced the best values

FinalModel1 <- step(lm(viral_load~Tissue*Distance), 
                    scope=c(lower=~Distance,upper=~Tissue*Distance*CD4*Shannon_diversity), 
                    direction="both") # Best model for producing lowest AIC values
drop1(FinalModel1, test="F") # p-value 0.03266
gvlma::gvlma(FinalModel1) # All assumptions are satisfied 
par(mfrow=c(2,2)) 
plot(FinalModel1) # FinalModel1 has an acceptable fit
par(mfrow=c(1,1))
detach(Dataset3)
