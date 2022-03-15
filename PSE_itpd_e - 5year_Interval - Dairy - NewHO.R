rm(list=ls())

library(haven)
library(dplyr)
library(tidyr)
library(fixest)
library(readxl)
#library(DataCombine)


#loaddata -----

milk <- read_stata("chapterone_dairydata.dta")


unique(milk$iso3_o)
unique(milk$iso3_d)
unique(milk$year)
unique(milk$industry_id)
unique(milk$industry_descr)

milk <- milk%>%filter(industry_id==38)

### 5-year interval ---------------------------
yint <- 5
yearres <- seq(2001,2016,yint);yearres

# fixed effects ------------------------------------------- 

dairy <- milk%>%
  filter(year%in%yearres)%>%
  mutate(timeexp   = paste(year,iso3_o, sep=""),                 # time-export FE for milk
         timeimp   = paste(year,iso3_d,sep="" ),                 # time-import FE for milk
         expimp    = paste(iso3_o,iso3_d,sep=""),                # export-import FE for milk
         bfe       = 1*(iso3_d==iso3_o),                         # domestic border FE
         bfen      = 1*(iso3_d!=iso3_o),                         # international border FE
         lpsebfe   = log(pse+0.0001)*bfe,                        # domestic producer subsidy FE
         lpsebfen  = log(pse+0.0001)*bfen,                       # international producer subsidy FE
         capland   = capital_cur_o/agland,
         lcapital_cur_o_perland = log(capland),
         lgoveffect = log(goveffect+2.5),
         laccountability = log(accountability+2.5))%>%
  rename(wto_joint = member_wto_joint,
         eu_joint  = member_eu_joint)

#size factors------------------
ValProd <- milk%>%
  group_by(year,iso3_o)%>%
  summarise(ValProd = sum(trade))


ValCons <- milk%>%
  group_by(year,iso3_d)%>%
  summarise(ValCons = sum(trade))

dairy <- left_join(dairy,ValProd, by = c("year","iso3_o"))
dairy <- left_join(dairy,ValCons, by = c("year","iso3_d"))

milk_data <- dairy%>%mutate(lValProd   = log(ValProd),
                            lValCons   = log(ValCons))


# Here I am dropping outliers and replacing them with means
#milk_data$capland[(milk_data$capland)>1]<-mean(milk_data$capland, na.rm = T)

#milk_data <- milk_data%>%
#  mutate(lcapital_cur_o_perland = log(capland))


#keep2 <- c('iso3_o','year','capland')
#check2 <- milk_data[keep2]

#summary(check2$capland)

### regressions ----------------------------------
gravvar <- c("trade","lndist")
gravvar1 <- paste(gravvar,collapse = "~")
model <- formula(paste(gravvar1,"wto",sep = "+"))
model
class(model)

#table 2, column 1 ---------
formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint|             # Policy Factors
                     expimp+iso3_d+iso3_o+year)
policy <- femlm(fml=formula,data = milk_data,family = c("poisson"))
policyS <- summary(policy)
round(policyS$coeftable,4)

formula <- trade~
  lpsebfe+agree_fta+eu_joint|             
  expimp+iso3_d+iso3_o+year

class(formula)

#table 4, column 2 ---------
formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland|        # HO Factors
                     expimp+iso3_d+iso3_o+year)
pol_ho <- femlm(fml=formula,data = milk_data,family = c("poisson"))
pol_hoS <- summary(pol_ho)
round(pol_hoS$coeftable,4)

#table 4, column 3 ---------
formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+
                     lValProd+lValCons|             # Gravity Factors
                     expimp+iso3_d+iso3_o+year)
pol_ho_size <- femlm(fml=formula,data = milk_data,family = c("poisson"))
pol_ho_sizeS <- summary(pol_ho_size)
round(pol_ho_sizeS$coeftable,4)


#table 4, column 4-8---------
#Institutional Factor Regression
formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+lgoveffect|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)


formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+
                     laccountability|                      # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
voice <- femlm(fml=formula,data = milk_data,family = c("poisson"))
voiceS <- summary(voice)
round(voiceS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+
                     legalsystempropertyrights|                      # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
rights <- femlm(fml=formula,data = milk_data,family = c("poisson"))
rights <- summary(rights)
round(rights$coeftable,4)


formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+
                     accountability+
                     legalsystempropertyrights|                      # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
both <- femlm(fml=formula,data = milk_data,family = c("poisson"))
both <- summary(both)
round(both$coeftable,4)



ResultFcn <- function(Resultf){
  temp1 <- round(cbind(Resultf$coeftable[,1]),3)
  temp2 <- paste('(',round(cbind(Resultf$coeftable[,2]),3),')',sep = "")
  temp4 <- cbind(Resultf$coeftable[,4])
  temp5 <- cbind(rownames(Resultf$coeftable))
  n <- length(temp1)
  
  seqn <- 1:n
  sefcn <- function(sf){
    sym=ifelse(temp4[sf]<=0.01,"***",
               ifelse(temp4[sf]<=0.05,"**",
                      ifelse(temp4[sf]<=0.10,"*","")))
    return(paste(temp1[sf],sym,sep = ""))
  }
  temp14 <- sapply(seqn,sefcn)
  
  seqtop <- seq(1,2*n,2)
  seqbot <- seq(2,(2*n+1),2)
  temp14 <- cbind(temp14,seqtop)
  temp2 <- cbind(temp2,seqbot)
  temp5 <- rbind(temp5,cbind(rep("S.E.",n)))
  
  temp <- cbind(temp5,rbind(temp14,temp2))
  
  torder <- order(as.numeric(c(temp[,3])))
  temp <- temp[torder,1:2]
  
  rbind(temp,"BLANK",Resultf$nobs,round(Resultf$pseudo_r2,3),Resultf$message)
}


result1 <- ResultFcn(policyS)
write.csv(result1, file = "Result1.csv")

result2 <- ResultFcn(pol_hoS)
write.csv(result2, file = "Result2.csv")

result3 <- ResultFcn(pol_ho_sizeS)
write.csv(result3, file = "Result3.csv")

result4 <- ResultFcn(goveffS)
write.csv(result4, file = "Result4.csv")

result5 <- ResultFcn(voiceS)
write.csv(result5, file = "Result5.csv")














































milk_data$legalsystempropertyrights


#Institutional Factor Regression
formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+transfersandsubsidies|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+protectionofpropertyrights|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+legalsystempropertyrights|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+regulatorytradebarriers|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+capitalcontrols|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+freedomtotradeinternationally|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+hiringandfiringregulations|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+labormarketregulations|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+regulation|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

formula <- formula(trade~
                     lpsebfe+agree_fta+eu_joint+
                     lcapital_cur_o_perland+        # HO Factors
                     lValProd+lValCons+legalsystempropertyrights|                        # Institutional Factors
                     expimp+iso3_d+iso3_o+year)
goveff <- femlm(fml=formula,data = milk_data,family = c("poisson"))
goveffS <- summary(goveff)
round(goveffS$coeftable,4)

