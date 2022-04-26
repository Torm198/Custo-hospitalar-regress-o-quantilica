require(tidyverse)
require(quantreg)
require(rms)


banco <- read_csv('insurance.csv') %>% mutate(across(where(is.character),factor))


rq(charges~.,data = banco,tau=0.025)$coef



qs <- seq(0.01,0.99,by=0.01)

a <- length(qs)
b <- 9
CL <- matrix(NA,a,b)
LS <- matrix(NA,a,b)
LI <- matrix(NA,a,b)
pv <- matrix(NA,a,b)


for(k in 1:length(qs)){
  reg_quant <- Rq(charges~.,x=T,y=T,data = banco,tau=qs[k])
  
  
  LI[k,] <- reg_quant$coefficients-1.96*reg_quant$summary[,2]
  CL[k,] <- reg_quant$coefficients
  LS[k,] <- c(reg_quant$coefficients)+1.96*reg_quant$summary[,2]
  pv[k,] <- reg_quant$summary[,4]
  
}

nome <- names(reg_quant$coefficients)


tratamento <- function(x,nomea){
  x <- data.frame(x)
  names(x) <- nome
  x$quantil <-qs 
  
  x <- x%>% pivot_longer(-quantil, names_to = 'beta', values_to=nomea)
  return(x)
}




resultado <- left_join(tratamento(LI,'inferior'),tratamento(CL,'pontual')) %>%
  left_join(tratamento(LS,'superior')) %>%
  left_join(tratamento(pv,'pvalor'))

ggplot(resultado,aes(x=quantil,y=pontual))+
  geom_line()+
  geom_ribbon(aes(ymin=inferior,ymax=superior),alpha=0.2)+
  facet_wrap(~beta)
ggplot(resultado,aes(x=quantil,y=pvalor))+
  geom_line()+
  facet_wrap(~beta)


