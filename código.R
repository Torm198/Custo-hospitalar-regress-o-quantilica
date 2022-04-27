require(tidyverse)
require(quantreg)
require(rms)


banco <- read_csv('insurance.csv') %>% mutate(across(where(is.character),factor))

########exploratório#######
ggplot(banco,aes(x=age,y=charges))+geom_point()
ggsave('age.pdf',device = 'pdf')
ggplot(banco,aes(x=sex,y=charges))+geom_point()
ggsave('sex.pdf',device = 'pdf')
ggplot(banco,aes(x=bmi,y=charges))+geom_point()
ggsave('bmi.pdf',device = 'pdf')
ggplot(banco,aes(x=children,y=charges))+geom_point()
ggsave('children.pdf',device = 'pdf')
ggplot(banco,aes(x=smoker,y=charges))+geom_point()
ggsave('smoker.pdf',device = 'pdf')
ggplot(banco,aes(x=region,y=charges))+geom_point()
ggsave('region.pdf',device = 'pdf')




##########################

#funções essenciais para o pseudoR
#função de perda
rho <- function(u,tau=.5)u*(tau - (u < 0))

#soma das perdas
Vget <- function(x){
  sum(rho(x$residuals,x$tau))
}



qs <- seq(0.01,0.99,by=0.01)

a <- length(qs)
b <- 9
CL <- matrix(NA,a,b)
LS <- matrix(NA,a,b)
LI <- matrix(NA,a,b)
pv <- matrix(NA,a,b)
pseudo_r <- numeric(a)



#loop para estimar todo o espectro quantilico


for(k in 1:length(qs)){
  reg_quant <- Rq(charges~.,x=T,y=T,data = banco,tau=qs[k])
  reg_nsat <- Rq(charges~1,x=T,y=T,data = banco,tau=qs[k])
  
  LI[k,] <- reg_quant$coefficients-1.96*reg_quant$summary[,2]
  CL[k,] <- reg_quant$coefficients
  LS[k,] <- c(reg_quant$coefficients)+1.96*reg_quant$summary[,2]
  pv[k,] <- reg_quant$summary[,4]
  pseudo_r[k] <-1 - Vget(reg_quant)/Vget(reg_nsat)
}

nome <- names(reg_quant$coefficients)

# função abaixo recebe uma matriz de resultado para tratar numa forma que facilite a criação de gráficos
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
  facet_wrap(~beta,scales = 'free_y')
ggsave('estimativas_sem_escala.pdf',device='pdf')
ggplot(resultado,aes(x=quantil,y=pontual))+
  geom_line()+
  geom_ribbon(aes(ymin=inferior,ymax=superior),alpha=0.2)+
  facet_wrap(~beta)
ggsave('estimativas_com_escala.pdf',device='pdf')

ggplot(resultado,aes(x=quantil,y=pvalor))+
  geom_line()+
  facet_wrap(~beta)
ggsave('pvalor.pdf',device='pdf')

ggplot(data=NULL,aes(x=qs,y=pseudo_r))+geom_line()
ggsave('pseudoR.pdf',device='pdf')




# predict intervalo de 95%


inf <- Rq(charges~.,x=T,y=T,data = banco,tau=0.025)
sup <- Rq(charges~.,x=T,y=T,data = banco,tau=0.975)

novo_dado <- data.frame(age=23,
                        sex='male',
                        bmi=23.5,
                        children=0,
                        smoker='no',
                        region='southeast')

predict(inf,novo_dado)
predict(sup,novo_dado)


