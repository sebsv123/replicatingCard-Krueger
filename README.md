# replicatingCard-Krueger
Short replication sheet code in R for Card and Krueger "Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania" (1994) for an Uni project (econometric class)
I tried my best having the exact same values as referred in Table 4, pg.780, noticing that, despite the difference between my replication and the real models is pretty small (regarding coefficients, sd...), 
is not an easy task emulating the model due the exact path the authors choosed is pretty detailed and is not clarified at all in the paper.
This replication got most of the non-easy details to be noticed in order to reach the exact same results, so this code is open for getting suggestions in order to modify and having a current valid R code able to screen this elegant work.
I also included an alternative model that takes in account some of the dependent variables are non-linear, while heteroskedacity was detected by White test in the original model and RESET test shows the current model is misspecified, so an alternative model is proposed considering these three factors, trying to solve them while not falling into overfitting.

# Load and cleaning data process from njmin file:
## njmin file got previously loaded in the directory, so the data migration to R got much easier
# libraries (reccomended ones):
library(whitestrap)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(car)
library(GGally)
library(lmtest)
library(MASS)
library(car)
library(tidyverse)
library(tidytuesdayR)
library(Transform)
library(gamlss)

# Data cleaning:
salarios <- read.table("public.dat"
)

salarios <- salarios %>% rename( CHAINr = V2, CO_OWNED = V3,                                                                  
                                   STATEr  = V4,                                                                   
                                   SOUTHJ   = V5,
                                   CENTRALJ    = V6,                                                               
                                   NORTHJ  = V7,                                                                     
                                   PA1   = V8,                                                                      
                                   PA2   = V9,                                                                       
                                   SHORE = V10,                                                                       
                                   NCALLS = V11,                                                                     
                                   EMPFT = V12,                                                                    
                                   EMPPT = V13,                                                                      
                                   NMGRS = V14,                                                                      
                                   WAGE_ST = V15,                                                                    
                                   INCTIME = V16,                                                                    
                                   FIRSTINC  = V17,                                                                  
                                   BONUS = V18,                                                                      
                                   PCTAFF = V19,                                                                      
                                   MEAL = V20,                                                                        
                                   OPEN  = V21, 
                                   HRSOPEN = V22,                                                                    
                                   PSODA = V23,                                                                      
                                   PFRY  = V24,                                                                      
                                   PENTREE = V25,                                                                    
                                   NREGS = V26,                                                                      
                                   NREGS11 = V27,                                                                    
                                   TYPE2 = V28,                                                                      
                                   STATUS2 = V29,                                                                    
                                   DATE2 = V30,                                                                      
                                   NCALLS2 = V31,                                                                    
                                   EMPFT2  = V32,                                                                    
                                   EMPPT2  = V33,                                                                    
                                   NMGRS2  = V34,                                                                    
                                   WAGE_ST2  = V35,                                                                  
                                   INCTIME2  = V36,                                                                  
                                   FIRSTIN2  = V37,                                                                  
                                   SPECIAL2  = V38,                                                                  
                                   MEALS2  = V39,                                                                    
                                   OPEN2R  = V40,                                                                    
                                   HRSOPEN2  = V41,  
                                   PSODA2   = V42,                                                                        
                                   PFRY2    = V43,                                                                        
                                   PENTREE2   = V44,                                                                      
                                   NREGS2   = V45,                                                                        
                                   NREGS112  = V46    )

salarios <- replace(salarios, salarios == ".", NA)
## Nueva matriz: Se definen las primeras variables de interés para el análisis
NJ <- as.numeric(sala$STATEr)
co_owned <- as.numeric(sala$CO_OWNED)
chain <- as.numeric(sala$CHAINr)
southj <- as.numeric(sala$SOUTHJ)
centralj <- as.numeric(sala$CENTRALJ)
pa1 <- as.numeric(sala$PA1)
pa2 <- as.numeric(sala$PA2)
nmngr <- as.numeric(sala$NMGRS)
nmngr2 <- as.numeric(sala$NMGRS2)
emppt <- as.numeric(sala$EMPPT)
emppt2 <- as.numeric(sala$EMPPT2)
empft <- as.numeric(sala$EMPFT)
empft2 <- as.numeric(sala$EMPFT2)
bk <- ifelse(chain==1, 1, 0)
kfc <- ifelse(chain==2, 2, 0)
roys <- ifelse(chain==3, 3, 0)
wendys <- ifelse(chain==4, 4, 0)
pa1 <- as.numeric(sala$PA1)
pa2 <- as.numeric(sala$PA2)
pentree <- as.numeric(sala$PENTREE)
pentree2 <- as.numeric(sala$PENTREE2)
vapentree <- pentree2-pentree
vapmeal <- vapsoda + vapfry + vapentree
FTE1 <- empft + nmngr + (emppt * 1/2)
FTE2 <- empft2 + nmngr2 + (emppt2 * 1/2)
FTE <- FTE2-FTE1 #Var Total Employment!!
gap <- ifelse(state==1 & wage_st <= 5.05, ((5.05-wage_st)/wage_st), 0) #Most efficient way to create gap variable
gap[is.infinite(gap)] <- NA

datosf <- as.data.frame(cbind(NJ, co_owned, chain, gap, FTE,
                              southj, centralj, pa1, pa2, bk, kfc, roys, wendys, vahrsopen, vaopen, vapentree, vapmeal))
df <- filter(datosf, chain != 4) #no incluyen a wendys en los modelos del paper, con esto se excluyen los datos de esta muestra
dat <- datosf

sala <- as.data.frame(cbind(salarios, NJ, vastwage, vaempft, vahrsopen, vaopen, vapsoda, vapfry,
                            vanregs,vanregs11, gap, FTE
                            ))

# matriz filtrada de datos:

datosf <- as.data.frame(cbind(NJ, co_owned, chain, gap, FTE,
                              southj, centralj, pa1, pa2, vapmeal))
df <- filter(datosf, chain != 4)
dat <- df

# modelo (i):

modelo_1 <- lm(FTE~NJ, data = dat)
summary(modelo_1)
# modelo(ii):

modelo_2 <- lm(FTE ~ NJ + chain + co_owned, data = df)
summary(modelo_2)
plot(modelo_2)

# modelo(iii):

modelo_3 <- lm(FTE ~ gap, data = df)
summary(modelo_3)
plot(modelo_3)

# modelo(iv):

modelo_4 <- lm(FTE ~ gap + bk + kfc + roys + co_owned, data = dat)
summary(modelo_4)
plot(modelo_4)
resettest(modelo_4)
# modelo(v):

modelo_5 <- lm(FTE ~ gap + bk + kfc + roys + pa1 + 
                pa2 + southj + centralj, data = dat)
summary(modelo_5)
plot(modelo_5)
resettest(modelo_5)
vif(modelo_5)

## Relevant dependent qnd independent variables distributions:

ggplot(data = df, aes(x = gap)) +
  geom_density(alpha = 0.5, fill = "gray50") +
  geom_rug(alpha = 0.2) +
  labs(title = "Distribución de GAP") +
  theme_bw()

ggplot(data = df, aes(x = FTE)) +
  geom_density(alpha = 0.5, fill = "lightgreen") +
  geom_rug(alpha = 0.2) +
  labs(title = "Distribución de empleo") +
  theme_bw() 

# Which distribution fits better the data?, according to GAIC:
distribuciones1 <- fitDist(
  dat$FTE,
  k = 2, # esta penalización equivale al AIC
  type = "realAll",
  trace = FALSE,
  try.gamlss = TRUE
)
wp(ampliado, ylim.all = 0.5)
distribuciones1$fits %>%
  enframe(name = "distribucion1", value = "GAIC") %>%
  arrange(GAIC)

print(distribuciones1$fits)
summary(distribuciones1) #!!

mod5_opt <- lm(FTE ~ gap  + bk + kfc + roys
               + pa1 + pa2 + southj + centralj, data = dat)

summary(mod5_opt)
resettest(mod5_opt) #problemas especificacion funcional
white_test(mod5_opt)
vif(mod5_opt) #rechazamos H0 al 35%, HAY HETEROSCEDASTICIDAD

# Linear model:
set.seed(99)
ampliado <- lm(FTE ~ gap  + bk + kfc + roys + pa1 + 
                 pa2 + southj + centralj +
                 vahrsopen + vapmeal, data = dat)

summary(ampliado)
vif(ampliado)
#vif supports no collinearity among variables. 
anova(ampliado) 
summary(ampliado)
plot(ampliado)

## Non-linear model:
view(dat$FTE)

ampliado_glm <- gamlss(
  formula = FTE ~ pb(gap) +  roys 
 + pa2 + southj  + vapmeal + pb(vahrsopen),
  family = SHASH(),
  data = na.omit(dat),
  trace = F, 
)
summary(ampliado_glm)
plot(ampliado_glm)
drop1(ampliado_glm, parallel = "multicore", ncpus = 2)
term.plot(ampliado_glm, pages = 1, ask = FALSE, rug = TRUE)  # Best way to interpretate the results, remarkable that gap has a non linear relation with FTE, and when higher the gap, lowest FTE increase.
     # relacion del logaritmo en media con el modelo

resettest(ampliado_glm) #still issue with functional form :( I would bet a polinomic version can solve this, but assuming overfitting is pretty easy to reach in that way (there are methods for avoiding that, not done here)
white_test(ampliado_glm) #heteroskedasticity got significantly reduced!









