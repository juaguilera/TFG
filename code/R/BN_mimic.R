library(readr)
library(arules)
library(bnlearn)

setwd('C:/Users/julia/Documents/MatCAD/MatCAD-4/TFG/Dades/mimic-iii-clinical-database-1.4/')
dataset_1_row_per_pacient <- read_csv("./data/output/dataset_1_row_per_pacient_v3.csv")

colnames(dataset_1_row_per_pacient)

df <- as.data.frame(subset(dataset_1_row_per_pacient, select=-c(...1, subject_id, DRG_type, DRG_code)))

df <- df[complete.cases(df), ]

df.original <- df

# DICRETIZE VARIABLES

# SEX
table(df[[1]])  # sex. Correcte i relativament equilibrat. OK

# WEIGHT
summary(df$weight)

library(ggplot2)
ggplot(data = df, mapping = aes(x = weight))+      
  geom_histogram(              
    color = "red",              
    fill = "blue")   

boxplot(df$weight,
        ylab = "weight"
)


error.weight<-which(df$weight>400 | df$weight<30)  

## per sota, com que l'edat minima es de 15 anys, no podem suposar bebes i, per tant,
## agafem, per exemple, 30 kg. 

df<-df[-error.weight,]

summary(df[[2]])

ggplot(data = df, mapping = aes(x = weight))+      
  geom_histogram(              
    color = "red",              
    fill = "blue")   

boxplot(df$weight,
        ylab = "weight"
)


## en aquest cas, els talls s'han de posar per consideracions mèdiques, i podrien
## ser: 55, 75 i 95 Kg., tot i que no 
## tenim l'alçada (seria per a 1.75 m. aprox) i s'hauria de fer amb el IMC, 
## pero vaja, ja anira prou be:

df$weight <- arules::discretize(df$weight, method="fixed", breaks=c(0,55, 75, 95,Inf), 
                                labels=c("underweight", "healthy", "overweight", "obese"))
table(df$weight)

plot(df$weight,df.original$weight[-error.weight])


# AGE 

summary(df$age)

library(ggplot2)
ggplot(data = df, mapping = aes(x = age))+      
  geom_histogram(              
    color = "red",              
    fill = "blue")   

boxplot(df$age,
        ylab = "age"
)

## en aquest cas, els talls s'han de posar per consideracions demografiques i mediques, 
## i podrien ser
## ser: 40, 60 i 75 Kg., encara que depen del context, perque hi ha pocs joves

df$age <- arules::discretize(df$age, method="fixed", breaks=c(0,40,60,75,Inf), 
                             labels=c("young", "young adult", "adult", "senior"))
table(df$age)

plot(df$age,df.original$age[-error.weight])

# ADMISSION_TYPE

table(df$admission_type)   # queda una mica descompensada, perque URGENT te nomes 163 casos. 
# es pot deixar, o ajuntar amb EMERGENCY. 
# He decidit deixar-ho fins a veure les conclusions
# que es poden treure si tenen sentit.

# ADMIT_LOCATION

table(df$admit_location) # aqui hi ha dues categories molt desequilibrades que hauries d'ajuntar 
# amb alguna de les altres, si te sentit, o eliminar els casos (en total 
# son 26 + 1 = 27 casos

admit_location.minor.cases<-which(df$admit_location=="TRSF WITHIN THIS FACILITY" | df$admit_location=="TRANSFER FROM SKILLED NUR")  
print(admit_location.minor.cases)
df<-df[-admit_location.minor.cases,]

# DRG_CODE

#table(df$DRG_code)  # o be ajuntem codis en unes quantes categories, o aquesta variable no te
# cap sentit com a variable predictora. En el que segueix l'eliminare
# de les variables predictores. L'ideal seria l'altra opcio.
# No sera que t'has confos i has deixat aquesta per comptes de DRG_type???

# DIAGNOSIS

table(df$diagnosis)  # idem que l'anterior. AQUESTA SI QUE ES MOLT IMPORTANT TENIR-LA CATEGORITZADA

# DIAGNOSIS_SEVERITY

table(as.factor(df$diagnosis_severity))  # ok

# DIAGNOSIS_MORTALITY

table(as.factor(df$diagnosis_mortality))  # ok

# RESPIRATORY_SERVICES

table(as.factor(df$respiratory_services))  # ok

# CURRENT_CAREUNIT

table(df$current_careunit)  # ok

# LOS_X  (temps d'estada a la UCI, en hores)

df$LOS_x <- df$LOS_x / 24 # Passem de hores a dies.
summary(df$LOS_x)

length(which(df$LOS_x<1))
length(which(df$LOS_x<=2 & df$LOS_x>=1))
length(which(df$LOS_x<=4 & df$LOS_x>2))
#length(which(df$LOS_x<4 & df$LOS_x>=3))
length(which(df$LOS_x>=4))


df$LOS_x <-arules::discretize(df$LOS_x, method="fixed", breaks=c(0,1,3,4,Inf), 
                              labels=c("0-1-days", "1-2-days", "3-4-days", "more-4-days")) 

table(df$LOS_x)  

# LOS_Y  (temps d'estada a l'hospital, incloent la UCI, en hores)

summary(df$LOS_y)

df$LOS_y <- df$LOS_y /24 # Passem de hores a dies.
#df$LOS_y <- df$LOS_y / (24 * 7) # Passem de hores a setmanes.
summary(df$LOS_y)

length(which(df$LOS_y<4))
length(which(df$LOS_y<7 & df$LOS_y>=4))
length(which(df$LOS_y<7 & df$LOS_y>=5))
length(which(df$LOS_y<14 & df$LOS_y>=7))
length(which(df$LOS_y>=14))

df$LOS_y <- arules::discretize(df$LOS_y, method="fixed", breaks=c(0,4,7,14,Inf), labels=c("0-3-days", "4-7-days", "1-2-week", "more-2-weeks"))
table(df$LOS_y)

# DISCHARGE_LOCATION
table(df$discharge_location)    # algunes categories amb pocs valors, pero de moment ho deixem

# IS_DEAD
table(df$is_dead) 

table(df$discharge_location,df$is_dead)

############. ROSARIO: aquesta no es la variable que vam parlar (mail meu data 13/5/24), perque tal i com
############. esta nomes ens diu si va morir a l'hospital, cosa que ja ens diu discharge_location, pero no
############. si va morir fora de l'hospital. Malgrat tot, segueixo com si fos el que tocava, per a veure
############. com fas el procediment i per que et dona error.
##### Canvi fet

# VARIABLES AS FACTOR
fac_cols <- c("gender", "admission_type", "admit_location", 
              # "DRG_code",
              "diagnosis", 
              "diagnosis_severity",
              "diagnosis_mortality", "respiratory_services", "current_careunit", 
              "LOS_x", "LOS_y", # aquestes dues no cal, perque ja son factor
              "discharge_location",
              "is_dead")

df[,fac_cols] <- lapply(df[,fac_cols], as.factor)

str(df)

################################
# M1 - CLASSIFICACIÓ MULTILABEL
################################

outputs <- c("respiratory_services", "current_careunit", "LOS_x", "LOS_y", "discharge_location", "is_dead")
inputs <- c("gender", "weight", "age", "admission_type", "admit_location", "diagnosis", 
            "diagnosis_severity", "diagnosis_mortality")
### ROSARIO: traiem de la base de dades les variables DRG_code i diagnosis
all<-c(outputs,inputs)

df1_blacklist <- data.frame("from" = rep(inputs, each=length(outputs)),
                            "to" = rep(outputs, time=length(inputs)))

# print("Learning the structure of the network")
## AQUESTA XARXA EN REALITAT NO L'HAS DE FER SERVIR FINS QUE VALIDIS!!!
# xarxa1 <- hc(df[,all],score="bic", blacklist = df1_blacklist) # poso df[,all]

print("Training and testing the network using k-fold cross-validation")
numfolds = 10
# xarxa1.finals <- bn.cv(df, xarxa1, fit.args = list(method = "mle"), k = numfolds)
xarxa1.finals <- bn.cv(df[,all], "hc", 
                       algorithm.args = list(score="bic",blacklist=df1_blacklist),  # FALTABA!!!
                       fit.args = list(method = "mle"), k = numfolds)
### ROSARIO: L'ESTRUCTURA D'APREN A PARTIR DE CADA CONJUNT D'ENTRENAMENT, PER TANT
###. NO POT SER LA FIXADA A xarxa1. EN CANVI, LI DIEM EL METODE PER APRENDRE: hc
xarxa1.finals[[1]]   # es una llista. La xarxa (objecte bn.fit) es a $fitted
library(Rgraphviz)
plot.xarxa1.1<-graphviz.plot(xarxa1.finals[[1]]$fitted)  # en principi, cada xarxa pot tenir
# un DAG diferent. 
plot.xarxa1.1

plot.xarxa1.5<-graphviz.plot(xarxa1.finals[[6]]$fitted)
plot.xarxa1.5
############################################################################

confusion.matrix.model1.lists <- list()
xarxa1.predictions<-list()

# Prova per obtenir la matriu de confusió per un output concret
validation.set <- df[xarxa1.finals[[1]]$test,all]
xarxa1.finals.grain<-as.grain(xarxa1.finals[[1]]$fitted)  # has de passar la xarxa de classe bn.fit
                                                          # a format gRain, quen es un paquet, per 
                                                          # a poder fer millor les prediccions
xarxa1.predictions <- predict(xarxa1.finals.grain, response="respiratory_services", 
validation.set, predictors=inputs, type='class')

confusionmatrix_output1 <- table(validation.set$respiratory_services, xarxa1.predictions$pred$respiratory_services)

## Ara et prediu "0" i "1", pero abans, tal i com ho tenies, nomes et predeia "0" !!


# Guardem a la llista confusion.matrix.lists 10 llistes, cadascuna
# amb 6 matrius de confusió(1 per variable output)
for(fold in 1:numfolds){
  confusion.matrix.model1.lists[[fold]] <- list() # Creem una llista on guardarem les matrius de confusió
  xarxa1.predictions[[fold]]<-list() # idem per a les prediccions
  validation.set <- df[xarxa1.finals[[fold]]$test,all] # Agafem el set de validació utilitzat en aquest fold concret
                                                       # he tret les dues variables que, de moment, no fem servir posant "all"
  
  for(output in 1:length(outputs)){

    xarxa1.predictions[[fold]][[output]]<-vector()
    outname <- outputs[[output]]

    # Fem la predicció amb els valors del bn.fit i el validation.set
    # S'ha de fer cas a cas, perque pot ser que algun cas no pugui predir
    # aixo si, ara triga mes, es clar... paciencia!!
    for (i in 1:dim(validation.set)[1]) {   
      if (is.null(predict(as.grain(xarxa1.finals[[fold]]$fitted), response=outname, 
                validation.set[i,], predictors=inputs, type='class')$pred[[1]])==TRUE)
        {
        xarxa1.predictions[[fold]][[output]][i]<-NA
      } else {
          xarxa1.predictions[[fold]][[output]][i]<-predict(as.grain(xarxa1.finals[[1]]$fitted), response=outname, 
                                                           validation.set[i,], predictors=inputs, type='class')$pred[[1]]
        }
    }
    
    # Generem la matriu de confusió
    categories_to_predict = unique(validation.set[[outname]])
    print(categories_to_predict)
    
    # Inicialitzem a 0s la matriu perquè no es prediuen totes les labels en totes les variables.
    confusion.matrix <- matrix(0, nrow = length(categories_to_predict), ncol = length(categories_to_predict),
                               dimnames = list(categories_to_predict, categories_to_predict))
    
    taula <- table(validation.set[[outname]], xarxa1.predictions[[fold]][[output]])
    confusion.matrix[rownames(taula), colnames(taula)]<- taula
    #print(confusion.matrix)
    
    # Guardem la matriu de confusió de cada output
    confusion.matrix.model1.lists[[fold]][[output]] <- confusion.matrix
  }
}

# Output variables - "respiratory_services", "current_careunit", "LOS_x", "LOS_y", "discharge_location", "is_dead_hosp"
print(confusion.matrix.model1.lists[[1]])   # fold 1


#######. A BANDA DE QUE PREDIGA MILLOR O PITJOR CADA VARIABLE OUTPUT, TENIM LA QUESTIO 
#######. QUE TAL I COM HAS DEFINIT "is_dead_hosp" ESTA MOLT CORRELACIONADA AMB "respiratory_services"
#######. SI PREDIU "is_deas_hosp" = 1, HAURIA DE PREDIR "discharge_location"=DEAD. 
#######. PERO AIXO NO ACOSTUMA A PASSAR!! 
#######. JO, POTSER, EL QUE FARIA SERIA TORNAR A FER TOT PERO TRAIENT LA VARIABLE "is_dead_hosp" DEL MODEL
#######. ES A DIR, QUE NO SERA NI INPUT NI OUTPUT DEL MODEL1, PERQUE AL MODEL 1, ES PREDIU 
#######. discharge_location, I SI LA PREDICCIO ES "DEAD", VOL DIR QUE LA PREDICCIO PER A is_dead_hosp SERIA "1".
#######. EN CANVI, AL MODEL2 (BINARY RELEVANCE), LA POTS DEIXAR, DE MOMENT, A VEURE SI LA PREDIU BE (EN EL MODEL1
########. LA PREDICCIO ERA DOLENTA)

table(xarxa1.predictions[[1]][[5]],xarxa1.predictions[[1]][[6]])


############################
# M2 - BINARY RELEVANCE
############################

######. ROSARIO: NO CAL, SON EL MATEIXOS OUTPUTS I INPUTS QUE ABANS:
# outputs <- c("respiratory_services", "current_careunit", "LOS_x", "LOS_y", "discharge_location", "is_dead_hosp")
# inputs <- c("gender", "weight", "age", "admission_type", "admit_location", "DRG_code", "diagnosis", "diagnosis_severity", "diagnosis_mortality")

## has de repetir el segunt per a cada output per separat: 

df2_blacklist<-list()
xarxa2.finals<-list()
xarxa2.predictions<-list()
confusion.matrix.model2.lists<-list()
numfolds = 10

for(fold in 1:numfolds){
  confusion.matrix.model2.lists[[fold]] <- list() 
}



for(output in 1:length(outputs)){
  outname <- outputs[[output]]
  df2_blacklist[[output]] <- data.frame("from" = inputs,
                                        "to" = rep(outname, time=length(inputs)))
  
  xarxa2.finals[[output]] <- bn.cv(df[,c(inputs,outname)], "hc", 
                                   algorithm.args = list(score="bic",blacklist=df2_blacklist[[output]]),  
                                   fit.args = list(method = "mle"), k = numfolds)
  
  
  for(fold in 1:numfolds){
    xarxa2.predictions[[fold]]<-list() # idem per a les prediccions
    validation.set <- df[xarxa2.finals[[output]][[fold]]$test,c(inputs,outname)] 
    
    xarxa2.predictions[[fold]][[output]]<-vector()
    
    for (i in 1:dim(validation.set)[1]) {   
      if (is.null(
        predict(as.grain(xarxa2.finals[[output]][[fold]]$fitted), response=outname, 
                validation.set[i,], predictors=inputs, type='class')$pred[[1]])==TRUE){
        xarxa2.predictions[[fold]][[output]][i]<-NA} else {
          xarxa2.predictions[[fold]][[output]][i]<-predict(as.grain(xarxa2.finals[[output]][[fold]]$fitted), response=outname, 
                                                           validation.set[i,], predictors=inputs, type='class')$pred[[1]]}
    }
    
    # Generem la matriu de confusió
    categories_to_predict = unique(validation.set[[outname]])
    
    # Inicialitzem a 0s la matriu perquè no es prediuen totes les labels en totes les variables.
    confusion.matrix <- matrix(0, nrow = length(categories_to_predict), ncol = length(categories_to_predict),
                               dimnames = list(categories_to_predict, categories_to_predict))
    
    taula <- table(validation.set[[outname]], xarxa2.predictions[[fold]][[output]])
    confusion.matrix[rownames(taula), colnames(taula)]<- taula
    #print(confusion.matrix)
    
    # Guardem la matriu de confusió de cada output
    confusion.matrix.model2.lists[[fold]][[output]] <- confusion.matrix
  }
} 

# Output variables - "respiratory_services", "current_careunit", "LOS_x", "LOS_y", "discharge_location", "is_dead_hosp"
print(confusion.matrix.model2.lists[[1]])   # fold 1


#############.  ROSARIO: QUAN TINGUIS LES 10 MATRIUS DE CONFUSIO PER A CADASCUNA DE LES 6 VARIABLES OUTPUT
#############.  PER A CADASCUN DELS DOS MODELS:
#############.  confusion.matrix.model1.lists[[fold]][[output]]
#############.  confusion.matrix.model2.lists[[fold]][[output]] 
#############   ES TRACTARA DE CALCULAR PER A CADA OUTPUT UNA METRICA DE COMPORTAMENT ADIENT, 

#### "respiratory_services": qualsevol metrica binaria (accuracy, F-score, etc.), 
#### "current_careunit": accuracy, perque es multi-class, 
####  "LOS_x", "LOS_y": MAE, perque es ordinal 
####  "discharge_location": accuracy, perque es multi-class, 
####  "is_dead_hosp": qualsevol metrica binaria (accuracy, F-score, etc.)

#############  I DESPRES COMPARAR LES DUES MOSTRES DE MIDA 10 (INDEPENDENTS) PER A CADA METRICA DE CADA
#############. VARIABLE OUTPUT

