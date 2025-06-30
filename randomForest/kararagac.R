library(dplyr)
install.packages("randomForest")
library(randomForest)
data<-read.table(file.choose(),header=T, sep=";")

##turkce isimlendirme
names(data)<-c("yalniz_gecirilen_zaman","sahne_korkusu","sosyal_etkinlik_katilim","disari_cikma","sosyallesmeden_sonra_tukenmislik","arkadas_daire_boyutu","gonderi_frekansi","kisilik")
data<-data %>%
  mutate(sahne_korkusu=recode(sahne_korkusu,"Yes"="evet","No"="hayır"))


data<-data %>%
  mutate(kisilik=recode(kisilik,"Extrovert"="disa_donuk","Introvert"="ice_donuk"))

data<-data %>%
  mutate(sosyallesmeden_sonra_tukenmislik=recode(sosyallesmeden_sonra_tukenmislik,"Yes"="evet","No"="hayır"))


##veri kontrol
str(data)
data$kisilik<-as.factor(data$kisilik)
data$sahne_korkusu<-as.factor(data$sahne_korkusu)
data$sosyallesmeden_sonra_tukenmislik<-as.factor(data$sosyallesmeden_sonra_tukenmislik)

table(data$yalniz_gecirilen_zaman)
data$yalniz_gecirilen_zaman<-gsub(",",".",data$yalniz_gecirilen_zaman)
data$yalniz_gecirilen_zaman<-as.numeric(data$yalniz_gecirilen_zaman)
str(data)
table(data$sosyal_etkinlik_katilimi)
data$sosyal_etkinlik_katilim<-gsub(",",".",data$sosyal_etkinlik_katilim)
data$sosyal_etkinlik_katilim<-as.numeric(data$sosyal_etkinlik_katilim)
str(data)

table(data$disari_cikma)
data$disari_cikma<-as.numeric(data$disari_cikma)

table(data$arkadas_daire_boyutu)
data$arkadas_daire_boyutu<-gsub(",",".",data$arkadas_daire_boyutu)
data$arkadas_daire_boyutu<-as.numeric(data$arkadas_daire_boyutu)

table(data$gonderi_frekansi)
data$gonderi_frekansi<-gsub(",",".",data$gonderi_frekansi)
data$gonderi_frekansi<-as.numeric(data$gonderi_frekansi)

str(data)

sum(is.na(data))

##veri train/test olarak bolmek
set.seed(123)
indeks<-sample(1:nrow(data),0.7*nrow(data))
train<-data[indeks,]
test<-data[-indeks,]

randomforest_model<-randomForest(kisilik~.,data=train,ntree=100,importance=TRUE)

##model degerlendirme
ongoru<-predict(randomforest_model,test)
confusionmatris<-table(predicted=ongoru, Actual=test$kisilik)
print(confusionmatris)

mean(ongoru==test$kisilik)

##etkili degerler
importance(randomforest_model)
varImpPlot(randomforest_model)
