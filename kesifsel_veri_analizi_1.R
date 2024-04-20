install.packages("tidyverse") #veri temizleme ve veri manupülasyonu için.
install.packages("readxl") #excel formatındaki veri setini yüklemek için. 
install.packages("readr")


library(tidyverse)
library(readxl)
library(readr)



#veri Kaynağı
#titanik_data veri setini Kaggle'dan aldım ve bilgisayarıma indirdim. Kaynak:https://www.kaggle.com/competitions/titanic/data?select=train.csv


#Bu çalışmada keşifsel veri analiz teknikleri kullanılarak titanik veri setinin(titanik.xlsx);
#varsa eksik verilerinin temizlenmesini müteakip,
#temel istatistiklerinin hesaplanması yöntemleriyle yolcuların durumlarına göre 
#ortalama yaşlarının hesaplanması ve görselleştirilmesi amaçlanmıştır. 



getwd()
setwd("C:/Users/ALİM/Desktop/titanic")

titanik_data <- read_excel("C:/Users/ALİM/Desktop/titanic/titanik.xlsx") # excel dosyasını yükleniliyor.
                                                                      #Bu veri setine titanik_data ismi veriliyor. 

view(titanik_data) #veri setini görüyoruz. 

names(titanik_data) # sütunları görüyoruz.

titanik <- data.frame(titanik_data %>%
  select(PassengerId , Survived , Pclass , Name , Sex, Age, Cabin , Embarked))#belirli sütunları aldık. 

view(titanik)

is.na(titanik) #boş kayıt var mı ona bakılır.

titanik %>%
  filter(is.na(Embarked)) #belirli bir sütunda boş kayıt var mı ona baktık. 

null_Embarked <- data.frame(titanik %>%
  filter(is.na(Embarked))) #Embarked sütunundaki null kayıtları içeren tablo oluşturduk. 

view(null_Embarked) # 2 kayıt olduğunu görüyoruz. Bu 2 kaydı titanik veri setinden çıkaracağız. 

veri_yeni <- anti_join(titanik, null_Embarked) # anti_join fonksiyonu ile null_Embarked tablosundaki satırları titanik veri setinden çıkardık ve veri_yeniye atadık.

view(veri_yeni)

veri_yeni %>% 
  filter(is.na(Embarked)) #Embarked sütununda boş kayıt kaldı mı kontrolünü sağladık.

titanik <- veri_yeni # veri_yeniyi titanik'e atadık. Yani artık titanik veri setinde 
                      # Embarked sütunundaki boş olan kayıtlar yer almıyor. 

view(titanik) 

#titanik yolcularının yaşlarının(age) temel istatistiklerini inceleyelim. Ancak age sütunu numerik mi ona bakalım.

is.numeric(titanik$Age) #age numerik mi ona baktık. numerik değil. 

#age sütununun temel istatistiklerini hesaplayabilmemiz için numerik veri tipinde olması lazım.
#age sütununu numerik veri tipine dönüştüreceğiz. 

titanik$Age <- as.numeric(titanik$Age) #age sütununu numerik yaptık ve tütanik veri setinin age sütununa atadık.

is.numeric(titanik$Age) # true döndü

#Şimdi boş değer var mı ona bakalım.

sum(is.na(titanik$Age))#177 adet boş değer mevcut. Bunları uygun bir değerle değiştireceğiz.

mean(titanik$Age , na.rm = TRUE) # ortalama yaş 29.64209 geldi. Boş kayıtlara yazmak için uygun olabilir.

median(titanik$Age  ,na.rm = TRUE) # medyanı 28. Boş kayıtlara yazılabilir. 

titanik$Age[is.na(titanik$Age)] <-  median(titanik$Age  ,na.rm = TRUE) #boş değerleri median ile değiştirdik.

sum(is.na(titanik$Age)) #boş değer kaldı mı kontrol ettik. 

view(titanik)

#Şimdi hayatta kalma durumuna(Survived) göre yolcuları ayıralım ve yaş(Age) ortalamalarını inceleyelim.

hayatta_kalanlar <-titanik %>%
                    filter(titanik$Survived  == 1) #hayatta kalanları filtreledik

view(hayatta_kalanlar)

mean(hayatta_kalanlar$Age) #hayatta kalanların ortalama yaşları  28.16374

olenler <-titanik %>%
  filter(titanik$Survived  == 0) #olenleri filtreledik 

view(olenler) 

mean(olenler$Age) #olenlerin yaş ortalamaları  30.02823
          
yas_ort <- c(mean(olenler$Age) , mean(hayatta_kalanlar$Age)) #ölü ve hayatta kalanların 
                                                            #yaş ortalamalarını yas_ort'a atadık.

print(yas_ort)

durum <- c("Olu" , "Hayatta") #yolcuların durumlarını durum'a atadık

print(durum)

Yas_ort_durum <- data.frame( yas_ort , durum ) #yaş ortalamaları ve durumu birleştirerek yeni bir nesne oluşturduk.

view(Yas_ort_durum)

ggplot(Yas_ort_durum , aes( x= durum , y=yas_ort)) +
  geom_bar(stat='identity' , fill= 'blue' , width = 0.5) +
  scale_y_continuous(limits = c(0, 40))+
  labs(x = 'Yolcuların Durumu' ,
       y = 'Yaş Ortalamaları',
       title = 'Yolcuların Durumuna Göre Ortalama Yaşlar') +
  theme_bw()   # çubuk grafiği kullanılarak görselleştirildi. 

#Çalışma sonucunda Ölü yolcuların ortalama yaşlarının, 
#Hayatta olan yolcuların ortalama yaşlarından daha yüksek olduğu görülmüştür. 


  

