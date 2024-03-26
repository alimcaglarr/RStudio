#temel fonksiyonlar.
getwd()

kare <- function(x) {
  return(x^2)
} # sayının karesini alma fonksiyonu

print(kare(10)) #100

#----------

geri_sayım <- function(adan) {
  print(adan)
  while (adan!=0) { #sonlanacağı değer. geri sayım fonksiyonuna yaazdığımız girdiden itibaren 0 kadar girdi olarak alır.
    Sys.sleep (1)
    adan <- adan-1
    print(adan)
  }
}
geri_sayım (10) # girdiden 0'a kadar geri sayan bir sayaç fonksiyonu yapıldı.

#----------

veri = read.csv("http://johnmuschelli.com/intro_to_r/data/Youth_Tobacco_Survey_YTS_Data.csv")

View(veri)

head(veri)

dim(veri)

nrow(veri)

#-----------

install.packages("dplyr")
library(dplyr)

veri_yeniden_adlandır = rename(veri , YeaR = YEAR)

names(veri)
names(veri_yeniden_adlandır)

veri_yeniden_adlandır2 = rename(veri_yeniden_adlandır ,  year = YeaR)

names(veri_yeniden_adlandır2)

ls()

#-----------

install.packages("tidyverse")
library(tidyverse)

data("mtcars")

veri2 = data.frame(mtcars)

head(veri2)
head(mtcars)
view(veri2)
dim(veri2)

veri2_yeniden_ad = dplyr::rename_all(veri2 , toupper)

names(veri2_yeniden_ad)

veri2_gear = veri2$gear

dim(veri2_gear)

veri2_mpg=select(veri2,mpg)

filtre_veri2_20ile17_arası_değil = filter(veri2 , mpg> 20 | mpg < 17)

veri3 =data.frame(veri2) # yeni kolon eklemek için veri2'den veri3'ü türettik
view(veri3)

veri3$new_column = veri3$wt*2  #yeni kolon ekleme

dim(veri3)

veri2_mut = mutate(veri2 , new_col = wt*3) # bu şekilde de yeni sütun eklenebilir.
dim(veri2_mut)


mtcars_sınıf = mutate(mtcars , mpg_sınıflandırma = 
                        ifelse(mpg < 20 ,
                               "düşük",
                               ifelse(mpg <=30,
                                      "orta",
                                      "yüksek"))
          )  # ifelse kullanılarak yeni sütun ekledik. 

arrange(mtcars , desc(mpg))  #order by(sıralama func.) mpg'e göre azalan sıraladık.

transmute()  # mutate'e benzer. 

#-------------örnek çalışmalar.


install.packages("carData")
library(carData)
library(dplyr)
library(tidyverse)

data("Salaries")

maas_unvan <- Salaries %>%
  group_by(unvan = rank) %>% #bu şekilde isim de verilebilir. 
  summarise(ort_maas = mean(salary)) # unvanların ortalama maaşları. 


#-----
cinsiyete_göre = mutate(Salaries , cinsiyet = 
                          case_when(sex== 'Male' & rank=='AsstProf'~  'Male AsstProf',
                                    sex== 'Female' & rank=='AsstProf' ~ 'Female AsstProf',
                                    sex== 'Male' & rank=='Prof'~  'Male Prof',
                                    sex== 'Female' & rank=='Prof' ~ 'Female Prof',
                                    sex== 'Male' & rank=='AssocProf'~  'Male AssocProf',
                                    sex== 'Female' & rank=='AssocProf'~  'Female AssocProf',
                                    
                          ))  # cinsiyet ve ünvana göre yeni sütun oluşturduk.

cinsiyete_göre_maas_unvan <- cinsiyete_göre %>%
  group_by(cinsiyet) %>%
  summarise(ort_maas = mean(salary))


View(cinsiyete_göre)


View(cinsiyete_göre_maas_unvan)


#--------------------------------


install.packages("rpart")
library(rpart)


view(cu.summary)

ort_arac_fiyatı <- cu.summary %>%
  group_by( Type) %>% 
  summarise(mean(Price))

view(ort_arac_fiyatı)


view(cu.summary)

veri = mutate(cu.summary, güvenlik_derecesi = 
                case_when(cu.summary$Reliability == 'Much worse' ~ 1,
                          cu.summary$Reliability == 'worse' ~ 2, 
                          cu.summary$Reliability == 'average' ~ 3,
                          cu.summary$Reliability == 'better'~ 4, 
                          cu.summary$Reliability == 'Much better' ~ 5))

veri3 = merge(veri, ort_arac_fiyatı ,'Type')  # ortalama fiyatı ekledik tabloya

view(veri3)



view(veri)

veri <- data.frame(cu.summary$Price , cu.summary$Reliability, cu.summary$Type)


gruplu_veri <-veri3 %>% 
  group_by(Type) %>% 
  summarise(Ortalama_fiyat = mean(Price) , Güvenilirlik_Derecesi = mean(güvenlik_derecesi ,na.rm = TRUE))

view(gruplu_veri)








