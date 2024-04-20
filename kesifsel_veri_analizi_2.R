#işimize yarayacak paketlerin yüklenilmesi
install.packages("MASS")
install.packages("dplyr") # filter gibi fonksiyonları kullanmak için. 
install.packages("tidyverse")

#paketlerin çağırılması
library(MASS)
library(tidyverse)
library(dplyr)

set.seed(123) #tekrarlanabilir veriler üretmek için seed belirleme 

n <- 100 #n değeri tanımladık, veri seti oluştururken kullanacağız 

maas <- c(3000 , 5000 , 7000) #maas adlı değişken oluşturduk

#veri setini oluşturma
data <- data.frame( #data.frame şeklinde oluşturuyoruz
  personel_id = 1001:1100,#1001'den 1100'e kadar sıralı değer atama
  personel_maas = sample(maas , n, replace = TRUE,prob = c(0.9, 0.07, 0.03)),#oluşturduğumuz maaş değişkenini kullanarak
                                                                            #personel_maas adlı sütun oluşturduk. sample()
                                                                            #fonksiyonu belirtilen değerlerden
                                                                            #rastgele değişken üretmemiz için. replace=TRUE 
                                                                            #değerlerin tekrar kullanılabilmesini sağlıyor. 
                                                                            #prob olasılık veriyor. Yüksek maaşların olasılığını
                                                                            #düşük tuttuk. Bununla ilgili ileride 
                                                                            #ayrı bir değişken oluşturacağız.
  Performans_notu = rnorm(n , mean = 3 , sd = 1) #bağımsız değişken 
)

#veri setini görme
head(data)

#boş verilerin kontrolü 
is.na(data)

#data veri setini data1 veri setine atama.
data1<- data

#yeni maaş hesaplamak için bir fonksiyon oluşturuyoruz.
yeni_maas_hesaplama <- function(maas, Performans_notu) {
                    yeni_maas <- maas*Performans_notu
                    return(yeni_maas)
}

#oluşturduğumuz fonksiyonu veri setimizde kullanıyouz. yeni_maas isimli değişken oluşturduk. 
data1$yeni_maas <- yeni_maas_hesaplama(data1$personel_maas, data1$Performans_notu)

#yeni maaş eski maaşından daha düşük ise eski maaşı yaz değilse yeni maaşı yaz. Koşul oluşturduk. 
data$yeni_maas <- ifelse(data1$yeni_maas < data1$personel_maas , data1$personel_maas, data1$yeni_maas)

#veri setini görme
view(data)

#personel_maas değişkenini baz alarak yeni bir dweğişken oluşturduk data veri setine ekledik. Personeller isimli veri seti oluşturduk. 
personeller <-mutate(data , unvan = case_when(personel_maas==7000 ~ "genel_mudur",
                                             personel_maas==5000 ~ "mudur",
                                             personel_maas==3000 ~ "calisan",
                                        TRUE ~ NA_character_
))


view(personeller)

#personeller veri setini unvanlara göre filtreledik. 
calisan <- filter(personeller , unvan=="calisan")

view(calisan)

mudur <- filter(personeller , unvan== "mudur")

genel_mudur <- filter(personeller , unvan== "genel_mudur")

#max calısan maaşını hesapladık 
max_calisan_maas <- calisan %>% 
  summarise(max_maas = max(yeni_maas)) %>%
  pull(max_maas)

#max müdür maaşını hesapladık
max_mudur_maas <- mudur %>%
                    summarise(max_maas = max(yeni_maas)) %>%
                    pull(max_maas)

view(max_mudur_maas)

#senaryomuza göre yeni maaşı bir altındaki unvanın max yeni maaşından düşük olan personeller ayrılacak. 

#ayrılacak genel müdürler
ayrilacak_gm <- genel_mudur %>%
  filter(yeni_maas < max_mudur_maas)

#ayrılacak müdürler
ayrilacak_mdr <- mudur %>% 
  filter(yeni_maas< max_calisan_maas)

#calisan ünvanına sahip personellerden ise yeni maaşı aynı ünvana sahip personellerin yeni maaşının 
#ortalamasından düşük yeni maaş alan calisan'lar ayrılacak. 
ayrilacak_calisan <- calisan %>%
                      filter(yeni_maas< mean(yeni_maas))

#calisan unvanlı personellerin yeni maaşının ortalaması 
mean(calisan$yeni_maas)  #9161.172

ayrilacaklar <- bind_rows(ayrilacak_gm, ayrilacak_mdr ,ayrilacak_calisan ) # bu şekilde ayrılacak personellerin tamamını
                                                                            #tek bir nesnede topladık. 

view(ayrilacaklar)

#personeller veri seti ile ayrilacaklar veri setinin kesişen personel_id'lerini ayrilacak_personel; diğer id'leri 
#kalan_personel olarak isimlendirdik ve personeller veri setine yeni bir değişken olarak atadık. 
personeller <-  mutate(personeller , durum = ifelse(personeller$personel_id %in% ayrilacaklar$personel_id,
                                   'ayrilacak_personel' ,
                                   'kalan_personel'))

view(personeller)

personeller %>%
  group_by(durum) %>%
  count()  # ayrılacaklar ve kalanların sayısına baktık. Her iki grupta da 50 kişi bulunmakta. 

#krosstab analizi.durum değşkeniyle unvan değişkeninin ilişkisini gösteren çapraz tablo oluşturduk.
krosstab <- table(personeller$durum, personeller$unvan)

print(krosstab) #krosstab tablosunu görüntüledik.

krosstab_df <- as.data.frame(krosstab) #data.frame olarak güncelledik. 
colnames(krosstab_df) <- c("unvan", "durum", "Sayi") #sayı değişkeni ekledik. 

view(krosstab_df)

#grafik oluşturma 
grafik <- ggplot(krosstab_df, aes(x = unvan, y = Sayi, fill = durum)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Unvan", y = "Sayı", fill = "Durum", title = "Unvana Gore Gruplandırılmıs Durum Sayıları") +
  theme_minimal()

plot(grafik) #grafiği görüntülemek için.

#grafikte unvanların sıralaması estetik olarak hoş durmuyordu. Örneğin ayrilacak_personel grubunda genel_mudur olmadığı için 
#ve genel_mudur "bar"ı calisan ve mudur arasında kaldığı için bunu en sağa almaya çalıştık. 

#"bar"da bu şekilde görüntülenmesi için "calisan", "mudur", "genel_mudur" şeklinde sıralama yaptık. 
krosstab_df$durum <- factor(krosstab_df$durum, levels = c("calisan", "mudur", "genel_mudur"))

# Grafiği yeniden oluşturdurk. scale_fill_manual() fonksiyonu ile sıralamayı manuel yaptık ve renkleri değiştirdik. 
grafik <- ggplot(krosstab_df, aes(x = unvan, y = Sayi, fill = durum)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Unvan", y = "Sayı", fill = "Durum", title = "Unvana Gore Gruplandırılmıs Durum Sayıları") +
  scale_fill_manual(values = c("calisan" = "blue", "mudur" = "green", "genel_mudur" = "red")) +
  theme_minimal()