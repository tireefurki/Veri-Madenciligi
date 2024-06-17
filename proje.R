# KUTUPHANELER

library(tuber)  # YOUTUBE API ILE BAGLANTI KURMAK ICIN KULLANILDI
library(httpuv)  # YOUTUBE DAN VERI CEKMEK ICIN KULLANILDI
library(tidytext)  # METIN MADENCILIGI ICIN KULLANILAN KUTUPHANEDIR
library(dplyr)  # VERI MANIPILASYONU ICIN KULLANILAN KUTUPHANEDIR
library(tm)  # METIN MADENCILIGI ICIN GEREKLI OLAN KUTUPHANEDIR
library(stringr)  # KARAKTER YAPILI METINLER ICIN KULLANILIR
library(tibble)  #VERI CERCEVESI OLUSTURMAK ICIN KULLANILMISTIR
library(magrittr)   #  %>% KOMUTU ILE ZINCIRLEME YAPMAK ICIN KULLANILAN KUTUPHANEDIR
library(ggthemes)  # GRAFIK OLUSTURMAK ICIN KULLANILAN KUTUPHANEDIR
library(ggplot2)  # VERILERI GORSELLESTIRMEK ICIN KULLANILAN KUTUPHANEDIR
library(wordcloud2)  # KELIME BULUTU ICIN KULLANILAN KUTUPHANEDIR
library(RColorBrewer) # KELIME BULUTUNUN RENKLENDIRILMESI ICIN KULLANILAN PAKETTIR
library(sentimentr) # DUYGU ANALIZI ICIN KULLANILAN KUTUPHANEDIR
library(pander)  # VERI ANALIZI SONUCLARINI GOSTERMEK ICIN KULLANILAN KUTUPHANEDIR
library(pastecs) # TABLOLASTIRMA ICIN KULLANILAN KUTUPHANEDIR



# YOUTUBE DAN BULDUGUM VIDEONUN YORUMLARINI CEKTIM

app_id <- "1057915472699-i1c4u51400l2hgu34sggad1qa59gm7rv.apps.googleusercontent.com"
app_secret <- "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"
yt_oauth(app_id, app_secret, token ="")
get_all_comments(video_id = "lA77zsJ31nA")    
comments1 <- get_all_comments(video_id ="lA77zsJ31nA")
write.csv(comments1, file = "youtubecomments.csv")
verim <- read.csv("youtubecomments.csv")   # csv DOSYASI CAGIRMA

# TIDYTEXT VE DPLYR KUTUPHANELERINI KULLANARAK PARCALAMA ISLEMI YAPTIM

install.packages("tidytext")



verim <- verim%>%
  dplyr::select(textOriginal)
verim <- verim %>%
  unnest_tokens(word, textOriginal)

verim<-verim %>% as_tibble()


# DURAKLAMA KELIMELERINI CIKARMAK 

data("stop_words")
View(stop_words)

verim <- verim %>%
  anti_join(stop_words, by = c("word"))


# LATIN ALFABESINDEKI HARFLER HARICINDEKI KELIMELERIN CIKARILMASI

verim$word <- as.character(verim$word)
verim <- verim[str_detect(verim$word, "^[A-Za-z0-9 ]"), ]
verim <- as.data.frame(verim)




# METIN TEMIZLEMEK ICIN BAZI FONKSIYONLAR

verim$word <- tolower(verim$word)   # BUYUK HARF OLAN YORUMLARI KUCUK HARFE CEVIRDIM
verim<-verim %>% mutate(word=removePunctuation(word)) #METINDEN NOKTLAMA ISARETLERINI KALDIRDIM
verim<- verim %>% mutate(word=str_squish(word))     #SAYILARLA YAZILARIN BIRLESTIGI KELIMELERIKALDIRDIM
verim<-verim %>% mutate(word=removeNumbers(word))   #RAKAM VE SAYILARI KALDIRDIM
verim<-verim %>% filter(str_length(word)>2)    # 2 HARFTEN AZ OLAN KELIMELERI KALDIRDIM
verim<-verim %>% filter(str_length(word)>1)
verim<-verim %>% filter(str_length(word)>3)




#KELIMELERI COUNT EDEREK FREKANS DEGERLERINI YAZDIRDIM
count_ettim <- verim%>%
  count(word, sort = TRUE)




# CIKARMAK ISTEDIGIM KELIMELERI SET OLUSTURARAK SILDIM
silinecek_kelimeler <- c("naj??wi??tszej", "marii", "chwa??a", "pannie", "cze????", "wieczna", "precz", "panny", "judaszem", "rozkaz", "wysokie", "it???s", "nami", "dont", "more??ski", "he???s", "can???t") 
count_ettim <- count_ettim[!count_ettim$word %in% silinecek_kelimeler, ]
verim$word <- gsub("overly", "", verim$word)   # AYKIRI DEGERI KALDIRDIM



# FREKANS GRAFIGINI YAZDIRDIM

count_ettim%>%
  head(30)%>%
  ggplot(aes(reorder(word,n),n))+
  geom_col()+
  coord_flip() + theme_minimal()+
  labs(x="KELIMELER",
       y="FREKANS DEGERI",
       title="TEKRAR EDEN KELIME GRAFIGI")
  
  
  

#KELIME BULUTU ICIN PAKETLER
install.packages("wordcloud2")
install.packages("RColorBrewer")
library(wordcloud2) # kelime bulutu olusturmak icin
library (RColorBrewer) # renk paleti secmek icin

#KELIME BULUTUNU CIZME

wordcloud2(count_ettim)


# POZITIF VE NEGATIF DEGERLERE GORE TABLO OLUSTURMA

poz_neg<-count_ettim%>% inner_join(get_sentiments("bing"),by="word")
poz_neg_ozet <- poz_neg %>%
  group_by(sentiment) %>%
  summarise(word_count = n(), .groups = "drop")




# POZITIF NEGATIF KELIMELERI GRAFIK OLARAK GORSELLESTIRDIM


install.packages("ggthemes")
library(ggthemes)

poz_neg %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  head(50) %>%
  ggplot(aes(x=reorder(word,n), y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Kelime",x = "yogunluk") +
  coord_flip() +
  theme_hc()+
  labs(caption = "Veri Kaynagi: Youtube'da CNET kanalinda Neurolink videosundan analiz edilmistir.")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))




# POLARITE (DUYGU TONU) ANALIZI


polarite<-sentiment(verim$word)

stat.desc(polarite$sentiment, basic=T) %>% pander()    # asag??da  ortalama degerler?? vs. veriyor



# POLARITE GRAFIGI OLUSTURMA 


polarite<-sentiment(verim$word)
tablo<-cbind(verim$word, polarite[,c(3,4)])

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Kelimelerin Frekansi") +
  theme_gray()+
  labs(caption = "Veri Kaynagi: Youtube'da CNET kanalinda Neurolink videosundan analiz edilmistir.")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))


# POZITIF NEGATIF VE NOTR DEGERLERE GORE GRAFIK OLUSTURMA

bar_grafik <- polarite$sentiment  
bar_grafik <- as.data.frame(bar_grafik)
bar_grafik$bar_grafik[bar_grafik$bar_grafik > 0] <- 'Pozitif'   
bar_grafik$bar_grafik[bar_grafik$bar_grafik == 0] <- 'Notr'      # DEGERLERE GORE POZITIF, NEGATIF VE NOTR BASLIGI ATADIM
bar_grafik$bar_grafik[bar_grafik$bar_grafik < 0] <- 'Negatif'
bar_grafik$bar_grafik <- as.factor(bar_grafik$bar_grafik)
degerler <- table(bar_grafik$bar_grafik)
barplot(degerler, main='Duygu Dagilimi', xlab='Kategori', ylab='Sayi', col=c("red","blue", "green" ))

