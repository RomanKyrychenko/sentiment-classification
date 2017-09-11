Sys.setlocale(,"UK_ua")

library(sentimentr)
library(tidyverse)
library(tidytext)
senti_words <- data_frame(term=unique(c(sent_h$x,sent_j$x,sent_n$x,sent_s$x,lexicon::nrc_emotions$term,SentimentAnalysis::DictionaryGI$negative,
                                        SentimentAnalysis::DictionaryGI$positive,SentimentAnalysis::DictionaryHE$negative,SentimentAnalysis::DictionaryHE$positive,
                                        SentimentAnalysis::DictionaryLM$negative,SentimentAnalysis::DictionaryLM$positive,SentimentAnalysis::DictionaryLM$uncertainty)))

senti_words <- readxl::read_excel("senti_words.xlsx")

lexicon::hash_sentiment_huliu
lexicon::hash_emoticons
lexicon::hash_sentiment_jockers 
lexicon::hash_sentiment_nrc

sentiword <- as_data_frame(lexicon::hash_sentiment_sentiword)
sentiword <- left_join(sentiword,senti_words,by=c("x"="term"))
sentiword_ua_ru <- distinct(bind_rows(data_frame(words=sentiword$ua,polarity=sentiword$y),
                             data_frame(words=sentiword$ru,polarity=sentiword$y)))

txt <- 'Вчерашний день прошел под знаком завершения трансферного окна в большинстве топ-чемпионатов Европы.Так больше не 
могут совершать покупки команды из Англии, Германии, Франции или Италии. Однако в Испании "час икс" наступит лишь сегодня ночью.
Таким образом, испанские команды могут подписывать игроков из других чемпионатов, а это означает, что трансферные саги о переходе 
Филиппе Коутиньо в Барселоны или же Диего Косты в Атлетико податливый все еще не завершены.Напомним, что ранее появилась информация, что 
Ливерпуль согласился продать Коутиньо в Барселону за 160 миллионов евро. Кроме того, Барса может подписать и Рияда Мареза.'

#sentiment_by(txt,polarity_dt=sentiword_ua_ru,valence_shifters_dt=sentiword_ua_ru)

masiv <- read_csv("masiv.csv")

tonal <- masiv$title_ru %>% 
  as_data_frame() %>% 
  mutate(id_text=c(1:12059),nwords=stringr::str_count(value,"\\S+"))  %>%  
  unnest_tokens(sentence,value,token = "sentences") %>% 
  group_by(id_text) %>% 
  mutate(id=c(1:n())) %>% 
  unnest_tokens(words,sentence,token = "ngrams", n = 3) %>% 
  separate(words,c("word1","word2","word3"),sep = " ") %>% 
  left_join(sentiword_ua_ru,by=c("word1"="words")) %>% 
  left_join(sentiword_ua_ru,by=c("word2"="words")) %>% 
  left_join(sentiword_ua_ru,by=c("word3"="words")) %>% 
  filter(!is.na(polarity.x)) %>% 
  mutate(polarity=polarity.x*ifelse(!is.na(polarity.y),polarity.y,1)*ifelse(!is.na(polarity.y)&!is.na(polarity),
                                                                            polarity,1)) %>% 
  select(id_text,id,nwords,word1, word2, word3, polarity) %>% 
  group_by(id_text) %>% 
  summarize(nwords = mean(nwords,na.rm=T), polarity=sum(polarity, na.rm=T)/(nwords+1)) #%>% 
  #mutate(polarity=ifelse(polarity>0,scales::rescale(polarity,to=c(0,1)),
  #                       ifelse(polarity<0,scales::rescale(polarity,to=c(0,-1)),0
  #                       )))

tidysent <- function(x){
  x %>% 
    as_data_frame() %>% 
    mutate(id_text=c(1:12059),nwords=stringr::str_count(value,"\\S+"))  %>%  
    unnest_tokens(sentence,value,token = "sentences") %>% 
    group_by(id_text) %>% 
    mutate(id=c(1:n())) %>% 
    unnest_tokens(words,sentence,token = "ngrams", n = 3) %>% 
    separate(words,c("word1","word2","word3"),sep = " ") %>% 
    left_join(sentiword_ua_ru,by=c("word1"="words")) %>% 
    left_join(sentiword_ua_ru,by=c("word2"="words")) %>% 
    left_join(sentiword_ua_ru,by=c("word3"="words")) %>% 
    filter(!is.na(polarity.x)) %>% 
    mutate(polarity=polarity.x*ifelse(!is.na(polarity.y),polarity.y,1)*ifelse(!is.na(polarity.y)&!is.na(polarity),
                                                                              polarity,1)) %>% 
    select(id_text,id,nwords,word1, word2, word3, polarity) %>% 
    group_by(id_text) %>% 
    summarize(nwords = mean(nwords,na.rm=T), polarity=sum(polarity, na.rm=T)/(nwords+1))
}

tidysent_by <- function(x, person){
  x %>% 
    as_data_frame() %>% 
    mutate(id_text=c(1:12059),nwords=stringr::str_count(value,"\\S+"))  %>%  
    unnest_tokens(sentence,value,token = "sentences") %>% 
    group_by(id_text) %>% 
    mutate(id=c(1:n())) %>% 
    unnest_tokens(words,sentence,token = "ngrams", n = 3) %>% 
    filter(grepl(person,words)) %>% 
    separate(words,c("word1","word2","word3"),sep = " ") %>% 
    left_join(sentiword_ua_ru,by=c("word1"="words")) %>% 
    left_join(sentiword_ua_ru,by=c("word2"="words")) %>% 
    left_join(sentiword_ua_ru,by=c("word3"="words")) %>% 
    filter(!is.na(polarity.x)) %>% 
    mutate(polarity=polarity.x*ifelse(!is.na(polarity.y),polarity.y,1)*ifelse(!is.na(polarity.y)&!is.na(polarity),
                                                                              polarity,1)) %>% 
    select(id_text,id,nwords,word1, word2, word3, polarity) %>% 
    group_by(id_text) %>% 
    summarize(nwords = mean(nwords,na.rm=T), polarity=sum(polarity, na.rm=T))
}

por_t <- tidysent_by(masiv$title_ru,"порошенко") %>% arrange(polarity)


