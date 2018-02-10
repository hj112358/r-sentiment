# r presentation code

req_packages <- c("tidyverse", "tidytext", "wordcloud", "sentimentr", "rtweet")
packing_list <- req_packages[!req_packages %in% installed.packages()[,"Package"]]
if(length(packing_list) > 0) install.packages(req_packages)
sapply(req_packages, require, character.only = TRUE)


# slide 20
rats <- data_frame(sentence = c("A rat learning to press a lever in order to earn sugar pellets is an example of operant conditioning.",
                                             "In a related paradigm known as 'self-administration', rats readily learn to perform operant tasks when rewarded with intravenous stimulants (e.g., cocaine or various amphetamines).",
                                             "However, for some rats, no amount of conditioning will result in the self-administration of hallucinogenic substances such as LSD or MDMA, as many rats experience these drugs as aversive (as opposed to rewarding) stimuli.",
                                             "Depending upon the reward (or punishment) selected for an operant task, such a paradigm can address motivated learning in a rat model."))
                      
# slide 23
rat_tokens_raw <- rats %>%
  unnest_tokens(word, sentence)

rat_tokens_raw


# slide 25
stop_words


# slide 27
rat_tokens <- rat_tokens_raw %>%
  anti_join(stop_words, by = "word")

rat_tokens


# slide 28
rat_tokens_count <- rat_tokens %>%
  count(word, sort = TRUE)
rat_tokens_count


# slide 30
rat_tokens_mod <- rat_tokens_raw
rat_tokens_mod$word <- str_replace_all(rat_tokens_mod$word,
                                       "tasks",
                                       "task")
rat_tokens_mod$word <- str_replace_all(rat_tokens_mod$word,
                                       "rats",
                                       "rat")
rat_tokens_mod$word <- str_replace_all(rat_tokens_mod$word,
                                       "reward[:alpha:]+",
                                       "reward")


# slide 31
any(str_detect(rat_tokens_raw$word, "rats"))
any(str_detect(rat_tokens_raw$word, "tasks"))
any(str_detect(rat_tokens_raw$word, "reward[:alpha:]"))


# slide 32
rat_tokens_final <- rat_tokens_mod %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)


# slide 33
library(wordcloud)

wordcloud(words = rat_tokens_final$word,
          freq = rat_tokens_final$n)


# slide 35
sentiments[sentiments$lexicon == "bing",]


# slide 36
sentiments[sentiments$lexicon == "bing" & sentiments$sentiment == "positive",]


# slide 38
ex_text <- c("I am happy. 
             He is not happy.",
             
             "They like baseball.
             We do not like football.")


# slide 39
get_sentences(ex_text) %>%
  sentiment()

get_sentences(ex_text) %>%
  sentiment_by()


# slide 41
library(sentimentr)

get_sentences(ex_text) %>%
  sentiment_by() %>%
  highlight()


# slide 44
sent_text <- data_frame(sentence = c("Rats seem to naturally enjoy the stimulating effects of certain drugs.
                                     However, most find hallucinogenic drugs to be quite aversive.",
                                     "Catheterization and cannulation surgeries are used to introduce drugs into the rat's nervous system. 
                                     Unfortunately, Institutional Review Boards do not allow undergraduates to undergo similar procedures.",
                                     "A recent meta-review of leading academic psychology publications revealed a shocking proportion of studies are not reproducible.
                                     Of all the sub-disciplines, social psychology fared the worst; three in four social psych studies were found to be non-replicable.",
                                     "In recent years, social psychology has been marred by investigators using dubious research methods, or outright fabricating data.
                                     Nevertheless, these scandals have proven to be lucrative for the careers of certain quantitative psychologists, who have risen to prominence by exposing such practices."))

sent_text %>%
  get_sentences() %>%
  sentiment_by() %>%
  highlight()




# amazon reviews

# slide 47
library(readr)

reviews_raw <- read_csv("7817_1.csv")
reviews <- reviews_raw[!is.na(reviews_raw$reviews.rating),] %>%
  select(name, reviews.username, reviews.rating, reviews.text) %>%
  get_sentences()


# slide 49
reviews_sentiments <- sentiment_by(reviews$reviews.text,
                                   list(reviews$name,
                                        reviews$reviews.rating))

# slides 50 & 51
ggplot(reviews_sentiments, aes(x = reviews.rating,
                               y = ave_sentiment,
                               group = reviews.rating)) +
  geom_boxplot() +
  labs(x = "Star rating", y = "Average Sentiment") +
  theme_bw()

# slides 52-54
reviews_products <- sentiment_by(reviews$reviews.text,
                                 list(reviews$name))
plot(reviews_products)


