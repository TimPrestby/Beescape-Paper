packages = c("ggplot2", "dplyr","tidytext","extrafont","scales","widyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

lapply(packages, require, character.only = TRUE)

#Load fonts
#font_import(paths = "C:/Windows/Fonts",prompt = F)

loadfonts(device = "win", quiet = TRUE)
windowsFonts("trebuchet ms" = windowsFont("trebuchet ms"))

#load datset

df <- read.csv("Qualitative.csv")

df$text= as.character(df$text)

#Break sentences into words 
df_words = df %>% 
  unnest_tokens(word, text) %>%
  count(q, word, sort = TRUE)

###################Get the total words##################
total_words <- df_words %>% 
  group_by(q) %>% 
  summarize(total = sum(n))

#Join the data 
df_words = left_join(df_words, total_words)


##Apply bind_tf_idf to determine more important words 
df_words <- df_words %>%
  bind_tf_idf(word, q, n)

df_words


#Figure out the inverse of the frequency to remove the weight of common words 
df_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#Plot important words 
df_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(q) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = q)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~q, ncol = 1, scales = "free") +
  coord_flip()









############### Cleaned method ################
#Remove stop words
data(stop_words)

df_clean = df_words %>%
  anti_join(stop_words)


total_words <- df_clean %>% 
  group_by(q) %>% 
  summarize(total = sum(n))

#Join the data 
df_clean = left_join(df_clean, total_words)


##Apply bind_tf_idf to determine more important words 
df_clean <- df_clean %>%
  bind_tf_idf(word, q, n)

df_clean


#Figure out the inverse of the frequency to remove the weight of common words 
df_clean %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#Plot important words 
df_clean %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(q) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = q)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~q, ncol = 1, scales = "free") +
  coord_flip()


