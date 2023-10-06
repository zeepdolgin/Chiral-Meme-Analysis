## This script gets the data from Reddit wallstreetbets subreddit.
## Before running the next 2 lines, make sure that both libraries are installed.

library(tidyverse)
library(RedditExtractoR)
library(plotly)
library(TSstudio)

# download Reddit data into the 'reddit1' object: there's a wait time of 2 seconds between pages,
# so this will take at least 200 seconds.

reddit1 <- get_reddit(subreddit = "wallstreetbets", page_threshold = 10000)


## Change the data type of post_date and comm_date from character to date:

reddit1$post_date <- as.Date(reddit1$post_date,
                            format = "%d-%m-%y")
reddit1$comm_date <- as.Date(reddit1$comm_date,
                            format = "%d-%m-%y")

# Merge new data with old data:

reddit_combined <- merge(reddit, reddit1, all=TRUE)

# remove duplicated rows

reddit_combined <- reddit[!duplicated(reddit$comment),] 

# serializes the 'reddit_combined' object into the project directory.

reddit_combined %>% saveRDS("Data/reddit_combined.RDS")

reddit <- readRDS("Data/reddit_combined.RDS")%>% 
  as_tibble()




# download stock list from here: https://www.NASDAQ.com/market-activity/stocks/screener

stocks <- read_csv("Data/stock_data.csv")

# stocks now contains stocks data.

stocks %>% 
  filter(Symbol == "GME")

stocks %>% 
  filter(Symbol == "AMC")

# With this function we create a reddit_mentions data frame that contains information on:
# 1: Stock symbol mentioned,
# 2: Comment,
# 3: Date,

reg_expression <- regex(paste0("\\b(?:",
                               paste(stocks$Symbol, collapse = "|"),
                               ")\\b"))

reddit_mentions <- reddit %>%
  mutate(stock_mention = str_extract_all(comment, reg_expression)) %>%
  unnest(stock_mention)

## Serializes the data frame into the project directory:

reddit_5 %>% saveRDS("Data/reddit_mentions.RDS")

reddit_mentions <- readRDS("Data/reddit_mentions.RDS")

## reddit_mentions now contains the filtered data.

reddit_mention_counts <- reddit_mentions %>% 
  group_by(post_date, stock_mention) %>% 
  count()

# false positives (non-stock related):
fp <- c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL", "USA", "IT", "WISH",
        LETTERS)

top5 <- reddit_mention_counts %>% 
  group_by(stock_mention) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>%
  filter(!(stock_mention %in% fp)) %>% 
  head(5) %>% 
  pull(stock_mention)
top5
top10 <- reddit_mention_counts %>% 
  group_by(stock_mention) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>%
  filter(!(stock_mention %in% fp)) %>% 
  head(10) %>% 
  pull(stock_mention)
top10

reddit_mention_counts %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = post_date, y = n, color = stock_mention)) +
  geom_line()
plot <- reddit_mention_counts %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = post_date, y = n, color = stock_mention)) +
  geom_line()
plot + theme(panel.background = element_rect(fill = 'white', colour = 'black'))

reddit_mentions %>% 
  filter(!(stock_mention %in% fp)) %>% 
  group_by(stock_mention) %>% 
  count() %>% 
  arrange(-n) %>% 
  print(n = 20)


reddit_mentions %>% 
  filter(!(stock_mention %in% fp)) %>% 
  saveRDS("data/reddit_mentions.RDS")
