# Follwoer-analysis
Follower analysis of COVID-19 experts' Twitter accounts 
```{r}

library(tidyverse)
library(rtweet)
library(igraph)
library(tidygraph)
library(ggraph)
#library(ggplot2)
#library(hrbrthemes)
#library(UpSetR)
#windowsFonts()
library(extrafont)
#font_import()
#loadfonts(device = "win")

#You can find the name of a font you need for the family parameter of element_text with the following code snippet:

# names(wf[wf=="TT Times New Roman"])

```


1. GHS
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("GHS")
my_friends <- get_friends("GHS")
my_friends %>% 
  select(user_id) %>% 
  mutate(GHS_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_GHS = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\GHS.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\GHS.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\GHS.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\GHS.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```


2. _HannahRitchie
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("_HannahRitchie")
my_friends <- get_friends("_HannahRitchie")
my_friends %>% 
  select(user_id) %>% 
  mutate(HannahRitchie_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows__HannahRitchie = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\_HannahRitchie.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\_HannahRitchie.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\_HannahRitchie.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\_HannahRitchie.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



3. AdamJKucharski
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("AdamJKucharski")
my_friends <- get_friends("AdamJKucharski")
my_friends %>% 
  select(user_id) %>% 
  mutate(AdamJKucharski_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_AdamJKucharski = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\AdamJKucharski.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\AdamJKucharski.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\AdamJKucharski.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\AdamJKucharski.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



4. aetiology
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("aetiology")
my_friends <- get_friends("aetiology")
my_friends %>% 
  select(user_id) %>% 
  mutate(aetiology_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_aetiology = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\aetiology.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\aetiology.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\aetiology.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\aetiology.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



5. alexandraphelan
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("alexandraphelan")
my_friends <- get_friends("alexandraphelan")
my_friends %>% 
  select(user_id) %>% 
  mutate(alexandraphelan_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_alexandraphelan = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\alexandraphelan.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\alexandraphelan.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\alexandraphelan.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\alexandraphelan.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



6. AmeshAA
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("AmeshAA")
my_friends <- get_friends("AmeshAA")
my_friends %>% 
  select(user_id) %>% 
  mutate(AmeshAA_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_AmeshAA = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\AmeshAA.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\AmeshAA.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\AmeshAA.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\AmeshAA.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



7. ashishkjha
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("ashishkjha")
my_friends <- get_friends("ashishkjha")
my_friends %>% 
  select(user_id) %>% 
  mutate(ashishkjha_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_ashishkjha = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\ashishkjha.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\ashishkjha.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\ashishkjha.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\ashishkjha.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```


8. Atul_Gawande
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("Atul_Gawande")
my_friends <- get_friends("Atul_Gawande")
my_friends %>% 
  select(user_id) %>% 
  mutate(Atul_Gawande_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_Atul_Gawande = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\Atul_Gawande.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\Atul_Gawande.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\Atul_Gawande.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\Atul_Gawande.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



9.BillHanage
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("BillHanage")
my_friends <- get_friends("BillHanage")
my_friends %>% 
  select(user_id) %>% 
  mutate(BillHanage_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_BillHanage = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\BillHanage.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\BillHanage.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\BillHanage.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\BillHanage.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



10. bogochisaac
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("bogochisaac")
my_friends <- get_friends("bogochisaac")
my_friends %>% 
  select(user_id) %>% 
  mutate(bogochisaac_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_bogochisaac = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\bogochisaac.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\bogochisaac.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\bogochisaac.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\bogochisaac.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



11. CaulfieldTim
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("CaulfieldTim")
my_friends <- get_friends("CaulfieldTim")
my_friends %>% 
  select(user_id) %>% 
  mutate(CaulfieldTim_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_CaulfieldTim = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id,15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\CaulfieldTim.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\CaulfieldTim.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\CaulfieldTim.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\CaulfieldTim.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



12. CDCDirector
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("CDCDirector")
my_friends <- get_friends("CDCDirector")
my_friends %>% 
  select(user_id) %>% 
  mutate(CDCDirector_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_CDCDirector = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\CDCDirector.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\CDCDirector.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\CDCDirector.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\CDCDirector.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



13. celinegounder
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("celinegounder")
my_friends <- get_friends("celinegounder")
my_friends %>% 
  select(user_id) %>% 
  mutate(celinegounder_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_celinegounder = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\celinegounder.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\celinegounder.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\celinegounder.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\celinegounder.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



14. chngin_the_wrld
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("chngin_the_wrld")
my_friends <- get_friends("chngin_the_wrld")
my_friends %>% 
  select(user_id) %>% 
  mutate(chngin_the_wrld_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_chngin_the_wrld = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\chngin_the_wrld.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\chngin_the_wrld.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\chngin_the_wrld.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\chngin_the_wrld.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




15. cmyeaton
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("cmyeaton")
my_friends <- get_friends("cmyeaton")
my_friends %>% 
  select(user_id) %>% 
  mutate(cmyeaton_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_cmyeaton = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\cmyeaton.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\cmyeaton.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\cmyeaton.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\cmyeaton.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




16. cnnbrk
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("cnnbrk")
my_friends <- get_friends("cnnbrk")
my_friends %>% 
  select(user_id) %>% 
  mutate(cnnbrk_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_cnnbrk = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\cnnbrk.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\cnnbrk.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\cnnbrk.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\cnnbrk.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



17. DavidJuurlink
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("DavidJuurlink")
my_friends <- get_friends("DavidJuurlink")
my_friends %>% 
  select(user_id) %>% 
  mutate(DavidJuurlink_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_DavidJuurlink = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DavidJuurlink.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DavidJuurlink.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DavidJuurlink.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DavidJuurlink.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



18. devisridhar
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("devisridhar")
my_friends <- get_friends("devisridhar")
my_friends %>% 
  select(user_id) %>% 
  mutate(devisridhar_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_devisridhar = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\devisridhar.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\devisridhar.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\devisridhar.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\devisridhar.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



19. drewaharris
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("drewaharris")
my_friends <- get_friends("drewaharris")
my_friends %>% 
  select(user_id) %>% 
  mutate(drewaharris_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_drewaharris = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\drewaharris.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\drewaharris.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\drewaharris.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\drewaharris.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




20. DrMikeRyan
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("DrMikeRyan")
my_friends <- get_friends("DrMikeRyan")
my_friends %>% 
  select(user_id) %>% 
  mutate(DrMikeRyan_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_DrMikeRyan = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrMikeRyan.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrMikeRyan.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrMikeRyan.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrMikeRyan.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




21. DrNancyM_CDC

```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("DrNancyM_CDC")
my_friends <- get_friends("DrNancyM_CDC")
my_friends %>% 
  select(user_id) %>% 
  mutate(DrNancyM_CDC_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_DrNancyM_CDC = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrNancyM_CDC.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrNancyM_CDC.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrNancyM_CDC.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrNancyM_CDC.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




22. DrPChouinard
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("DrPChouinard")
my_friends <- get_friends("DrPChouinard")
my_friends %>% 
  select(user_id) %>% 
  mutate(DrPChouinard_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_DrPChouinard = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrPChouinard.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrPChouinard.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrPChouinard.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrPChouinard.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



23. DrTedros
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("DrTedros")
my_friends <- get_friends("DrTedros")
my_friends %>% 
  select(user_id) %>% 
  mutate(DrTedros_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_DrTedros = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrTedros.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrTedros.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrTedros.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrTedros.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




24. DrTomFrieden
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("DrTomFrieden")
my_friends <- get_friends("DrTomFrieden")
my_friends %>% 
  select(user_id) %>% 
  mutate(DrTomFrieden_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_DrTomFrieden = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrTomFrieden.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\DrTomFrieden.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrTomFrieden.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\DrTomFrieden.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



25. epstein_dan
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("epstein_dan")
my_friends <- get_friends("epstein_dan")
my_friends %>% 
  select(user_id) %>% 
  mutate(epstein_dan_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_epstein_dan = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\epstein_dan.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\epstein_dan.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\epstein_dan.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\epstein_dan.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




26. florian_krammer
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("florian_krammer")
my_friends <- get_friends("florian_krammer")
my_friends %>% 
  select(user_id) %>% 
  mutate(florian_krammer_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_florian_krammer = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\florian_krammer.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\florian_krammer.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\florian_krammer.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\florian_krammer.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




27. HelenBranswell
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("HelenBranswell")
my_friends <- get_friends("HelenBranswell")
my_friends %>% 
  select(user_id) %>% 
  mutate(HelenBranswell_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_HelenBranswell = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\HelenBranswell.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\HelenBranswell.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\HelenBranswell.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\HelenBranswell.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




28. JAMA_current
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("JAMA_current")
my_friends <- get_friends("JAMA_current")
my_friends %>% 
  select(user_id) %>% 
  mutate(JAMA_current_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_JAMA_current = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\JAMA_current.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\JAMA_current.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\JAMA_current.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\JAMA_current.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




29. JeremyKonyndyk
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("JeremyKonyndyk")
my_friends <- get_friends("JeremyKonyndyk")
my_friends %>% 
  select(user_id) %>% 
  mutate(JeremyKonyndyk_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_JeremyKonyndyk = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\JeremyKonyndyk.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\JeremyKonyndyk.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\JeremyKonyndyk.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\JeremyKonyndyk.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




30. JohnsHopkinsSPH
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("JohnsHopkinsSPH")
my_friends <- get_friends("JohnsHopkinsSPH")
my_friends %>% 
  select(user_id) %>% 
  mutate(JohnsHopkinsSPH_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_JohnsHopkinsSPH = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\JohnsHopkinsSPH.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\JohnsHopkinsSPH.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\JohnsHopkinsSPH.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\JohnsHopkinsSPH.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



31. juliaoftoronto
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("juliaoftoronto")
my_friends <- get_friends("juliaoftoronto")
my_friends %>% 
  select(user_id) %>% 
  mutate(juliaoftoronto_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_juliaoftoronto = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\juliaoftoronto.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\juliaoftoronto.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\juliaoftoronto.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\juliaoftoronto.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





32. kakape

```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("kakape")
my_friends <- get_friends("kakape")
my_friends %>% 
  select(user_id) %>% 
  mutate(kakape_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_kakape = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\kakape.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\kakape.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\kakape.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\kakape.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




33. MackayIM

```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("MackayIM")
my_friends <- get_friends("MackayIM")
my_friends %>% 
  select(user_id) %>% 
  mutate(MackayIM_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_MackayIM = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\MackayIM.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\MackayIM.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\MackayIM.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\MackayIM.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




34. MaxCRoser
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("MaxCRoser")
my_friends <- get_friends("MaxCRoser")
my_friends %>% 
  select(user_id) %>% 
  mutate(MaxCRoser_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_MaxCRoser = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\MaxCRoser.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\MaxCRoser.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\MaxCRoser.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\MaxCRoser.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




35. michaelmina_lab
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("michaelmina_lab")
my_friends <- get_friends("michaelmina_lab")
my_friends %>% 
  select(user_id) %>% 
  mutate(michaelmina_lab_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_michaelmina_lab = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\michaelmina_lab.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\michaelmina_lab.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\michaelmina_lab.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\michaelmina_lab.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




36. mlipsitch

```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("mlipsitch")
my_friends <- get_friends("mlipsitch")
my_friends %>% 
  select(user_id) %>% 
  mutate(mlipsitch_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_mlipsitch = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\mlipsitch.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\mlipsitch.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\mlipsitch.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\mlipsitch.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



37. mugecevik
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("mugecevik")
my_friends <- get_friends("mugecevik")
my_friends %>% 
  select(user_id) %>% 
  mutate(mugecevik_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_mugecevik = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\mugecevik.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\mugecevik.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\mugecevik.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\mugecevik.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




38. mvankerkhove
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("mvankerkhove")
my_friends <- get_friends("mvankerkhove")
my_friends %>% 
  select(user_id) %>% 
  mutate(mvankerkhove_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_mvankerkhove = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\mvankerkhove.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\mvankerkhove.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\mvankerkhove.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\mvankerkhove.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




39. nytimes
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("nytimes")
my_friends <- get_friends("nytimes")
my_friends %>% 
  select(user_id) %>% 
  mutate(nytimes_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_nytimes = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\nytimes.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\nytimes.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\nytimes.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\nytimes.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




40. NYUDocs
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("NYUDocs")
my_friends <- get_friends("NYUDocs")
my_friends %>% 
  select(user_id) %>% 
  mutate(NYUDocs_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_NYUDocs = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\NYUDocs.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\NYUDocs.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\NYUDocs.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\NYUDocs.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




41. profvrr
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("profvrr")
my_friends <- get_friends("profvrr")
my_friends %>% 
  select(user_id) %>% 
  mutate(profvrr_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_profvrr = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\profvrr.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\profvrr.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\profvrr.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\profvrr.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




42. projecthopeorg

```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("projecthopeorg")
my_friends <- get_friends("projecthopeorg")
my_friends %>% 
  select(user_id) %>% 
  mutate(projecthopeorg_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_projecthopeorg = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\projecthopeorg.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\projecthopeorg.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\projecthopeorg.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\projecthopeorg.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```



43. PublicHealth
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("PublicHealth")
my_friends <- get_friends("PublicHealth")
my_friends %>% 
  select(user_id) %>% 
  mutate(PublicHealth_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_PublicHealth = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\PublicHealth.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\PublicHealth.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\PublicHealth.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\PublicHealth.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




44. RoopaDhatt
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("RoopaDhatt")
my_friends <- get_friends("RoopaDhatt")
my_friends %>% 
  select(user_id) %>% 
  mutate(RoopaDhatt_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_RoopaDhatt = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\RoopaDhatt.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\RoopaDhatt.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\RoopaDhatt.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\RoopaDhatt.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





45. SavetheChildren
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("SavetheChildren")
my_friends <- get_friends("SavetheChildren")
my_friends %>% 
  select(user_id) %>% 
  mutate(SavetheChildren_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_SavetheChildren = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\SavetheChildren.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\SavetheChildren.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\SavetheChildren.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\SavetheChildren.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




46. SCBriand
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("SCBriand")
my_friends <- get_friends("SCBriand")
my_friends %>% 
  select(user_id) %>% 
  mutate(SCBriand_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_SCBriand = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\SCBriand.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\SCBriand.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\SCBriand.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\SCBriand.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





47. ScottGottliebMD
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("ScottGottliebMD")
my_friends <- get_friends("ScottGottliebMD")
my_friends %>% 
  select(user_id) %>% 
  mutate(ScottGottliebMD_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_ScottGottliebMD = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\ScottGottliebMD.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\ScottGottliebMD.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\ScottGottliebMD.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\ScottGottliebMD.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




48. SteveFDA
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("SteveFDA")
my_friends <- get_friends("SteveFDA")
my_friends %>% 
  select(user_id) %>% 
  mutate(SteveFDA_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_SteveFDA = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\SteveFDA.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\SteveFDA.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\SteveFDA.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\SteveFDA.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





49. T_Inglesby
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("T_Inglesby")
my_friends <- get_friends("T_Inglesby")
my_friends %>% 
  select(user_id) %>% 
  mutate(T_Inglesby_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_T_Inglesby = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\T_Inglesby.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\T_Inglesby.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\T_Inglesby.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\T_Inglesby.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




50. trishgreenhalgh
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("trishgreenhalgh")
my_friends <- get_friends("trishgreenhalgh")
my_friends %>% 
  select(user_id) %>% 
  mutate(trishgreenhalgh_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_trishgreenhalgh = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\trishgreenhalgh.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\trishgreenhalgh.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\trishgreenhalgh.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\trishgreenhalgh.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





51. trvrb
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("trvrb")
my_friends <- get_friends("trvrb")
my_friends %>% 
  select(user_id) %>% 
  mutate(trvrb_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_trvrb = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\trvrb.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\trvrb.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\trvrb.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\trvrb.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```




52. US_FDA
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("US_FDA")
my_friends <- get_friends("US_FDA")
my_friends %>% 
  select(user_id) %>% 
  mutate(US_FDA_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_US_FDA = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\US_FDA.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\US_FDA.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\US_FDA.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\US_FDA.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





53. UWVirology
```{r}
twitter_token <- create_token(
  app = "Persian007",
  consumer_key = "yrQ7rQdR2oKI6sr7pkpxCkUUT" ,
  consumer_secret = "EZrjQc9ZOJuK1IMQBuhNxCOdsaJiTTyXL8SN9EeLJqCBfee9cF" ,
access_token="1268243337552596993-GMnPecv7cApJgSGCFol1de7p4WAZNk", 
access_secret="0G5pn3Y8dVYIVcRCfC2ksypnjdmZjSysw8ew0z1IB58dj",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("UWVirology")
my_friends <- get_friends("UWVirology")
my_friends %>% 
  select(user_id) %>% 
  mutate(UWVirology_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_UWVirology = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\UWVirology.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\UWVirology.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\UWVirology.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\UWVirology.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





54. vivek_murthy
```{r}
twitter_token <- create_token(
  app = "Miladn5",
  consumer_key = "SdREvqn8hS4b368N0Zob0NM6t" ,
  consumer_secret = "qvsF39e5Dc3f3Y2bySdqSa9Lxbsiw1HiSYjNTaBCjE5ifRhAOd" ,
access_token="1189228289568755712-kqRUC2oxVsOOhzo0dyXxUga9cNxdwj", 
access_secret="jCjZZzcw1r7JPYKFY2OLkNj2DW5v4QLdtwSinGBozw3qi",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("vivek_murthy")
my_friends <- get_friends("vivek_murthy")
my_friends %>% 
  select(user_id) %>% 
  mutate(vivek_murthy_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_vivek_murthy = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\vivek_murthy.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\vivek_murthy.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\vivek_murthy.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\vivek_murthy.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```





55. WHO
```{r}
twitter_token <- create_token(
  app = "mn123",
  consumer_key = "beCHc0RxuYCffaLne2bErZGEh" ,
  consumer_secret = "ChOZkIXXfm5PcfzruFURUteo7j8RDCaVsXf0fo71JTVYY6hTtr" ,
access_token="1258855138527150091-SDy8N5aVZQKkMhzrQ3pJSxdO4h81lV", 
access_secret="WY9BhrlIsaW3dUSgHUD4EFhIb7LAMILjWT6N2zNVaTJCI",
  set_renv = TRUE)

set.seed(12345)
followers <- get_followers("WHO")
my_friends <- get_friends("WHO")
my_friends %>% 
  select(user_id) %>% 
  mutate(WHO_follows = TRUE) %>% 
  full_join(followers %>% mutate(follows_WHO = TRUE), by = 'user_id') %>% 
  mutate_all(funs(replace(., which(is.na(.)), FALSE))) %>% 
  select(2:3) %>% table

ids <- base::sample(followers$user_id, 15,replace = FALSE)
# create empty list to store results
friends <- list()
# start loop
for (a in 1:length(ids)){
  friends[[a]] <- get_friends(ids[a])
  
  # pause if divisible by 15
  if (a %% 15 == 0){
    Sys.sleep(15*60) # must enter time in seconds
  }}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

write_csv(friends,"C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\WHO.csv")
friends <- read_csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\friends\\WHO.csv")

filter(friends, friend %in% user)
net <- friends %>% 
  group_by(friend) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

g <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph()
g

ggraph(g) +
  geom_edge_link() +
  geom_node_point(size = 3, colour = 'steelblue') +
  theme_graph()

ggraph(g) +
  geom_edge_link(edge_width = 0.15, arrow = arrow(30, unit(.15, "cm"))) +
  theme_graph()

g2 <- net %>% 
  select(user, friend) %>%  # drop the count column
  as_tbl_graph(directed = F) %>%  # make undirected
  activate(nodes) %>% 
  mutate(centrality = centrality_betweenness(), closeness = centrality_closeness(), degree = centrality_degree(), eigen = centrality_eigen())
g2

write.csv(g2, "C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\WHO.csv")
g3 <- read.csv("C:\\Users\\mn852\\OneDrive - Mississippi State University\\0. SNA\\Follower analysis\\centrality output\\WHO.csv")

c(round(mean(g3$centrality),1),round(sd(g3$centrality),1),round(median(g3$centrality),1),round(min(g3$centrality),1), round(max(g3$centrality),1),
round(mean(g3$closeness),5),round(sd(g3$closeness),5),round(median(g3$closeness),5),round(min(g3$closeness),5), round(max(g3$closeness),5),
round(mean(g3$degree),1),round(sd(g3$degree),1),round(median(g3$degree),1),round(min(g3$degree),1), round(max(g3$degree),1),
round(mean(g3$eigen),3),round(sd(g3$eigen),3),round(median(g3$eigen),3),round(min(g3$eigen),3), round(max(g3$eigen),3))

ggraph(g2) +
  geom_edge_link() +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  theme_graph()
```
