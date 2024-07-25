---
title: "Chinese historiography on the Japan-educated students"
author:
- name: Christian Henriot
  affiliation: Aix-Marseille University
date: "`r lubridate::today()`"
tags: [China, Japan, Student, University, Education, Historiography]  
abstract:  |
  This markdown script presents the core of the methodological framework for my study of Chinese historiography on the the Japan-educated students in the late imperial and republican period. It is based on an initial corpus of 485 references collected in the CNKI database. After selecting only the journal articles, the master thesis, and the doctoral dissertations that contained an abstract, the final corpus is made up of 433 items. The primary tool for this analysis is topic modeling, which serves both to categorize the docuemnts and to identify historiographical trends for in-depth examination.  This study implements a suite of R packages tailored for structural topic modeling, stm and stminsights, as well as tidyverse for data manipulation and ggplot2 for visualization.


  <style>
    body {
    text-align: justify}
  </style>

output: 
  html_document :
    toc: true
    toc_float: True
    #  collapsed: false
    #  smooth_scroll: false
    toc_depth: 3
    number_sections: true
    code_folding: show # hide
    theme: readable # all theme -> https://bootswatch.com/3/
    #fig_width: 10
    #fig_height: 10
    #fig_caption: true
    df_print: paged
---

# Data Preparation and Analysis

## Loading Libraries
To implement this script, you will need the following set of libraries:
```{r setup, include=FALSE, warning=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(topicmodels)
library(broom)
library(quanteda)
library(jiebaR)
library(wordcloud)
library(tm)
library(stm)
library(stminsights)
library(tidytext)
library(reshape)
library(pals)
library(ggraph)
library(igraph)
```

Save and Load Data
The following chunk of script is not active in the markdown script. We suggest to use this in your own script to save all the lines of script as well as the data produced through the script. This will enable you to retrieve all data without having to run the whole script again to obtain the results from data processing.

Save data
```{r eval=FALSE}
save.image('cnkijp.RData')
```

Re-upload saved RData file
```{r eval=FALSE}
load(file = "cnkijp.RData")
```

Upload file. Make sure that the CSV file is present in your directory with the right pathway.
```{r}
CNKI_20231014003254589 <- read_delim("CNKIdata/CNKI-20231014003254589.csv",
delim = ";", escape_double = FALSE, trim_ws = TRUE)
```

Select the relevant columns from the original dataset that will be used analysis.
```{r}
CNKIjp <- CNKI_20231014003254589 %>% select(SrcDatabase, Title, Author, Affiliation, Abstract, `Literature Source`, Year, PageCount)

```

## Data Cleaning

The dataset extracted from CNKI includes items that we want to exclude from analysis. The filter applies negatively to remove all detected terms in the "SrcDatabase" column.
```{r}
# Remove irrelevant types of publications
CNKIjp2 <- CNKIjp %>% filter(!str_detect(SrcDatabase, "报纸"))
CNKIjp2 <- CNKIjp2 %>% filter(!str_detect(SrcDatabase, "中国会议"))
CNKIjp2 <- CNKIjp2 %>% filter(!str_detect(SrcDatabase, "年鉴"))
CNKIjp2 <- CNKIjp2 %>% filter(!str_detect(SrcDatabase, "国际会议"))
CNKIjp2 <- CNKIjp2 %>% filter(!str_detect(Author, "Author"))
```

We also remove the semi-colons that appear after the name of authors. This will not remove the semi-colons between author's names in the case of co-authorship. Since authors are not a target of analysis, we leave the data as is.
```{r}
CNKIjp2 <- CNKIjp2 %>% mutate(Author2 = (str_remove(Author, ";$")))
CNKIjp2 <- CNKIjp2 %>% relocate(Author2, .before = Author)
```

## Grouping and Counting
In the lines of script below, we group by category of data and create statistical measures for each category.
```{r}
# Authors       
CNKIjpAut <- CNKIjp2 %>% group_by(Author) %>% count()   

# Affiliations       
CNKIjpAff <- CNKIjp2 %>% group_by(Affiliation) %>% count()     

# Literature Source       
CNKIjpSou <- CNKIjp2 %>% group_by(`Literature Source`) %>% count()     

# Year       
CNKIjpYea <- CNKIjp2 %>% group_by(Year) %>% count()   
```
 This shows that 363 distinct authors or co-authors have contributed from 1 to 12 papers. The highest number of ciontribution (12) was authored by 徐志民 (中国社会科学院近代史研究所). These authors were affilated to 355 different institutions. Their work was published in 299 distinct publications. 

We examine below the temporal distribition of this scholarly production.
First we filter to retain only the publications produced after 1950
```{r}
CNKIjpYea_fil <- CNKIjpYea %>% filter(Year>1949)
```

Next we plot the data (historgam)
```{r}
# Plot number of dissertations per year 
ggplot(data = CNKIjpYea_fil) + 
  geom_bar(mapping = aes(x = Year, y = n), stat="identity", fill="darkblue") + 
  labs(title = "Number of outputs per year (1950-2023)", 
       subtitle = "Output per year", 
       caption = "based on data extracted from CNKI",
       x = "Year",
       y = "Number of outputs")
```
The level of interest in the topic of Japan-educated students remained very low until the 2000s in Chinese academia. There was a single work between the 1950 and 1970 and only one until 1980. The topic attract some interest at a stable and low level through the next twenty years. The 2000s saw a rapid upsurge that peaked in 2013. The production decreased thereafter to the average level of the 2000s, followed by a resurgence after 2020.

## Topic Modeling on Abstracts

Before we proceed to topic modeling, we need to add an ID column to identify each   row of data.
```{r}
# Add an ID column with incremental digits starting with 'P'
CNKIjp2$ID <- paste0("P", seq(1, nrow(CNKIjp2)))
CNKIjp2 <- CNKIjp2 %>% relocate(ID, .before = SrcDatabase)
```


We create a new file for topic modeling. We select only the necessary columns, namely ID, Source, Abstract (this contains the text on which we apply topic modeling), and Year (to enable time effect on topics).
Next we enable the jieba tokenizer. "Jieba" (Chinese 结巴 for "to stutter") is a Chinese word segmentation module that works well with contemporary Chinese [jieba on GitHub](https://github.com/fxsjy/jieba).
```{r}
CNKIjp_TM <- CNKIjp2 %>% select(ID, SrcDatabase, Abstract, Year)
CNKIjp_TM <- CNKIjp_TM %>% filter(!is.na(Abstract))

# Initialize the jieba tokenizer
jieba_tokenizer <- worker()
```

A preliminary step in topic modeling is to remove the stop words. For most stop words, we can rely on pre-defined libraries, but this will not remove terms that are specific to the corpus that you want to process. In the present case, for example, we do not need the terms that are inevitable linled to the original queried terms such as "留日", "留学生", etc.

We define define custom list of stop words that we combine with the default stop words in jieba.
```{r}
custom_stop_words <- c("留日学生", "留学生", "留学", "本文", "留日", "日本", 
                       "以及", "其中", "研究", "部分", "主要", "分析", "进行", 
                       "影响", "方面", "学生", "中国", "的", "了", "在", "和", 
                       "对", "是", "为", "与", "中", "日", "年", "以", "等", 
                       "后", "人", "也", "上", "他们", "有", "这", "于", "从", 
                       "之", "而", "但", "由", "所", "并", "到", "下", "成", 
                       "及", "多", "地", "者", "此", "作", "个", "来", "向", 
                       "就", "着", "都", "具", "还", "更", "起", "清", "生", 
                       "化", "其", "学", "一", "文", "用", "大", "末", "习", 
                       "立", "会", "出")

all_stop_words <- unique(c(jieba_tokenizer$stop_word, custom_stop_words))

```

The following line of script serves to okenize the text in the Abstract column and to remove the stop words
```{r}
CNKIjp_TM$Tokens <- sapply(CNKIjp_TM$Abstract, function(text) {
  tokens <- unlist(segment(text, jieba_tokenizer))
  tokens <- tokens[!tokens %in% all_stop_words]
  return(paste(tokens, collapse = " "))  
})
```


## Data Exploration

We create a corpus from the "Tokens" column. The "Summary" function display the inital rows of data in the created corpus.
```{r exploration}
lcorpus <- corpus(CNKIjp_TM$Tokens, docnames = CNKIjp_TM$ID)

summary(lcorpus, showmeta = TRUE, 5)
```

From the corpus of tokens, we can produce a preliminary visualization in the form of a word cloud. To do this we use the "wordcloud" library.
```{r}
tokens_list <- unlist(strsplit(CNKIjp_TM$Tokens, " "))
term_freq <- table(tokens_list)
wordcloud(names(term_freq), term_freq, max.words=100, colors=brewer.pal(8, "Dark2"))
```

## Topic Modeling
The actual implementation of topic modeling starts from here. First we transform  textual data into a Document-Term Matrix (DTM). This matrix is a crucial input format for LDA (Latent Dirichlet Allocation) and other topic modeling algorithms.

Because we want the results to be reprocible, we set the value of the random seed to ensure that the results are reproducible. Random processes in R, such as the initialization of the LDA algorithm, will produce the same results each time the script is run when the seed is set to a specific value.

Newt we extract the top terms for each topic identified by the LDA model.The result, terms, is typically a matrix where each column represents a topic and each row represents a term, listing the top 10 terms for each topic.

Finally, we convert the matrix to a data frame, making it easier to handle and manipulate in subsequent analyses or for exporting results.
```{r}
dtm_lda <- DocumentTermMatrix(Corpus(VectorSource(CNKIjp_TM$Tokens)))

# Fit the LDA model
set.seed(123)  # For reproducibility
lda_model <- LDA(dtm_lda, k = 5, control = list(seed = 1234))

# Extract the top terms for each topic
terms <- terms(lda_model, 10)

# Convert to a data frame
topics_df <- as.data.frame(terms)

# View the top terms for each topic
topics_df
```
The "topics_df" data frame contains the top terms for each topic, which can be inspected or used in further analysis. Each column represents a topic, and each row represents one of the top 10 terms for that topic.

## Further analysis with topic modeling using the "stm" library

The use of stm requires a pre-processing phase of processing the raw text data for use in the STM (Structural Topic Model). This proceeds in several steps: 
- Extract and prepare a metadata frame from the original data.
- Process the text data to create a structured corpus.
- Plot the impact of removing infrequent words based on different thresholds.
- Prepare the documents, vocabulary, and metadata for fitting the STM model, applying a threshold to remove words that appear fewer than 5 times. You can modify this threshold.
```{r}
meta <- CNKIjp_TM %>% transmute(ID, Tokens, Year)
corpus <- stm::textProcessor(CNKIjp_TM$Tokens, metadata = meta, stem = FALSE, wordLengths = c(2, Inf), verbose = FALSE)
stm::plotRemoved(corpus$documents, lower.thresh = c(0,10, by=5))
out <- stm::prepDocuments(corpus$documents, corpus$vocab, corpus$meta, lower.thresh = 5)
```

We build the model and plot the result. The "searchK" function searches for the optimal number of topics (K) by fitting STM models with different numbers of topics and evaluating various metrics. When you run plot(Ksearch1), you will see several plots, each representing a different metric across the range of topic numbers (5 to 10 in this case). By examining these plots, you can choose the number of topics that optimizes these metrics, helping you decide on the most appropriate number of topics for your STM analysis.

You can see below that the searchK function produces successive iterations to compute the topics.

```{r message=FALSE, warning=FALSE}
Ksearch1 <- stm::searchK(out$documents, out$vocab, c(5,10), cores = 1)
plot(Ksearch1)
```
Since we chose to computer only two models, the results fall under two very contrasted views, with the 5-topic model offering the most appropriate model. In such cases, however, it may be worth exploring models between 5 and 10 topics.


We can compute various models depending on the Ksearch result (this allows you to explore various models).
```{r}
mod.10 <- stm::stm(out$documents, out$vocab, K=10, data=out$meta, verbose = FALSE)
mod.5 <- stm::stm(out$documents, out$vocab, K=5, data=out$meta, verbose = FALSE)
```

We can introduce time effect on topics. This estimates the impact of the "Year" covariate on the prevalence of the first 10 topics in your STM model. This analysis helps in understanding how topic prevalence changes over time or with other covariates included in your metadata.
```{r}
year10 <- stm::estimateEffect(1:10 ~ Year, mod.10, meta=out$meta)
year5 <- stm::estimateEffect(1:5 ~ Year, mod.5, meta=out$meta)
```


The next chunk of script generates a data frame containing the topic proportions for each document from the fitted STM model (here mod.10), along with the associated metadata. This operation complements the previous step by providing a detailed view of topic proportions at the document level. It allows for a more in-depth analysis of how topics are distributed across documents and how they relate to metadata.
```{r}
topicprop10<-make.dt(mod.10, meta)
topicprop5<-make.dt(mod.5, meta)
```

To visualize topics over time, we need specific libraries to prepare the data and create plots.
```{r}
library(reshape)
library(pals)
```

First, we select the specific columns from data frames that contain topic proportions for documents. The script isolates the columns representing the topic proportions, excluding any metadata or identifiers.
```{r}
topic10prop <- topicprop10 %>% select(c(2:11))
topic5prop <- topicprop5 %>% select(c(2:6))
```

Next we aggregate the topic proportions by year and calculate their mean. for models with 10 and 5 topics. This allows for the analysis of how topic prevalence changes over time, providing insights into temporal trends and shifts in the focus of the documents. 
```{r}
topic_proportion_per_year10 <- aggregate(topic10prop, by = list(Year = CNKIjp_TM$Year), mean)
topic_proportion_per_year5 <- aggregate(topic5prop, by = list(Year = CNKIjp_TM$Year), mean)
```

Finally, we use the melt function from the reshape2 package, to prepare the aggregated topic proportions data for visualization. This  transforms them into a long format often required for creating certain types of visualizations, such as line plots or heatmaps.
```{r}
vizDataFrame10y <- melt(topic_proportion_per_year10, id.vars = "Year")
vizDataFrame5y <- melt(topic_proportion_per_year5, id.vars = "Year")
```

We can now plot topic proportions per year as bar plot, one per model.
```{r}
require(pals)
ggplot(vizDataFrame10y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Topics related to Japan-trained students in CNKI", 
       subtitle = "Topic proportion over time", 
       caption = "10-topic stm model")
```

```{r}
require(pals)
ggplot(vizDataFrame5y, aes(x=Year, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "Topic") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Topics related to Japan-trained students in CNKI", 
       subtitle = "Topic proportion over time", 
       caption = "5-topic stm model")
```

The two plots above show the relative importance of each topic on any given year. This reflects the diversity of the topics found in the corpus over time. It also depends on the number of publications each year. Topic 1 is strongly represented in 1976, whereas Topic 3 ranks first in 1980.

This visualization, however, only indicates the trends, but it does not provide clues as to the content or meaning of each topic. We need to define them based on their content and attribute labels that will summarize what they represent.

To explore the actual content of the topics, we shall use functions of the STM package through a series of visualiztion functions.

## Additional functions of the STM package

The script below generates a histogram plot that visualizes the distribution of topic proportions for a fitted STM model with 5 and 10 topics. This helps you understand how prevalent each topic is across the documents in your corpus, identifying dominant topics and analyzing their spread. It also provides the top terms in the topic.

```{r}
plot.STM(mod.5, "hist")
```
```{r}
plot.STM(mod.10, "hist")
```


We now use the Tidytext library to extract the β (beta) proportions for a 10-topic STM model. The "tidy" function is used to convert a model object into a tidy data frame. The β (beta) matrix in topic modeling represents the probabilities of words given topics. Each entry in the matrix indicates how strongly a word is associated with a particular topic.
```{r}
# extract β proportions for the 10-topic model 
td_beta10 <- tidytext::tidy(mod.10) 

# plot the the distribution of words (10 first words) for each topic 
options(repr.plot.width=7, repr.plot.height=8, repr.plot.res=100) 

td_beta10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities by topic",
       subtitle = "List of the words associated with each topic",
       caption = "Based on CNKI 1950-2023")
```

This plot displays the top ten terms in each topic and their prevalence in the whole corpus. It also shows which topics are overall more common in the corpus.

We can also display the first 5 tokens. The figure can be adjusted to display more tokens (e.g. n=8 to list 8 tokens).

```{r}
plot.STM(mod.5,"summary", n=5)
```

```{r}
plot.STM(mod.10,"summary", n=5)
```

The script belows simply offers a different visualization of the same, with the possibility to choose which statistical index is used. Here we use "FREX". FREX in topic modeling stands for "Frequency and Exclusivity" and is used to identify terms that are both common within a topic and distinctive from other topics. It helps in interpreting and labeling topics by providing a balanced set of representative terms (the alternative measures in the STM packages includes Lift, Score, and Exclusivity).

```{r}
plot.STM(mod.5, "labels", topics=c(1,3,5), label="frex", n=10, width=50)
```

```{r}
plot.STM(mod.10, "labels", topics=c(1,6,8), label="frex", n=10, width=50)
```


## Correlation network visualization of the topics

This script creates a visual representation of topic correlations from a 10-topic STM model. The nodes represent topics, and the edges represent the correlations between these topics. The size of the nodes corresponds to the topic proportions, and the width of the edges corresponds to the strength of the correlations. 

```{r}
library(ggraph)
library(igraph)

stm_corrs <- get_network(model = mod.10, method = 'simple', labels = paste('Topic', 1:10), cutoff = 0.001, cutiso = TRUE)

ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation', title = "Simple method") + 
  scale_edge_width(range = c(1, 3)) +
  theme_graph()
```

In the case of the 10-topic model, we can observe limited connections between the ten topics. Topic 4 and 9 are the most closely related (it means that they share many terms). The other topics are all unrelated, hence very distinct from one another.
