JHU Word Predictor Presentation
========================================================
author: @cybersuno
date: 29/04/2020
autosize: true
transition: rotate
font-family: 'Arial'

<style>

.reveal {
  background-color: #AFE2EF;
}

/* slide titles */
.reveal h3 { 
  font-size: 35px;
  color: darkblue;
}

.reveal h2 {
  font-size: 30px;
}

.reveal p {
  font-size: 25px;
}

.reveal pre code {
  display: block; padding: 0.1em;
  font-size: 1em;
  line-height: 1em;
  background-color: white;
  overflow: visible;
  max-height: none;
  word-wrap: normal;
}

.reveal code {
  overflow: visible;
  max-height: none;
  max-width: none;
}

.reveal code.r {
  background-color: #F8F8F8;
}

.reveal ol{
	font-size: 25px;
}

.reveal ul{
	font-size: 25px;
}
</style>

Objectives
========================================================

This presentation is aimed to show the word predicting application developed for the capstone corresponding to the Data Science specialization by Johns Hopkins University and Coursera

In this presentation we will fulfill the following requirements

- How application is organized and how it works
- What the model behind the scenes was designed
- Which decisions were made for the sake of the efficiency and capacity

The objective of the application is to predict next word. This is achieved by two steps:
- Building a model by collecting information from several texts provided in the course content. For this, I have used the library  [quanteda](http://quanteda.io/)
- The model information is stored and managed in [data.table](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html) structures, that manages this information in a very fast and lightweight form.
- The model is used by a Shiny application which performs the interface to the user



The Application
========================================================

The application have two inputs:

- A slide in which a user can choose the length of the suggestion list
- An input box in which a user can write a sentence

It works in two ways:
- The app tries to predict the word while you are writing. For each character you type the suggestion list is changing meanwhile it have possibilities. In this situation, it uses the characters from the last blank space or dot as a pattern to match the suggestions.
- When you type a blank space, or a dot, the app predicts the following word

For instance:
Writing "my", the app shows as suggestions "my", "myself" or "mystery". When the space is written, it detects the word is finished and a suggestion list could be "sister", "brother", "dad".

The suggestion list is ordered by probability, so the first word is the first suggestion.

The app can be reached [here](https://cybersuno.shinyapps.io/JHU_Word_Predictor/)


The Model: Getting data
========================================================

Each of the three files is sampled: 10% of its length. To build a corpus. This is aimed to have a fast to load model in the shiny application. With quanteda, we add a docvar with the source of the information.

```r
    #(part of a function)
    flines <- readLines(f, encoding = "UTF-8") #read a file
    slines<-sample(flines,length(flines)*samplepct) #parametrized sampling
    c<-corpus(slines) #build a corpus from the text
    docvars(c,field="source")<-source #set as docvar the source for the text
    docnames(c)<-gsub("text",name,docnames(c)) #set a name for the corpus
```

Each file is a corpus, and the three together compose the main corpus to work in following steps.

```r
    newsCorpus <- readFileToCorpus_v4(paste(path,"en_US.news.txt",sep="/"),"News","News",pctSampling)
    blogsCorpus <- readFileToCorpus_v4(paste(path,"en_US.blogs.txt",sep="/"),"Blogs","Blog",pctSampling)
    twitCorpus <- readFileToCorpus_v4(paste(path,"en_US.twitter.txt",sep="/"),"Twitter","Twitter",pctSampling)
    
    vCorpus <- newsCorpus + blogsCorpus + twitCorpus #join the three corpus in a bigger one
```


The model: Cleaning and getting n-grams
========================================================

We tokenize the corpus twice: first to get sentences and result to get tokens. To avoid strange results found in the exploratory analysis, we substitute the underlines for hyphens. Other cleaning is made to erase special values like @users, #hashtags, user names composed by number and letters, etc.

```r
    t1<-tokens(corpus,what="sentence") #separate sentences
    t1<-gsub("_","-",t1) #replace _ for hyphens
    #tokenization with the cleaning
    t<-tokens(tolower(t1),remove_punct = TRUE,remove_symbols = TRUE,remove_numbers = TRUE,remove_url = TRUE,split_hyphens = TRUE) #tokenize
```

With this tokenization, we can build our model. It is composed by 3grams, 2grams and unigrams to apply a backoff: when no prediction is found in the 3grams, we use the 2grams and so on. The probability for each ngram is calculated applying Kneser Ney smoothing.


```r
    grams3<-dfm(tokens_ngrams(tokenization,n=3)) #doc term matrix for ngrams
    grams3<-dfm_trim(grams3, min_termfreq=2) #pruning: erase 1 occurence tokens
    df<-textstat_frequency(grams3) #build a dataframe with frequencies
    dtgrams3<-data.table(gram=df$feature, freq=df$frequency) #to data.table and split the grams
    dtgrams3[,c("first","second","third"):=tstrsplit(gram,"_",keep=c(1,2,3),fixed=TRUE)]
    setkey(dtgrams3,first,second,third) #key
```

With a backoff model with 3grams we have a well balanced model considering weight, accuracy and performance for the shiny app. One of the goals pursued is to have a reasonably good model loaded in the shiny app in few seconds.

Model: smoothing and usage
========================================================

Repeating those steps, we can build data.table for 3, 2 and 1-gram. Our model is a list with the three data.tables. But to  finish our model, probabilities are calculated through a implementation of the Kneser Ney smoothing.

```r
model<-smoothingKneserNey(model)
```

This model can be persisted and promoted together with a suggestion list function. The suggestion list is composed through recursivity (backoff):
- Use the last two words to find the most probable third word in the 3-grams
- If space left in the prediction list, use the last word in the 2-gram
- If space left in the prediction list, use the most probable unigrams
The suggestion list has the capacity to apply a pattern in the resultant preditictions to complete the word if it is not starting from scratch.

The source code can be found in [github repository](https://github.com/cybersuno/JHU_Predictor)

Thank You!


