#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(data.table)

source("predictModel.R",local=TRUE)

# readFileToCorpus_v4 <- function (filename,name,source,samplepct) {
#     #build the full path
#     f<-paste(filename,sep="")
#     #read the file
#     flines <- readLines(f, encoding = "UTF-8")
#     #sample the file according to the parameter
#     slines<-sample(flines,length(flines)*samplepct)
#     #join the lines-suppressed v3
#     #text <- paste(slines,collapse="\n")
#     #build a corpus from the text
#     c<-corpus(slines)
#     #set as docvar the source for the text
#     docvars(c,field="source")<-source
#     #set a name for the corpus
#     docnames(c)<-gsub("text",name,docnames(c))
#     #return the corpus
#     c
# }
# 
# #load the corpus from the three training files
# loadCorpus_v4 <- function(path, pctSampling) {
#     #build three corpus, one for each file, with a sample of samplepct%
#     newsCorpus <- readFileToCorpus_v4(paste(path,"en_US.news.txt",sep="/"),"News","News",pctSampling)
#     blogsCorpus <- readFileToCorpus_v4(paste(path,"en_US.blogs.txt",sep="/"),"Blogs","Blog",pctSampling)
#     twitCorpus <- readFileToCorpus_v4(paste(path,"en_US.twitter.txt",sep="/"),"Twitter","Twitter",pctSampling)
#     
#     #join the three corpus in a bigger one
#     vCorpus <- newsCorpus + blogsCorpus + twitCorpus
#     
#     #delete the partial corpora to save ram
#     rm(newsCorpus)
#     rm(blogsCorpus)
#     rm(twitCorpus)
#     
#     vCorpus
# }
# 
# #tokenize the corpus
# tokenize_v4 <- function(corpus) {
#     t1<-tokens(corpus,what="sentence")
#     t1<-gsub("_","-",t1)
#     #tokenization with the cleaning
#     t<-tokens(tolower(t1),
#               remove_punct = TRUE,
#               remove_symbols = TRUE,
#               remove_numbers = TRUE,
#               remove_url = TRUE,
#               split_hyphens = TRUE)
#     #erase the twitter tags
#     t<-tokens_remove(t,pattern="#*")
#     #erase mentions and mail
#     t<-tokens_remove(t,pattern="@.",valuetype="regex")
#     #erase numbers extended
#     t<-tokens_remove(t,pattern="^[0-9]",valuetype="regex")
#     t<-tokens_remove(t,pattern="[0-9].[a-zA-Z].",valuetype="regex")
# }
# 
# #save the variable model
# saveModel_v4 <- function(model,persistenceName) {
#     saveRDS(model,persistenceName)
# }
# 
# 
# #load the variable model from persistent filename
# loadModel_v4 <- function(persistenceName) {
#     model<-readRDS(persistenceName)
# }
# 
# 
# getSuggesionList_v4 <- function(model,sentence,list_length,use_pattern) {
#     #building a suggestion list of <list_length> for sentence <sentence>
#     #we have <pending_list> elements left on our list
#     pending_list<-list_length
#     suggestion_list<-vector()
#     
#     
#     #extract words from our sentence
#     words<-tolower(unlist(strsplit(sentence," ")))
#     
#     #if our sentence have at least 2 words, we continue our way
#     l<-length(words)
#     
#     
#     #if use_pattern is TRUE, that has to say that the last word is a pattern to have in mind
#     if (use_pattern) {
#         #last word is our pattern
#         applyPattern<-paste("^",words[l],sep="")
#         #rest of words to tokenize excludes the pattern
#         l<-l-1
#     }
#     
#     #use 5grams if we have enough length
#     # if (l>=4) {
#     #     tempdt<-model$g5
#     #     
#     #     #extract the list of the matching trigrams
#     #     sugg<-tempdt[.(words[l-3],words[l-2],words[l-1],words[l])]
#     #     
#     #     #order by frequency
#     #     sugg<-sugg[order(-freq)]
#     #     if(use_pattern) {
#     #         sugg<-grep(applyPattern,sugg[,fifth],value=TRUE)
#     #     } else {
#     #         sugg<-sugg[,fifth]
#     #     }
#     #     
#     #     if(length(sugg)>0) {
#     #         ll<-length(sugg)
#     #         #get all the suggestion list, or the existing trigrams
#     #         sl<-sugg[1:min(ll,pending_list)]
#     #         suggestion_list<-sl
#     #         #pending_list is updated
#     #         pending_list<-pending_list-min(ll,list_length)
#     #     }
#     # }
#     # #use 4grams if we have enough length in the sentence and positions left on suggestion list
#     # if(l>=3 && pending_list>0) {
#     #     tempdt<-model$g4
#     #     
#     #     #extract the list of the matching trigrams
#     #     sugg<-tempdt[.(words[l-2],words[l-1],words[l])]
#     #     sugg<-sugg[!(fourth %in% suggestion_list)]
#     #     
#     #     
#     #     #order by frequency
#     #     sugg<-sugg[order(-freq)]
#     #     if(use_pattern) {
#     #         sugg<-grep(applyPattern,sugg[,fourth],value=TRUE)
#     #     } else {
#     #         sugg<-sugg[,fourth]
#     #     }
#     #     
#     #     if(length(sugg)>0) {
#     #         ll<-length(sugg)
#     #         #get all the suggestion list, or the existing trigrams
#     #         sl<-sugg[1:min(ll,pending_list)]
#     #         suggestion_list<-c(suggestion_list,sl)
#     #         #pending_list is updated
#     #         pending_list<-pending_list-min(ll,list_length)
#     #     }
#     #     
#     # }
#     
#     #use 3grams if we have enough length in the sentence and positions left on suggestion list
#     if(l>=2 && pending_list>0) {
#         tempdt<-model$g3
#         
#         #extract the list of the matching trigrams
#         sugg<-tempdt[.(words[l-1],words[l])]
#         sugg<-sugg[!(third %in% suggestion_list)]
#         
#         #order by frequency
#         sugg<-sugg[order(-Prob)]
#         if(use_pattern) {
#             sugg<-grep(applyPattern,sugg[,third],value=TRUE)
#         } else {
#             sugg<-sugg[,third]
#         }
#         
#         if(length(sugg)>0) {
#             ll<-length(sugg)
#             #get all the suggestion list, or the existing trigrams
#             sl<-sugg[1:min(ll,pending_list)]
#             suggestion_list<-c(suggestion_list,sl)
#             #pending_list is updated
#             pending_list<-pending_list-min(ll,list_length)
#         }
#         
#     }
#     
#     #use 2grams if we have enough length in the sentence and positions left on suggestion list
#     if(l>=1 && pending_list>0) {
#         tempdt<-model$g2
#         
#         #extract the list of the matching trigrams
#         sugg<-tempdt[.(words[l])]
#         sugg<-sugg[!(second %in% suggestion_list)]
#         
#         #order by frequency
#         sugg<-sugg[order(-Prob)]
#         if(use_pattern) {
#             sugg<-grep(applyPattern,sugg[,second],value=TRUE)
#         } else {
#             sugg<-sugg[,second]
#         }
#         
#         if(length(sugg)>0) {
#             ll<-length(sugg)
#             #get all the suggestion list, or the existing trigrams
#             sl<-sugg[1:min(ll,pending_list)]
#             suggestion_list<-c(suggestion_list,sl)
#             #pending_list is updated
#             pending_list<-pending_list-min(ll,list_length)
#         }
#         
#     }
#     
#     #use unigrams if we have enough length in the sentence and positions left on suggestion list
#     if(pending_list>0) {
#         
#         
#         #we take the most probable unigrams
#         dt<-(model$g1)[!(gram %in% suggestion_list)]
#         dt<-dt[order(-Prob)]
#         
#         if(use_pattern) {
#             sugg<-grep(applyPattern,dt[,gram],value=TRUE)
#         } else {
#             sugg<-dt[,gram]
#         }
#         suggestion_list<-c(suggestion_list,
#                            sugg[1:pending_list])
#     }
#     
#     #our suggestion list is the union of the three lists
#     suggestion_list<-suggestion_list[!is.na(suggestion_list)]
#     suggestion_list
# }
# 
# 
# source("predictModel.R")
# 
# #quanteda options: multithread increased to 4
# #quanteda_options(threads=6)
# 
# ##Kneser-Ney algorithm application
# ##For each word, count the number of BIGRAM TYPES it completes (not occurences)
# ##Every bigram type was a novel continuation the first time it was seen
# ##SO: How Many times does a word w appears as a novel continuation / Total number of word bigram types
# ##Alternative metaphor: number of # of word types seen to precede w / # of words preceding all words
# ## LAMBDA: normalizing constant; the probability mass we've discounted
# ## lambda(w i-1) = d/c(w i-1) | {w: c(w i-1, w)>0}|
# smoothingKneserNey<-function(model) {
#     ##Application of Kneser-Ney in trigrams
#     ###Our rule is: Pkn = max(CKn_i-d,0) / CKn_i_1 + lambda * Pcont (not provided more details for simplicity)
#     ###In explanations, we will use wx as the xth word in each n-gram
#     
#     ###Our probability for the highest n-gram e
#     ###HIGHEST N-GRAM:
#     
#     #####CKn term will be the frequency for the 3gram / freq of its preceding bigram
#     #####For instance: "my dog is"
#     #####                   Numerator: frequency for "my dog is" in the 3grams
#     #####                   Denominator: frequency for "my dog" in the trigrams (as w1,w2)
#     ##### Frequency is available, so we add the calculation for the denominator
#     model$g3[,CKnDenom:=sum(freq),by=.(first,second)]
#     
#     
#     #lambda: 
#     #     d / c(w_i-1_) | {w: c(wi-1, w) > 0} |
#     #lambda: lambda_d/lambdaDen * lambdaNum
#     #lambda_d is fixed
#     
#     ###Pcont: Pcont(wi) = |{wi-1: C(wi-1 wi)>0}| / SUM W'i(|w'i-1:c(w'i-1 w'i)>0|)
#     #### Numeratior: number of different string types preceding the final word
#     #### Denominator: number of different possible n-gram types
#     #### So, numerator is the COUNT (not freq) for the w3
#     #### And denominator is full COUNT (not freq) of trigrams
#     
#     #### Since d is 0, then lambda is 0, and no need to further calculation in our trigrams
#     model$g3[,Prob:=freq/CKnDenom]
#     
#     
#     
#     ###BI-GRAM
#     #bigrams
#     #d is 0.75 now
#     #FIRST TERM: max(CKnNum-0.75,0)/CKnDen
#     #for a bigram w1, w2:
#     #firstTerm1 is the COUNT (different word types) in trigrams, which w1 w2 are the last in the trigram (those are w2 w3 in trigram)
#     #firstTerm2 is the COUNT (different word types) in trigrams, which w1 are the last in the trigrams (this is w3 in trigram)
#     
#     d<-0.75
#     
#     #CKnNum:
#     #extract counts for second and third w3 in trigrams, rename and setkey as first,second to be used in join
#     g3_w2w3_count<-model$g3[,.N,by=.(second,third)]
#     setnames(g3_w2w3_count,"second","first")
#     setnames(g3_w2w3_count,"third","second")
#     setnames(g3_w2w3_count,"N","w1_w2_as_w2_w3_Counts")
#     setkey(g3_w2w3_count,first,second)
#     
#     #join dt2 with this aux join to set cknNumCount
#     dtjoin<-g3_w2w3_count[model$g2]
#     setkey(dtjoin,first,second)
#     model$g2[,cknNumCount:=dtjoin[.(first,second),w1_w2_as_w2_w3_Counts]]
#     
#     #when is NA, we set a 0
#     model$g2[model$g2[,is.na(cknNumCount)],cknNumCount:=0]
#     
#     #when firstTerm is <0.75, then we set to 0.75 to set the MAX for the term
#     model$g2[,cknNum:=cknNumCount*1.0-0.75]
#     model$g2[model$g2[,cknNum<0],cknNum:=0]
#     
#     
#     #firstTerm2:
#     #extract counts for w3 in trigrams (we can use w2 from the previous calculation)
#     dt3_w3_count<-model$g3[,.N,by=.(third)]
#     setnames(dt3_w3_count,"third","second")
#     setnames(dt3_w3_count,"N","w2_as_w3_Counts")
#     setkey(dt3_w3_count,second)
#     
#     setkey(model$g2,second)
#     dtjoin<-dt3_w3_count[model$g2]
#     setkey(dtjoin,first,second)
#     setkey(model$g2,first,second)
#     model$g2[,cknDen:=dtjoin[.(first,second),w2_as_w3_Counts]]
#     
#     model$g2[,firstTerm:=cknNum/cknDen]    
#     
#     #lambda: 
#     #     d / c(w_i-1_) | {w: c(wi-1, w) > 0} |
#     #lambda: lambda_d/lambdaDen * lambdaNum
#     #lambda_d is fixed
#     lambda_d<-0.75
#     
#     #lambdaDen is the total frequecy in the bigrams
#     dtaux<-model$g1[model$g2]
#     setnames(dtaux,"gram","first")
#     setkey(dtaux,first,second)
#     model$g2[,lambdaDen:=dtaux[.(first,second),freq]]
#     
#     
#     model$g2[,lambdaNum:=.N,by=.(first)]
#     model$g2[,lambda:=lambda_d/lambdaDen*lambdaNum]    
#     
#     #PCont for the bigrams
#     pContDen<-nrow(model$g2)
#     model$g2[,pContNum:=.N,by=.(second)]
#     model$g2[,pCont:=pContNum/pContDen]
#     
#     model$g2[,Prob:=firstTerm+lambda*pCont]
#     
#     #for our purposes, the 1gram can be calculated in a simple way, selecting the top 100 elements
#     model$g1<-model$g1[order(-freq)][1:5000]
#     s<-sum(model$g1[,freq])
#     model$g1[,Prob:=freq/s]
#     setkey(model$g1,gram)
#     
#     model
# }
# 
# 
# #buildModel:
# ##param path: path for the training files. Expecting the three files for the capstone
# ##param sampleplct: % sampling for each of the files
# ##param saveFile: name of the file to save the model, if desired. If "", then no saving
# buildModel <- function (path, sample_pct, saveFile) {
#     #seed
#     set.seed(3435)
#     
#     #sample pct
#     sppct<-sample_pct
#     
#     tokenization<-tokenize_v4(loadCorpus_v4(path,sppct))
#     
#     
#     #build the document term matrix for each of the unigrams to five-grams
#     # grams5<-dfm(tokens_ngrams(tokenization,n=5))
#     # 
#     # #compose data.table for n-gram:
#     # #build a dataframe with frequencies
#     # df<-textstat_frequency(grams5)
#     # #build the data table with the gram and each of the frequencies
#     # dtgrams5<-data.table(gram=df$feature, freq=df$frequency)
#     # #split the gram using the "_", saving each piece in the places first to fifth
#     # dtgrams5[,c("first","second","third","fourth","fifth"):=
#     #              tstrsplit(gram,"_",keep=c(1,2,3,4,5),fixed=TRUE)]
#     # #we will use all tokens but last,so our key is all of the pieces except for the last
#     # setkey(dtgrams5,first,second,third,fourth)
#     # 
#     # saveRDS(grams5,paste("grams5_",model_suffix,".RData",sep=""))
#     # rm(grams5)
#     # 
#     # grams4<-dfm(tokens_ngrams(tokenization,n=4))
#     # 
#     # #repeat the logic for the rest of the n-grams
#     # #compose data.table for n-gram:
#     # #build a dataframe with frequencies
#     # df<-textstat_frequency(grams4)
#     # #build the data table with the gram and each of the frequencies
#     # dtgrams4<-data.table(gram=df$feature, freq=df$frequency)
#     # #split the gram using the "_", saving each piece in the places first to fifth
#     # dtgrams4[,c("first","second","third","fourth"):=
#     #              tstrsplit(gram,"_",keep=c(1,2,3,4),fixed=TRUE)]
#     # #we will use all tokens but last,so our key is all of the pieces except for the last
#     # setkey(dtgrams4,first,second,third)
#     # 
#     # saveRDS(grams4,paste("grams4_",model_suffix,".RData",sep=""))
#     # rm(grams4)
#     
#     
#     grams3<-dfm(tokens_ngrams(tokenization,n=3))
#     
#     #pruning: erase the 1 occurence tokens
#     grams3<-dfm_trim(grams3, min_termfreq=2)
#     
#     #build a dataframe with frequencies
#     df<-textstat_frequency(grams3)
#     #build the data table with the gram and each of the frequencies
#     dtgrams3<-data.table(gram=df$feature, freq=df$frequency)
#     #split the gram using the "_", saving each piece in the places first to fifth
#     dtgrams3[,c("first","second","third"):=
#                  tstrsplit(gram,"_",keep=c(1,2,3),fixed=TRUE)]
#     #we will use all tokens but last,so our key is all of the pieces except for the last
#     setkey(dtgrams3,first,second,third)
#     
#     
#     
#     grams2<-dfm(tokens_ngrams(tokenization,n=2))
#     
#     
#     #pruning: erase the 1 occurence tokens
#     grams2<-dfm_trim(grams2, min_termfreq=2)
#     
#     #build a dataframe with frequencies
#     df<-textstat_frequency(grams2)
#     #build the data table with the gram and each of the frequencies
#     dtgrams2<-data.table(gram=df$feature, freq=df$frequency)
#     #split the gram using the "_", saving each piece in the places first to fifth
#     dtgrams2[,c("first","second"):=
#                  tstrsplit(gram,"_",keep=c(1,2),fixed=TRUE)]
#     #we will use all tokens but last,so our key is all of the pieces except for the last
#     setkey(dtgrams2,first,second)
#     
#     grams1<-dfm(tokenization)
#     
#     #pruning: erase the 1 occurence tokens
#     grams1<-dfm_trim(grams1, min_termfreq=2)
#     
#     df<-textstat_frequency(grams1)
#     dtgrams1<-data.table(gram=df$feature, freq=df$frequency)
#     setkey(dtgrams1,gram)
#     
#     model<-vector("list",3)
#     
#     model$g1<-dtgrams1
#     model$g2<-dtgrams2
#     model$g3<-dtgrams3
#     
#     model<-smoothingKneserNey(model)
#     
#     if(saveFile!="") {
#         saveModel_v4(model,saveFile)    
#     }
#     
#     
#     model
# }
# 
# 








model<-loadModel_v4("m210425_10pct.RData")

readyToPredict <- function(inputtext) {
    rslt<-"<empty>"
    if (inputtext=="") {
        rslt<-"<empty>"
    }
    else {
        #get last letter for the input
        #last<-substr(inputtext,nchar(inputtext),nchar(inputtext))
        #if(last==" " | last=="," | last=="!" | last=="." | last=="?") {
        #    rslt<-tolower(inputtext)  
        #}
        text<-tokenize_v4(inputtext)
        rslt<-paste(text[length(text)],collapse=" ")
    }
    
    rslt
}

usePattern <- function(inputtext) {
    rslt<-TRUE
    
    #get last character for the input
    last<-substr(inputtext,nchar(inputtext),nchar(inputtext))
    
    
    #if last character is not a " ", then we use last characters as a pattern for our list
    if(last==" " | last=="," | last=="!" | last=="." | last=="?") {
        rslt<-FALSE
    }
    
    rslt
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #loadModel_v3("m210418.RData")
    
    
    inputText <- reactive({
        t <- input$text
        t
    })  
    
    
    
    output$transformed_text <- renderText({
        #init output
        transformed<-"No text to use in prediction. Most probable words shown"
        
        #clean the input
        text<-readyToPredict(inputText())
        #get if last character is end of sentence
        pat<-usePattern(inputText())
        
        if (text!="<empty>") {
            if (pat) {
                transformed<-paste("Using the text \"",text,"\", but last word is used as a pattern for the prediction for the other three (if possible)",collapse="")
            }
            else {
                transformed<-paste("Using the text \"",text,"\" without pattern",collapse="")
            }
        }

        print(transformed)
        
    })
    
    output$suggestion_list_size <- renderText({
        t<-paste("The total words predicted are up to ",input$suggestions," words",sep="")
    })
    
    output$prediction <- renderTable({
        patt<-usePattern(input$text)
        totalList<-input$suggestions
        
        sl<-getSuggesionList_v4(model,input$text,input$suggestions,patt)
        #if (length(sl)<totalList && patt) {
        #    sl2<-getSuggesionList_v4(model,input$text,(input$suggestions-length(sl))*2,FALSE)
        #    sl<-unique(c(sl,sl2))[1:totalList]
        #}
        
        sl
    },colnames=FALSE)
    
})

