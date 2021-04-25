#libraries
suppressPackageStartupMessages(library(quanteda))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table))

#The model is a vector of data.tables


#returns a corpus from a file
readFileToCorpus_v4 <- function (filename,name,source,samplepct) {
    #build the full path
    f<-paste(filename,sep="")
    #read the file
    flines <- readLines(f, encoding = "UTF-8")
    #sample the file according to the parameter
    slines<-sample(flines,length(flines)*samplepct)
    #join the lines-suppressed v3
    #text <- paste(slines,collapse="\n")
    #build a corpus from the text
    c<-corpus(slines)
    #set as docvar the source for the text
    docvars(c,field="source")<-source
    #set a name for the corpus
    docnames(c)<-gsub("text",name,docnames(c))
    #return the corpus
    c
}

#load the corpus from the three training files
loadCorpus_v4 <- function(path, pctSampling) {
    #build three corpus, one for each file, with a sample of samplepct%
    newsCorpus <- readFileToCorpus_v4(paste(path,"en_US.news.txt",sep="/"),"News","News",pctSampling)
    blogsCorpus <- readFileToCorpus_v4(paste(path,"en_US.blogs.txt",sep="/"),"Blogs","Blog",pctSampling)
    twitCorpus <- readFileToCorpus_v4(paste(path,"en_US.twitter.txt",sep="/"),"Twitter","Twitter",pctSampling)
    
    #join the three corpus in a bigger one
    vCorpus <- newsCorpus + blogsCorpus + twitCorpus
    
    #delete the partial corpora to save ram
    rm(newsCorpus)
    rm(blogsCorpus)
    rm(twitCorpus)
    
    vCorpus
}

#tokenize the corpus
tokenize_v4 <- function(corpus) {
    t1<-tokens(corpus,what="sentence")
    t1<-gsub("_","-",t1)
    #tokenization with the cleaning
    t<-tokens(tolower(t1),
              remove_punct = TRUE,
              remove_symbols = TRUE,
              remove_numbers = TRUE,
              remove_url = TRUE,
              split_hyphens = TRUE)
    #erase the twitter tags
    t<-tokens_remove(t,pattern="#*")
    #erase mentions and mail
    t<-tokens_remove(t,pattern="@.",valuetype="regex")
    #erase numbers extended
    t<-tokens_remove(t,pattern="^[0-9]",valuetype="regex")
    t<-tokens_remove(t,pattern="[0-9].[a-zA-Z].",valuetype="regex")
}

#save the variable model
saveModel_v4 <- function(model,persistenceName) {
    saveRDS(model,persistenceName)
}


#load the variable model from persistent filename
loadModel_v4 <- function(persistenceName) {
    model<-readRDS(persistenceName)
}


getSuggesionList_v4 <- function(model,sentence,list_length,use_pattern) {
    #building a suggestion list of <list_length> for sentence <sentence>
    #we have <pending_list> elements left on our list
    pending_list<-list_length
    suggestion_list<-vector()
    
    
    #extract words from our sentence
    words<-tolower(unlist(strsplit(sentence," ")))
    
    #if our sentence have at least 2 words, we continue our way
    l<-length(words)
    
    
    #if use_pattern is TRUE, that has to say that the last word is a pattern to have in mind
    if (use_pattern) {
        #last word is our pattern
        applyPattern<-paste("^",words[l],sep="")
        #rest of words to tokenize excludes the pattern
        l<-l-1
    }
    
    #use 5grams if we have enough length
    # if (l>=4) {
    #     tempdt<-model$g5
    #     
    #     #extract the list of the matching trigrams
    #     sugg<-tempdt[.(words[l-3],words[l-2],words[l-1],words[l])]
    #     
    #     #order by frequency
    #     sugg<-sugg[order(-freq)]
    #     if(use_pattern) {
    #         sugg<-grep(applyPattern,sugg[,fifth],value=TRUE)
    #     } else {
    #         sugg<-sugg[,fifth]
    #     }
    #     
    #     if(length(sugg)>0) {
    #         ll<-length(sugg)
    #         #get all the suggestion list, or the existing trigrams
    #         sl<-sugg[1:min(ll,pending_list)]
    #         suggestion_list<-sl
    #         #pending_list is updated
    #         pending_list<-pending_list-min(ll,list_length)
    #     }
    # }
    # #use 4grams if we have enough length in the sentence and positions left on suggestion list
    # if(l>=3 && pending_list>0) {
    #     tempdt<-model$g4
    #     
    #     #extract the list of the matching trigrams
    #     sugg<-tempdt[.(words[l-2],words[l-1],words[l])]
    #     sugg<-sugg[!(fourth %in% suggestion_list)]
    #     
    #     
    #     #order by frequency
    #     sugg<-sugg[order(-freq)]
    #     if(use_pattern) {
    #         sugg<-grep(applyPattern,sugg[,fourth],value=TRUE)
    #     } else {
    #         sugg<-sugg[,fourth]
    #     }
    #     
    #     if(length(sugg)>0) {
    #         ll<-length(sugg)
    #         #get all the suggestion list, or the existing trigrams
    #         sl<-sugg[1:min(ll,pending_list)]
    #         suggestion_list<-c(suggestion_list,sl)
    #         #pending_list is updated
    #         pending_list<-pending_list-min(ll,list_length)
    #     }
    #     
    # }
    
    #use 3grams if we have enough length in the sentence and positions left on suggestion list
    if(l>=2 && pending_list>0) {
        tempdt<-model$g3
        
        #extract the list of the matching trigrams
        sugg<-tempdt[.(words[l-1],words[l])]
        sugg<-sugg[!(third %in% suggestion_list)]
        
        #order by frequency
        sugg<-sugg[order(-Prob)]
        if(use_pattern) {
            sugg<-grep(applyPattern,sugg[,third],value=TRUE)
        } else {
            sugg<-sugg[,third]
        }
        
        if(length(sugg)>0) {
            ll<-length(sugg)
            #get all the suggestion list, or the existing trigrams
            sl<-sugg[1:min(ll,pending_list)]
            suggestion_list<-c(suggestion_list,sl)
            #pending_list is updated
            pending_list<-pending_list-min(ll,list_length)
        }
        
    }
    
    #use 2grams if we have enough length in the sentence and positions left on suggestion list
    if(l>=1 && pending_list>0) {
        tempdt<-model$g2
        
        #extract the list of the matching trigrams
        sugg<-tempdt[.(words[l])]
        sugg<-sugg[!(second %in% suggestion_list)]
        
        #order by frequency
        sugg<-sugg[order(-Prob)]
        if(use_pattern) {
            sugg<-grep(applyPattern,sugg[,second],value=TRUE)
        } else {
            sugg<-sugg[,second]
        }
        
        if(length(sugg)>0) {
            ll<-length(sugg)
            #get all the suggestion list, or the existing trigrams
            sl<-sugg[1:min(ll,pending_list)]
            suggestion_list<-c(suggestion_list,sl)
            #pending_list is updated
            pending_list<-pending_list-min(ll,list_length)
        }
        
    }
    
    #use unigrams if we have enough length in the sentence and positions left on suggestion list
    if(pending_list>0) {
        
        
        #we take the most probable unigrams
        dt<-(model$g1)[!(gram %in% suggestion_list)]
        dt<-dt[order(-Prob)]
        
        if(use_pattern) {
            sugg<-grep(applyPattern,dt[,gram],value=TRUE)
        } else {
            sugg<-dt[,gram]
        }
        suggestion_list<-c(suggestion_list,
                           sugg[1:pending_list])
    }
    
    #our suggestion list is the union of the three lists
    suggestion_list<-suggestion_list[!is.na(suggestion_list)]
    suggestion_list
}
