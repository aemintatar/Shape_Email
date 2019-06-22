library(data.table)
library(TDAmapper)
library(Rtsne)
library(igraph)
library(tidyverse)
library(CINNA)
library(ggplot2)

spam<-fread('/Users/emintatar/data_science/Adem_Research/spambase/shape_of_email/spam.txt')
spam<-unique(spam)
ham<-fread('/Users/emintatar/data_science/Adem_Research/spambase/shape_of_email/ham.txt')
ham<-unique(ham)

spam_distance <- as.matrix(dist(spam, method = "manhattan"))
ham_distance <- as.matrix(dist(ham, method = "manhattan"))

set.seed(1564)
ham_tsne<-Rtsne(ham, dims = 2, perplexity=30, verbose=TRUE, max_iter = 3000,pca_scale=TRUE)
spam_tsne<-Rtsne(spam, dims = 2, perplexity=30, verbose=TRUE, max_iter = 3000,pca_scale=TRUE)
interval_list=c(4,5,6,7)
overlap_list=c(40,50,60,70)
bin_list=c(5,7,10)
close_ham_score<-rep(0,48)
average_ham_score<-rep(0,48)
eccen_ham_score<-rep(0,48)
eigen_ham_score<-rep(0,48)
close_spam_score<-rep(0,48)
average_spam_score<-rep(0,48)
eccen_spam_score<-rep(0,48)
eigen_spam_score<-rep(0,48)
i=1
for (interval in interval_list){
  for (overlap in overlap_list){
    for (bin in bin_list){
      #ham
      ham_mapper <- mapper2D(distance_matrix  = ham_distance,filter_values = list(ham_tsne$Y[,1],ham_tsne$Y[,2]),num_intervals = c(interval,interval),percent_overlap = overlap,num_bins_when_clustering = bin)
      ham_graph <- graph.adjacency(ham_mapper$adjacency, mode="undirected")
      dg_ham <- decompose.graph(ham_graph) 
      pdf(sprintf('/Users/emintatar/data_science/Adem_Research/spambase/shape_of_email/ham/%s_%s_%s_mapper.pdf',interval,overlap,bin))
      plot(dg_ham[[1]],layout=layout.kamada.kawai,main=paste("Ham Mapper -",interval,"-",overlap,'-',bin)) #plot largest connected component
      dev.off()
      a_ham_score<-calculate_centralities(dg_ham[[1]], include = 'Average Distance')
      c_ham_score<-calculate_centralities(dg_ham[[1]], include = 'Closeness Centrality (Freeman)')
      e_ham_score<-calculate_centralities(dg_ham[[1]], include = 'Eccentricity Centrality')
      ei_ham_score<-calculate_centralities(dg_ham[[1]], include = 'eigenvector centralities')
      close_ham_score[i]<-c_ham_score[['Closeness Centrality (Freeman)']]
      average_ham_score[i]<-a_ham_score[['Average Distance']]
      eccen_ham_score[i]<-e_ham_score[['Eccentricity Centrality']]
      eigen_ham_score[i]<-ei_ham_score[['eigenvector centralities']]
      #spam
      spam_mapper <- mapper2D(distance_matrix  = spam_distance,filter_values = list(spam_tsne$Y[,1],spam_tsne$Y[,2]),num_intervals = c(interval,interval),percent_overlap = overlap,num_bins_when_clustering = bin)
      spam_graph <- graph.adjacency(spam_mapper$adjacency, mode="undirected")
      dg_spam <- decompose.graph(spam_graph) 
      pdf(sprintf('/Users/emintatar/data_science/Adem_Research/spambase/shape_of_email/spam/%s_%s_%s_mapper.pdf',interval,overlap,bin))
      plot(dg_spam[[1]],layout=layout.kamada.kawai,main=paste("Spam Mapper -",interval,"-",overlap,'-',bin)) #plot largest connected component
      dev.off()
      a_spam_score<-calculate_centralities(dg_spam[[1]], include = 'Average Distance')
      c_spam_score<-calculate_centralities(dg_spam[[1]], include = 'Closeness Centrality (Freeman)')
      e_spam_score<-calculate_centralities(dg_spam[[1]], include = 'Eccentricity Centrality')
      ei_spam_score<-calculate_centralities(dg_spam[[1]], include = 'eigenvector centralities')
      close_spam_score[i]<-c_spam_score[['Closeness Centrality (Freeman)']]
      average_spam_score[i]<-a_spam_score[['Average Distance']]
      eccen_spam_score[i]<-e_spam_score[['Eccentricity Centrality']]
      eigen_spam_score[i]<-ei_spam_score[['eigenvector centralities']]
      i=i+1
    }
  }
}

#box_plot
closeness_score<-c(close_ham_score,close_spam_score)
average_dist_score<-c(average_ham_score,average_spam_score)
eccen_score<-c(eccen_ham_score,eccen_spam_score)
eigen_score<-c(eigen_ham_score,eigen_spam_score)
ham_label<-rep('Ham',48)
spam_label<-rep('Spam',48)
labels<-c(ham_label,spam_label)
results<-data.frame(labels,closeness_score,average_dist_score,eccen_score,eigen_score)

p<-ggplot(results,aes(labels,log(closeness_score)))
p+geom_boxplot(aes(fill=labels))+ggtitle('Closeness Centrality')

p<-ggplot(results,aes(labels,average_dist_score))
p+geom_boxplot(aes(fill=labels))+ggtitle('Average Distance')

p<-ggplot(results,aes(labels,eccen_score))
p+geom_boxplot(aes(fill=labels))+ggtitle('Eccentricity Centrality')

p<-ggplot(results,aes(labels,eigen_score))
p+geom_boxplot(aes(fill=labels))+ggtitle('Eigenvector Centrality')
