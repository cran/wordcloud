% 
% TODO: Write Documentation 
% 



\name{wordcloud}
\alias{wordcloud}
\title{
  Plot a word cloud
}
\description{
  Plot a word cloud
}
\usage{
wordcloud(words,freq,scale=c(4,.5),min.freq=3,max.words=Inf,random.order=TRUE,
		rot.per=.1,colors="black",use.r.layout=FALSE,...)
}
\arguments{
  \item{words}{the words}
  \item{freq}{the words}
  \item{scale}{A vector of length 2 indicating the range of the size of the words. }
  \item{min.freq}{words with frequency below min.freq will not be plotted}
  \item{max.words}{Maximum number of words to be plotted. least frequent terms dropped}
  \item{random.order}{plot words in random order. If false, they will be plotted in decreasing frequency}
  \item{rot.per}{proportion words with 90 degree rotation}
  \item{colors}{color words from least to most frequent}
  \item{use.r.layout}{if false, then c++ code is used for collision detection, otherwise R is used}
  \item{...}{Additional parameters to be passed to text (and strheight,strwidth).}
}
\value{
nothing
 }
\seealso{
  \code{\link{text}}
}
\examples{
if(require(tm)){
	data(crude)
	crude <- tm_map(crude, removePunctuation)
	crude <- tm_map(crude, function(x)removeWords(x,stopwords()))
	tdm <- TermDocumentMatrix(crude)
	m <- as.matrix(tdm)
	v <- sort(rowSums(m),decreasing=TRUE)
	d <- data.frame(word = names(v),freq=v)
	
	wordcloud(d$word,d$freq)

	#A bigger cloud with a minimum frequency of 2
	wordcloud(d$word,d$freq,c(8,.3),2)

	#Now lets try it with frequent words plotted first

	wordcloud(d$word,d$freq,c(8,.5),2,,FALSE,.1)

	##### 			with colors 		#####

	if(require(RColorBrewer)){

		pal <- brewer.pal(9,"BuGn")
		pal <- pal[-(1:4)]
		wordcloud(d$word,d$freq,c(8,.3),2,,FALSE,.15,pal)


		pal <- brewer.pal(6,"Dark2")
		pal <- pal[-(1)]
		wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,.15,pal)
	}
	##### 			with font 			#####

	wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,.15,pal,
		vfont=c("gothic english","plain"))

	wordcloud(d$word,d$freq,c(8,.3),2,100,TRUE,.15,pal,vfont=c("script","plain"))
	
	wordcloud(d$word,d$freq,c(8,.3),2,100,TRUE,.15,pal,vfont=c("serif","plain"))
}


} 
