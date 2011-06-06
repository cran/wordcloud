# TODO: Add comment
# 
# Author: ianfellows
###############################################################################


#words: the words
#
#freq: their frequency
#
#scale: the range of the size of the words
#
#min.freq: words with frequency below min.freq will not be plotted
#
#max.words: Maximum number of words to be plotted. least frequent terms dropped
#
#random.order: plot words in random order. If false, 
#              they will be plotted in decreasing frequency
#
#rot.per: % of words with 90 degree rotation
#
#colors: color words from least to most frequent
#
#use.r.layout: if false, then c++ is used for collision detection
#
#...: Additional parameters to be passed to text (and strheight,strwidth).
#	  e.g. control font with vfont.
wordcloud <- function(words,freq,scale=c(4,.5),min.freq=3,max.words=Inf,random.order=TRUE,
		rot.per=.1,colors="black",use.r.layout=FALSE,...){
	tails <- "g|j|p|q|y"
	last <- 1
	nc<- length(colors)
	
	overlap <- function(x1, y1, sw1, sh1) {
		if(!use.r.layout)
			return(.overlap(x1,y1,sw1,sh1,boxes))
		s <- 0
		if (length(boxes) == 0) 
			return(FALSE)
		for (i in c(last,1:length(boxes))) {
			bnds <- boxes[[i]]
			x2 <- bnds[1]
			y2 <- bnds[2]
			sw2 <- bnds[3]
			sh2 <- bnds[4]
			if (x1 < x2) 
				overlap <- x1 + sw1 > x2-s
			else 
				overlap <- x2 + sw2 > x1-s
			
			if (y1 < y2) 
				overlap <- overlap && (y1 + sh1 > y2-s)
			else 
				overlap <- overlap && (y2 + sh2 > y1-s)
			if(overlap){
				last <<- i
				return(TRUE)
			}
		}
		FALSE
	}
	
	ord <- order(freq,decreasing=TRUE)
	words <- words[ord<=max.words]
	freq <- freq[ord<=max.words]
	
	if(random.order)
		ord <- sample.int(length(words))
	else
		ord <- order(freq,decreasing=TRUE)
	words <- words[ord]
	freq <- freq[ord]
	words <- words[freq>=min.freq]
	freq <- freq[freq>=min.freq]
	thetaStep <- .1
	rStep <- .05
	plot.new()
	op <- par("mar")
	par(mar=c(0,0,0,0))
	plot.window(c(0,1),c(0,1),asp=1)
	normedFreq <- freq/max(freq)
	size <- (scale[1]-scale[2])*normedFreq + scale[2]
	boxes <- list()
	
	
	for(i in 1:length(words)){
		rotWord <- runif(1)<rot.per
		r <-0
		theta <- runif(1,0,2*pi)
		x1<-.5
		y1<-.5
		wid <- strwidth(words[i],cex=size[i],...)
		ht <- strheight(words[i],cex=size[i],...)
		#mind your ps and qs
		if(grepl(tails,words[i]))
			ht <- ht + ht*.2
		if(rotWord){
			tmp <- ht
			ht <- wid
			wid <- tmp	
		}
		isOverlaped <- TRUE
		while(isOverlaped){
			if(!overlap(x1-.5*wid,y1-.5*ht,wid,ht) &&
					x1-.5*wid>0 && y1-.5*ht>0 &&
					x1+.5*wid<1 && y1+.5*ht<1){
				cc <- ceiling(nc*normedFreq[i])
				cc <- colors[cc]
				text(x1,y1,words[i],cex=size[i],offset=0,srt=rotWord*90,
						col=cc,...)
				#rect(x1-.5*wid,y1-.5*ht,x1+.5*wid,y1+.5*ht)
				boxes[[length(boxes)+1]] <- c(x1-.5*wid,y1-.5*ht,wid,ht)
				isOverlaped <- FALSE
			}else{
				if(r>sqrt(.5)){
					warning(paste(words[i],
									"could not be fit on page. It will not be plotted."))
					isOverlaped <- FALSE
				}
				theta <- theta+thetaStep
				r <- r + rStep*thetaStep/(2*pi)
				x1 <- .5+r*cos(theta)
				y1 <- .5+r*sin(theta)
			}
		}
	}
	par(mar=op)
	invisible()
}

.overlap <- function(x11,y11,sw11,sh11,boxes1){
	.Call("is_overlap",x11,y11,sw11,sh11,boxes1)
}
