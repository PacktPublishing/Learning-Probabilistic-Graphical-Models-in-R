data(NYTimes)
data <- NYTimes[ samples(1:3100, size=1000,replace=F) ] 

matrix <- create_matrix(cbind(as.vector(data$Title),as.vector(data$Subject)), language="english", removeNumbers=TRUE, stemWords=TRUE)
k <- length(unique(data$Topic.Code))
lda <- LDA(matrix, k)

print(lda@gamma[1,])
plot(colSums(lda@gamma)/nrow(lda@gamma),t='h')

sum(sapply( 1:nrow(lda@gamma), function(i) sum(lda@gamma[i,]>0.1) > 1))
