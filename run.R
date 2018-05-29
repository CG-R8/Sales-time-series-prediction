source('data_trend_pred.R')

train <- getdata("product_distribution_training_set.txt")

key_id <- get_key_id("key_production_IDs.txt")

result<- matrix(NA, nrow=28, ncol=101)

#for(i in c(1:101)){
  for(i in c(1:101)){
	result[,i]<-forecast.stl(train[,i])
	#print(result[,i])
	testit(0)
}
output<-get_final(result, key_id, nrow=101, ncol=29)
write.table(output, "output.txt", row.names=FALSE, col.names=FALSE)
testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

