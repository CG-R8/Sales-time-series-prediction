require(forecast)

getdata <- function(x){

    data<-as.matrix(read.table(x,header=FALSE))
    
    # Transpose the original data for the convinience
    trans<-t(data)

    # Create a new matrix to also hold the overall sales
    train <- matrix(data=NA, nrow=nrow(trans)-1, ncol=ncol(trans)+1)

    # Sum up the sales for each day in first row
    for(i in c(1:nrow(trans)-1)){
    	train[,1][i] <- sum(data[,i+1])
    }

    # Simply copy the data
    for(i in c(1:ncol(trans))){
    	train[,i+1] <- trans[,i][2:nrow(trans)]
    }
    return(train)
}

get_key_id <- function(x){

	data<-as.matrix(read.table(x,header=FALSE))
	return(data)
}

forecast.stl<-function(x, n.ahead=28) {
	# Computes data-trend prediction using STL with ETS method
	#
	# args:
	# x - Contains list of sales for each 118 days for a key product
	#
	# returns - List of sales for next 28 days

	# Data preprocessing to remove the 0s. So that we can perform 
	# logarithmic operation on the data to reduce it for greater 
	# accuracy.
  
  
  myts = ts(x,frequency=7)
  fit <- stl(myts, s.window="period")
  plot(fit)
  monthplot(myts)
  library(forecast)
  seasonplot(myts) 
  print(myts,calendar = TRUE)
  
  fit <- HoltWinters(myts, alpha=0.992,beta=0, gamma=0)
  mygraph <- forecast(fit, 28)
  plot(forecast(fit, 28)) 
  
  print(round(forecast(fit, 28)$mean),calendar = FALSE)
  ##testit(4)
  #testit(2)

  
  
  
  
  
  
	for(i in c(1:length(x)))
			x[i]=x[i]+1

	# Creating a time series object of the preprocessed data by 
	# performing log on the data.
	# Frequency is set to 30 as the it is a daily data for couple of months.
	# Therefore frequency = 30/1 = 30

	myTs<-ts(log(x), start=1, frequency=30)
 # plot(myTs)

	# Performing Seasonal Decomposition of Time Series by Loess (STL) and Error Trend Seasonal (ETS) method
	# stlf() combines stlm and forecast.stlm. It takes a ts argument, 
	# applies an STL decomposition, models the seasonally adjusted data, 
	# reseasonalizes, and returns the forecasts.
	fc<-stlf(myTs, 
             h=n.ahead, 
             s.window=2, 
             method='ets',
             ic='bic',
             opt.crit='mae')

	# As we had performed logarithmic operation to get the original forecast data we have to perform
	# exponentail operation.
	pred <- exp(fc$mean)
	

	# Post Processing of data making sure that negative values are set to 0.
	# Subtracting the value of constant e added while data preprocessing.
	# Rounding up the value to the closesd whole number.
  	for(i in c(1:n.ahead)){
  		pred[i]=pred[i]-1
  		pred[i]=round(pred[i]/1)*1
  		if(pred[i]<0)
  			pred[i]=0
  	}
	return(pred)
}

get_final <- function(result, key_id, nrow, ncol){

	output<-matrix(data=0, nrow=nrow, ncol=ncol)

	# Inserting key product ids in the first column of output matrix
	for(i in c(1:100)){
		output[,1][i+1]<-key_id[,1][i]
	}

	# Inserting predicted data into the output matrix
	tresult<-t(result)
	for(i in c(1:101)){
		for(j in c(1:28))
		output[,j+1][i]<-tresult[,j][i]
	}
	return(output)
}