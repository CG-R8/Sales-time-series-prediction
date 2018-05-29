#required libraries for the forcasting of the product
library("forecast")
library("Matrix")
library(TTR)
library(dplyr)
library(tcltk)

#select the input file
####inputfile = tk_choose.files(caption = "Choose the input data file")
inputfile = "product_distribution_training_set.txt"
#predicted output will be redirected to this file
product_predicted_data_output_file = "output-cgajare1-B00668719.txt"
#reading the data from the input file and redirecting it to sale_data object
sale_data = read.delim(inputfile,header=FALSE)
#creating a data matrix object by reading the first row and 2:119 columns of sale_data object
sale_data_matrix=data.matrix(colSums(sale_data[,c(2:119)]))
#converting the sale_data_matrix to time series object
sale_data_matrix_ts = ts(ts(sale_data_matrix[c(1:118),1]), frequency = 1)
#it returns the matrices containing the terms from the fourier series. It is suitable for arima series
data_xreg = fourier(ts(sale_data_matrix_ts, frequency=365.25), K=4, h=NULL)
#It returns the arima model object
auto_arima_data = auto.arima(sale_data_matrix_ts,  xreg=data_xreg, seasonal=FALSE)
#Forcasting the data of 28 days
forcast_arima_data_sum_products = forecast.Arima(auto_arima_data, xreg=data_xreg, h=28)

#mean value of forcast_arima_data_sum_products data
forcast_mean=forcast_arima_data_sum_products$mean
#Replacing the negative values with zero
forcast_mean[forcast_mean<0] = 0
forcated_mean_data = c(0,round(forcast_mean))
#creating a matrix of size row=1 and columns = 119
dailysale_predict_matrix=matrix(forcated_mean_data,nrow=1,ncol=119)
#creating adata matrix
product_matrix=data.matrix(sale_data,rownames.force = NA)
#transposing the matrix data
product_matrix=t(product_matrix)

#creating a matrix of size(100,119) to store the product predictions
product_dailysale_predict_matrix=matrix(0,nrow=100,ncol=119)

#prediction is done for all the 100 products each day sale
#i values will represent the products ID's.
i <- 1
while(i<=1){
  cat("  ")
  cat(i)
  sale_data_matrix_ts = ts(data.frame(product_matrix[c(2:119),i]), frequency=1)
  data_xreg = fourier(ts(sale_data_matrix_ts, frequency=365.25/4), K=4, h=NULL)
  auto_arima_data = auto.arima(sale_data_matrix_ts,  xreg=data_xreg, seasonal=FALSE)
  forcast_arima_data_rest = forecast.Arima(auto_arima_data, xreg=data_xreg,h=28)
  data_xreg_365 = fourier(ts(sale_data_matrix_ts, frequency=365.25),K=4, h=NULL)
  auto_arima_data_365 = auto.arima(sale_data_matrix_ts,  xreg=data_xreg_365, seasonal=FALSE)
  forcast_arima_data_rest_365 = forecast.Arima(auto_arima_data_365, xreg=data_xreg_365, h=28)
  forcast_mean=forcast_arima_data_rest$mean
  
  #plotting the forcasted data of i product on graph

  
  #replacing the negatinve values with mean
  forcast_mean[forcast_mean<0]=forcast_arima_data_rest_365$mean
  #If still, the values is negative then replacing the negatinve values with upper values
  forcast_mean[forcast_mean<0]=forcast_arima_data_rest_365$upper[,1]
  #If still, the values is negative then replacing the negatinve values with zero  
  forcast_mean[forcast_mean<0]=0
  #creating a matrix of the predicted data
  
  
  myts = ts(data.frame(product_matrix[c(2:119),i]),frequency=7)
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
  
  product_dailysale_predict_matrix[i,]=
    matrix(c(product_matrix[1,i],as.numeric(round(mygraph$mean))),1,119)
  
  i <- i+1
  


}
#combining the both the matrices i.e. dailysale_predict_matrix and product_dailysale_predict_matrix
final_matrix = rbind(dailysale_predict_matrix,product_dailysale_predict_matrix)
#picking only the 28 days data from the final matrix
product_predicted_output=final_matrix[,c(1:29)]
#if the output file exist, then deleting that file to store the freash data
if(file.exists(product_predicted_data_output_file)){
  file.remove(product_predicted_data_output_file) 
}
#wrtting the data in the text file.




write.table(product_predicted_output,file=product_predicted_data_output_file,quote = FALSE,sep = "\t",row.names = FALSE,col.names = FALSE)



testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
