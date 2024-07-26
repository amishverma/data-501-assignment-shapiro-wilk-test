

# My first comments

# my function-1
shapiro_wilk_test<-function(data,qqplot=FALSE){
  if(!is.numeric(data)) stop("data must be numeric") # check data must be numeric
  if(any(is.na(data))) stop("Data contains NA VALUES") # check data must not have na values
  if(any(is.infinite(data))) stop("Data contains infinite values") # data doesnt not have inifnite values
  if(length(data)<3) stop("Data must contain atlest 3 values") # atleast 3 values
  
    
}