###################################################################################################
### Project - Group 7
###################################################################################################

# Author:   Felix Gutmann
#           Max van Esso
#           Marco Fayet
# Course:   Computing Lab
# Due:      11.12.2015
# Type:     Project
# Content:  Analysis cigare project

###################################################################################################
### 0. Praeamble
###################################################################################################

### 0.1 Packages

library("RMySQL")
library("forecast")
library("lars")
library("recommenderlab")
library("glmnet")

### 0.2 Initíalize functions

  ### Call data from existing data base
  query <- function(db,que,n){

      temp <<- dbSendQuery(db,que)
      out  <- fetch(temp,n) 
      dbClearResult(temp)
      return(out)
      
  }

  ### Conditional summary
  inf.by <- function(cond,sumvar,fun){
      con <- unique(cond)
      out <- invisible(sapply(con, function(i){
        pos <- which(cond==i)
        fun(sumvar[pos])
      }))
      x <- data.frame(out)
      return(data.frame(Condition=con,Summary=x$out))

  }

### 0.3 Connect to database / Get summary

data.base   <- dbConnect( MySQL() , user='root' , password='root' ,dbname='cigar' , host='localhost' )
tab.names   <- dbListTables( data.base )
field.names <- list()

for( i in 1:length( tab.names ) ){

  field.names[[i]] <- dbListFields( data.base, tab.names[i] )

}

names(field.names) <- tab.names

###################################################################################################
#### START CODE # START CODE # START CODE # START CODE # START CODE # START CODE # START CODE #####
###################################################################################################

###################################################################################################
### 1. Descriptive 
################################################################################################### 

# Get total sales per day
q   <- "SELECT truncate(sum(i.Sales),2) as Sales from cigar.invoice I inner join cigar.invoice_detail i on I.InvoiceNumber=i.InvoiceNumber group by I.InvoiceDate"
res <- query(data.base,q,-1)
n   <- 1:nrow(res)
o   <- data.frame(cbind(n,res))

# Export results to database
dbSendQuery(data.base,"DROP TABLE IF EXISTS sales")
dbWriteTable(conn = data.base, name="sales", value=o, row.names=FALSE)

###################################################################################################
### 2. Recommandation system
###################################################################################################  

# 2.1. Get data from MySQL database

que4 <- "SELECT p.Brand, SUM(i.Volume), c.ClientID FROM product p INNER JOIN invoice_detail i
         ON p.BrandID=i.BrandID INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
         INNER JOIN client c ON I.ClientID=c.ClientID GROUP BY ClientID, Brand"

dat1  <- query(data.base,que4,-1)

que5 <- "SELECT c.ClientID, SUM(i.Volume) AS TotalVolume FROM invoice_detail i INNER JOIN invoice I
         ON i.InvoiceNumber=I.InvoiceNumber INNER JOIN client c ON I.ClientID=c.ClientID
         GROUP BY ClientID" 

dat2 <- query(data.base,que5,-1)

# 2.2. Data manipulation
merge.clientID    <- merge( dat1 , dat2 , by.dat1 = "ClientID", by.dat2 = "ClientID" )
preRank           <- cbind( merge.clientID , ( merge.clientID[,3] / merge.clientID$TotalVolume ) *10 )
colnames(preRank) <- c("ClientId","Brand","Total Brand Volume","Total Volume","Rank" ) 
Rank.matrix       <- preRank[,-3:-4]
rank.table        <- table( Rank.matrix $ClientId , Rank.matrix $Brand )
ranks             <- as.vector( Rank.matrix [,3] ) 

# 2.3. Multiplying the ones in the transposed table by the ranks
trank.table       <- t( rank.table )
trank.table[as.logical((trank.table))] <- ranks 

# 2.4. Re-transposing it to have the rank matrix
finalrank.table   <- t(trank.table) 
finalrank.matrix  <- as.data.frame.matrix(finalrank.table)
finalrank.matrix2 <- as.matrix(finalrank.matrix)

# 2.5. Final adjustments - Replace zero by NA
finalrank.matrix2[finalrank.matrix2==0] <- NA

# 2.6. Create Affinity matrix
affinity.matrix   <- as(finalrank.matrix2, "realRatingMatrix")

# 2.7. Train recommedation model
Rec.model <- Recommender(affinity.matrix[1:4999],
                         method = "UBCF", 
                         param=list(normalize = "Z-score", method = "Jaccard" , nn = 5, minRating = 0 )
                        )

# 2.8. Estimate items for top clients

que6 <- "select c.ClientID, sum(i.Volume) as TotalVolume, p.Brand from product p inner join invoice_detail i
         on p.BrandID=i.BrandID inner join invoice I on i.InvoiceNumber=I.InvoiceNumber
         inner join client c on I.ClientID=c.ClientID group by ClientID, Brand order by TotalVolume desc limit 5"

top <- query(data.base,que6,-1)

rec <- c()
id  <- top$ClientID

for(i in 1:nrow(top)){
  rec[i] <- unlist(as(predict(Rec.model, affinity.matrix[as.character(top$ClientID[i])], n=1),"list"))  
}

top <- cbind(top,Recommendations=rec)

# 2.9. Exporting SQL table
dbSendQuery(data.base,"DROP TABLE IF EXISTS recommendation")
dbWriteTable(conn = data.base, name="recommendation", value=top, row.names=FALSE)

###################################################################################################
### Prediction of sales - Brand level - Data preparation
###################################################################################################  

# Get total number of products from mysql database
n   <- -1
max <-  as.numeric( query( data.base , "SELECT COUNT(BrandID) FROM product" , n ) )

# Initialize storage list for output
sales.by.brand <- list()

# Get data from mysql database
for(i in 1:max){
  
  # Query for product i 
  que      <-  paste0("SELECT ft.InvoiceDate, SUM(st.Sales) AS Sales, st.BrandID
                      FROM invoice ft 
                      JOIN invoice_detail st
                      ON ft.InvoiceNumber=st.InvoiceNumber
                      WHERE st.BrandID=",i,"
                      GROUP BY ft.InvoiceDate")
  # Get data for product i 
  temp     <-  query(data.base,que,n)
  # Append to output list
  sales.by.brand[[i]] <- temp[c(1,2)]
  # 
  print(paste0("Get Brand Data:",i))
}

#Clean up! 
rm(temp)

#Rename output
names(sales.by.brand) <- sapply( 1:max , function(i){ paste0( "Brand" , i ) } )

#Create raw data set

n      <- -1
max    <- query(data.base,"SELECT DISTINCT(InvoiceDate) FROM invoice",n)


ts.wd                 <- max
colnames(ts.wd)[1]    <- "InvoiceDate"

#Merge sales data
for(i in 1:length(sales.by.brand)){
  
  #Init. temp sales of one brand 
  sales.temp <- sales.by.brand[[i]]
  n.temp     <- nrow(sales.temp)
  #Adjust indentifier for merge: "InvoiceDate"
  sales.temp[1] <- sapply( 1:n.temp, function(j){
    substr( as.matrix( sales.temp[1])[j], 1 , 10 ) 
  }
  )
  #Set column name
  colnames(sales.temp)[2] <- paste0("sa.br",i)
  #Merge data
  ts.wd <- merge( ts.wd, sales.temp , by="InvoiceDate" , all=TRUE )
  #Update
  print(paste0("Merge Brand",i))
}

#Clean up
rm(sales.temp, n.temp)

#First adjustments / Add total sales as row sums
ts.wd[is.na(ts.wd)] <- 0
ts.wd               <- invisible(transform(ts.wd, sum=rowSums(ts.wd[2:ncol(ts.wd)])))


### 1.2 Add weekday dummies

# Names
wd.tot  <- weekdays( as.Date(ts.wd$InvoiceDate) )
ts.wd   <- data.frame( ts.wd , weekdays = wd.tot )
wd.uni  <- unique( wd.tot )
d.names <- c( "mo" , "tu" , "we" , "th" , "fr" , "sa" , "so" )

for( i in 1:7 ){
  
  #Create dummy data frame
  df.temp <- data.frame( weekdays = wd.uni[i] , 1 )
  #Merge dummy
  ts.wd   <- merge( ts.wd, df.temp , by= "weekdays" , all = TRUE )
  #Replace column name
  colnames(ts.wd)[ncol(ts.wd)] <- d.names[i]
}

#Clean up!
rm(df.temp)

### 1.3 Final adjustments

ts.wd               <- ts.wd[ order(ts.wd$InvoiceDate , decreasing = FALSE ),]
ts.wd[1:2]          <- list(NULL)
ts.wd[is.na(ts.wd)] <- 0

#Lagged data frame

ts.wdl1 <- ts.wd

drops <- c("sum","InvoiceDate")
ts.wdl1 <- ts.wdl1[,!(names(ts.wdl1) %in% drops)]
ts.wdl1 <- ts.wdl1[1:(nrow(ts.wdl1)-1),]
ts.resp.brand <- ts.wd$sum
ts.resp.brand <- ts.resp.brand[2:length(ts.resp.brand)]

tsbr.lagged <- as.data.frame(cbind(y=ts.resp.brand,ts.wdl1))

###################################################################################################
### Analysis
################################################################################################### 

# 2.1. Define dataset explicitaly
brand.y 	<- as.matrix(tsbr.lagged$y)
brand.X 	<- as.matrix(cbind(tsbr.lagged[,2:535],so=tsbr.lagged[,537]))

# 2.2. Training data

X <- brand.X[1:457,]
y <- brand.y[1:457]

# 2.3. Prediction data

pred.x <- brand.X[457:nrow(brand.X),]
pred.y <- brand.y[457:length(brand.y)]

# 2.4. Fit models

# Lasso regression
lasso          <- glmnet(X, y, alpha=1)
lasso.fit 	   <- cv.glmnet(X, y, alpha=1)
lasso.coef     <- coef(lasso.fit, s = "lambda.min", exact = TRUE)
lasso.mse 	   <- lasso.fit$cvm[lasso.fit$lambda == lasso.fit$lambda.min]

# Ridge regression
ridge          <- glmnet(X, y, alpha=0)
ridge.fit 	   <- cv.glmnet(X, y, alpha=0)
ridge.coef     <- coef(ridge.fit, s = "lambda.min",exact = TRUE)
ridge.mse 	   <- ridge.fit$cvm[ridge.fit$lambda == ridge.fit$lambda.min]

# Elastic regression
elast          <- glmnet(X, y, alpha=0.5)
elastic.fit    <- cv.glmnet(X, y, alpha=0.5)
elastic.coef   <- coef(elastic.fit, s = "lambda.min",exact = TRUE)
elastic.mse    <- elastic.fit$cvm[elastic.fit$lambda == elastic.fit$lambda.min]

# 2.4. Prediction of 2014

LASpre <- predict(lasso.fit ,  pred.x,  s = "lambda.min")
RIDpre <- predict(ridge.fit ,  pred.x,  s = "lambda.min")
ELApre <- predict(elastic.fit, pred.x,  s = "lambda.min")

###################################################################################################
### Benchmark model
###################################################################################################

benchmark <- ts.wd$sum

y <- benchmark[2:length(benchmark)]
x <- benchmark[1:length(benchmark)-1]

# Training and prediction data 
x.training <- x[1:457]
y.training <- y[1:457]
x.pred     <- x[457:length(x)]
# Fit model 
m01 <- lm(y.training ~ x.training)

# Prediction 
OLSpre <- m01$coefficients[1] + x.pred * m01$coefficients[2]

###################################################################################################
### 5.Export predictions
###################################################################################################

n <- 1:length(LASpre)
predictions <- data.frame(cbind(time=n,sales=pred.y,OLS=as.numeric(OLSpre),lasso=as.numeric(LASpre),ridge=as.numeric(RIDpre),elastic=as.numeric(ELApre)))

dbSendQuery(data.base,"DROP TABLE IF EXISTS predictions")
dbWriteTable(conn = data.base, name="predictions", value=predictions, row.names=FALSE)

###################################################################################################
### 6. Check accurracy
###################################################################################################

tLAS <-ts(LASpre,start=1,end=181,frequency = 1)
tRID <-ts(RIDpre,start=1,end=181,frequency = 1)
tELA <-ts(ELApre,start=1,end=181,frequency = 1)
tOLS <-ts(OLSpre,start=1,end=181,frequency = 1)

LASac <- accuracy(tLAS,pred.y)
RIDac <- accuracy(tRID,pred.y)
ELAac <- accuracy(tELA,pred.y)
OLSac <- accuracy(tOLS,pred.y)

accur <- as.data.frame(cbind(LASac[5],RIDac[5],ELAac[5],OLSac[5]))
colnames(accur) <- c("Lasso","Ridge","ElasticNet","OLS")

###################################################################################################
### 6. Export accurracy
###################################################################################################

dbSendQuery(data.base,"DROP TABLE IF EXISTS accuracy")
dbWriteTable(conn = data.base, name="accuracy", value=accur, row.names=FALSE)

###################################################################################################
###### END CODE # END CODE # END CODE # END CODE # END CODE # END CODE # END CODE # END CODE ######
###################################################################################################