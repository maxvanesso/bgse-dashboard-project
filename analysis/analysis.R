###################################################################################################
### Project - Group 7
###################################################################################################

# Author:       Felix Gutmann
#               Max van Esso
#               Marco Fayet
# Course:       Computing Lab / Data Warehousing
# Last update:  21.12.2015
# Type:         Dashboard project
# Content:      Analysis cigare project

###################################################################################################
### 0. Praeamble
###################################################################################################

### 0.1 Load packages
library("RMySQL")
library("forecast")
library("recommenderlab")
library("glmnet")
library("ggmap")
library("ggplot2")

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

### 0.3 Styling options
co <- 1/255 
pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.oragange   <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )
  
### 0.4 Connect to database / Get summary

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
### Descriptive - Prepare total sales
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
### Map
###################################################################################################

# Get and adjust client data
que1 <- "SELECT State, COUNT(*) 
         FROM client
         GROUP BY State"
n1   <- -1
states.freq <- query(data.base,que1,n1)
colnames(states.freq)[1] <- "state"
colnames(states.freq)[2] <- "Frequency of States"

#Get map data
state_latlon <- read.csv("data/state_latlon v2.csv", header = TRUE)
map_matrix1  <- merge(states.freq, state_latlon, by="state")
map 		     <- invisible(get_map(location = "united states", zoom = 3, maptype = "toner-lite", source = 'google')) 

#get a map of USA from google source
map.final    <- ggmap(map) +
  geom_point(aes(x = longitude, y = latitude, size = map_matrix1$`Frequency of States`),
             data = map_matrix1, colour=pers.blue, alpha=0.7) + theme_nothing()

###################################################################################################
### Recommandation system
###################################################################################################  

# Get data from MySQL database

que4 <- "SELECT p.Brand, SUM(i.Volume), c.ClientID FROM product p INNER JOIN invoice_detail i
         ON p.BrandID=i.BrandID INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
         INNER JOIN client c ON I.ClientID=c.ClientID GROUP BY ClientID, Brand"

dat1  <- query(data.base,que4,-1)

que5 <- "SELECT c.ClientID, SUM(i.Volume) AS TotalVolume FROM invoice_detail i INNER JOIN invoice I
         ON i.InvoiceNumber=I.InvoiceNumber INNER JOIN client c ON I.ClientID=c.ClientID
         GROUP BY ClientID" 

dat2 <- query(data.base,que5,-1)

# Data preparation
merge.clientID    <- merge( dat1 , dat2 , by.dat1 = "ClientID", by.dat2 = "ClientID" )
preRank           <- cbind( merge.clientID , ( merge.clientID[,3] / merge.clientID$TotalVolume ) *10 )
colnames(preRank) <- c("ClientId","Brand","Total Brand Volume","Total Volume","Rank" ) 
Rank.matrix       <- preRank[,-3:-4]
rank.table        <- table( Rank.matrix $ClientId , Rank.matrix $Brand )
ranks             <- as.vector( Rank.matrix [,3] ) 
trank.table       <- t( rank.table )
trank.table[as.logical((trank.table))] <- ranks 
finalrank.table   <- t(trank.table) 
finalrank.matrix  <- as.data.frame.matrix(finalrank.table)
finalrank.matrix2 <- as.matrix(finalrank.matrix)
finalrank.matrix2[finalrank.matrix2==0] <- NA

# Create Affinity matrix
affinity.matrix   <- as(finalrank.matrix2, "realRatingMatrix")

# Train recommedation model
Rec.model <- Recommender(affinity.matrix[1:4999],
                         method = "UBCF", 
                         param=list(normalize = "Z-score", method = "Jaccard" , nn = 5, minRating = 0 )
                        )

# Get data - Estimate items for top clients based on total volume
que6 <- "select c.ClientID, sum(i.Volume) as TotalVolume, p.Brand from product p inner join invoice_detail i
         on p.BrandID=i.BrandID inner join invoice I on i.InvoiceNumber=I.InvoiceNumber
         inner join client c on I.ClientID=c.ClientID group by ClientID, Brand order by TotalVolume desc limit 5"

top <- query(data.base,que6,-1)
rec <- c()
id  <- top$ClientID

for(i in 1:nrow(top)){
  rec[i] <- unlist(as(predict(Rec.model, affinity.matrix[as.character(top$ClientID[i])], n=1),"list"))  
}

# Define data frame for output
top <- cbind(top,Recommendations=rec)

# Export results to MySQL database
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
### Prediction of sales - Brand Level
################################################################################################### 

# Define dataset explicitaly
brand.y 	<- as.matrix(tsbr.lagged$y)
brand.X 	<- as.matrix(cbind(tsbr.lagged[,2:535],so=tsbr.lagged[,537]))

# Training data
X <- brand.X[1:457,]
y <- brand.y[1:457]

# Prediction data
pred.x <- brand.X[458:nrow(brand.X),]
pred.y <- brand.y[458:length(brand.y)]

# Fit models

  # Lasso regression
  lasso.fit 	   <- cv.glmnet(X, y, alpha=1)
  # Ridge regression
  ridge.fit 	   <- cv.glmnet(X, y, alpha=0)
  # Elastic regression
  elastic.fit    <- cv.glmnet(X, y, alpha=0.5)

# Prediction of 2014

LASpre <- predict(lasso.fit ,  pred.x,  s = "lambda.min")
RIDpre <- predict(ridge.fit ,  pred.x,  s = "lambda.min")
ELApre <- predict(elastic.fit, pred.x,  s = "lambda.min")

###################################################################################################
### Benchmark model
###################################################################################################

# Get total sales
benchmark <- ts.wd$sum

# Prepare data for fit - lagged data
y <- benchmark[2:length(benchmark)]
x <- benchmark[1:length(benchmark)-1]

# Define training and prediction data 
x.training <- x[1:457]
y.training <- y[1:457]
x.pred     <- x[458:length(x)]

# Fit model 
m01 <- lm(y.training ~ x.training)

# Prediction of 2014
OLSpre <- m01$coefficients[1] + x.pred * m01$coefficients[2]

###################################################################################################
### Export predictions
###################################################################################################

# Define sequence of days and create data frame for export
n <- 1:length(LASpre)
predictions <- data.frame(cbind(time=n,sales=pred.y,OLS=as.numeric(OLSpre),lasso=as.numeric(LASpre),ridge=as.numeric(RIDpre),elastic=as.numeric(ELApre)))

# Export data to MySQL database
dbSendQuery(data.base,"DROP TABLE IF EXISTS predictions")
dbWriteTable(conn = data.base, name="predictions", value=predictions, row.names=FALSE)

###################################################################################################
### Check accurracy
###################################################################################################

# Create time series objects
tLAS <-ts(LASpre,start=1,end=180,frequency = 1)
tRID <-ts(RIDpre,start=1,end=180,frequency = 1)
tELA <-ts(ELApre,start=1,end=180,frequency = 1)
tOLS <-ts(OLSpre,start=1,end=180,frequency = 1)

# Compute accuracy summary
LASac <- accuracy(tLAS,pred.y)
RIDac <- accuracy(tRID,pred.y)
ELAac <- accuracy(tELA,pred.y)
OLSac <- accuracy(tOLS,pred.y)

# Define output dataframe with MAPEs of each model
accur <- as.data.frame(cbind(LASac[5],RIDac[5],ELAac[5],OLSac[5]))
colnames(accur) <- c("Lasso","Ridge","ElasticNet","OLS")

# Export data to MySQL database
dbSendQuery(data.base,"DROP TABLE IF EXISTS accuracy")
dbWriteTable(conn = data.base, name="accuracy", value=accur, row.names=FALSE)

###################################################################################################
###### END CODE # END CODE # END CODE # END CODE # END CODE # END CODE # END CODE # END CODE ######
###################################################################################################