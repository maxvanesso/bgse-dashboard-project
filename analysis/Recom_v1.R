#########################################################################################
### Project 1 # Project 1 # Project 1 # Project 1 # Project 1 # Project 1 # Project 1 ###
#########################################################################################

#Due:     04.12.15 
#Course:  Computing lab / Data warehousing
#Type:    Project
#Module:  Recommendation sector 

##################      ##################      ##################    ##################
### 0.Praeamble###      ### 0.Praeamble###      ### 0.Praeamble###    ### 0.Praeamble###
##################      ##################      ##################    ##################

### 0.0 Clear Workspace

rm( list = ls() )

### 0.1 Packages

library("RMySQL")
library("forecast")
library("lars")
library("recommenderlab")  

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

data.base   <- dbConnect( MySQL() , user='root' , password='' ,dbname='cigar' , host='localhost' )
tab.names   <- dbListTables( data.base )
field.names <- list()

for( i in 1:length( tab.names ) ){

  field.names[[i]] <- dbListFields( data.base, tab.names[i] )

}

names(field.names) <- tab.names

##################################################################################
#Start code # Start # code # Start code # Start code # Start code # Start code ###
##################################################################################

# Get data from MySQL database

que4 <- "SELECT p.Brand, SUM(i.Volume), c.ClientID FROM product p INNER JOIN invoice_detail i
         ON p.BrandID=i.BrandID INNER JOIN invoice I ON i.InvoiceNumber=I.InvoiceNumber
         INNER JOIN client c ON I.ClientID=c.ClientID GROUP BY ClientID, Brand"

dat1  <- query(data.base,que4,-1)

que5 <- "SELECT c.ClientID, SUM(i.Volume) AS TotalVolume FROM invoice_detail i INNER JOIN invoice I
         ON i.InvoiceNumber=I.InvoiceNumber INNER JOIN client c ON I.ClientID=c.ClientID
         GROUP BY ClientID" 

dat2 <- query(data.base,que5,-1)

# Data manipulation

merge.clientID    <- merge( dat1 , dat2 , by.dat1 = "ClientID", by.dat2 = "ClientID" )
preRank           <- cbind( merge.clientID , ( merge.clientID[,3] / merge.clientID$TotalVolume ) *10 )
colnames(preRank) <- c("ClientId","Brand","Total Brand Volume","Total Volume","Rank" ) 
Rank.matrix       <- preRank[,-3:-4]
rank.table        <- table( Rank.matrix $ClientId , Rank.matrix $Brand )
ranks             <- as.vector( Rank.matrix [,3] ) 
# Multiplying the ones in the transposed table by the ranks
trank.table       <- t( rank.table )
trank.table[as.logical((trank.table))] <- ranks 

# Re-transposing it to have the rank matrix
finalrank.table   <- t(trank.table) 
finalrank.matrix  <- as.data.frame.matrix(finalrank.table)
finalrank.matrix2 <- as.matrix(finalrank.matrix)

# Final adjustments - Replace zero by NA
finalrank.matrix2[finalrank.matrix2==0] <- NA

# Create Affinity matrix
affinity.matrix   <- as(finalrank.matrix2, "realRatingMatrix")

# Train recommedation model
Rec.model <- Recommender(affinity.matrix[1:4999],
                         method = "UBCF", 
                         param=list(normalize = "Z-score", method = "Jaccard" , nn = 5, minRating = 0 )
                        )

# Estimate items for top clients

que6 <- "select c.ClientID, sum(i.Volume) as TotalVolume, p.Brand from product p inner join invoice_detail i
         on p.BrandID=i.BrandID inner join invoice I on i.InvoiceNumber=I.InvoiceNumber
         inner join client c on I.ClientID=c.ClientID group by ClientID, Brand order by TotalVolume desc limit 5"

top <- query(data.base,que6,-1)

rec <- c()
id <- top$ClientID
for(i in 1:nrow(top)){
  rec[i] <- unlist(as(predict(Rec.model, affinity.matrix[as.character(top$ClientID[i])], n=1),"list"))  
}

top <- cbind(top,Recomandations=rec)

#Exporting SQL table
dbSendQuery(data.base,"DROP TABLE IF EXISTS recomandation")
dbWriteTable(conn = data.base, name="recomandation", value=top, row.names=FALSE)






