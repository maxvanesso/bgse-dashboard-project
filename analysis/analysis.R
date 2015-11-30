library(RMySQL)

#Connection to SQL
db = dbConnect(MySQL(), user='root', password='root' , dbname='ecommerce', host='localhost')

##### GENERATING THE CATEGORIES MAP

library(igraph)

# Import data

result = dbSendQuery(db, "SELECT C1.CategoryName as Cat1, 
       		                C2.CategoryName as Cat2,
       		                Count(DISTINCT O1.OrderID) as Weight
			                    FROM products P1
       		                JOIN products P2
         	                ON P1.ProductID != P2.ProductID 
        	                JOIN categories C1 on P1.CategoryID=C1.CategoryID 
        	                JOIN categories C2 on P2.CategoryID=C2.CategoryID
       		                LEFT JOIN order_details O1
                          INNER JOIN order_details O2
                          ON O1.OrderID = O2.OrderID
         	                ON O1.ProductID = P1.ProductId
                          AND O2.ProductID = P2.ProductID 
  			                  WHERE P1.CategoryID > P2.CategoryID          
			                    GROUP  BY P1.CategoryID, P2.CategoryID
			                    ORDER BY Weight DESC")
relations = fetch(result, n=-1)
colnames(relations) = c("from","to","weight")

result = dbSendQuery(db, "SELECT C1.CategoryName, SUM(O1.UnitPrice*O1.Quantity) as Revenue
                          FROM products P1
                          JOIN categories C1
                          ON P1.CategoryID= C1.CategoryID
                          LEFT JOIN ecommerce.order_details O1
                          ON O1.ProductID = P1.ProductId
                          GROUP BY CategoryName
                          ORDER BY Revenue DESC")
totalrevenue = fetch(result, n=-1)

# Load (DIRECTED) graph from data frame 
g <- graph.data.frame(relations, directed=FALSE)

# Define size of nodes
node.size<-setNames(totalrevenue$Revenue,totalrevenue$CategoryName)
names <-as.vector(totalrevenue$CategoryName)

# Plot and save graph
png("web/categories_network.png")
plot(g, vertex.label = names,
     vertex.shape="circle",
     vertex.color="orange",
     vertex.label.dist=1.2,
     vertex.size=node.size/10000,
     edge.width=E(g)$weight/20,
     vertex.label.cex=1.1,
     vertex.label.family="Helvetica"
)
dev.off()

#### IMPLEMENTATION OF APRIORI ALGORITHM ####

library(arules)

#Run query of Interest
result = dbSendQuery(db, "select distinct(a.OrderID), b.ProductName , b.ProductID from order_details a join products b on a.ProductID = b.ProductID order by a.OrderID")
data = fetch(result, n=-1)

#Prepare data for arule fuction
b<-split(data$ProductName, data$OrderID)
c<-as(b, "transactions")

#Finding rules
rules<-apriori(c, parameter=list(supp=0.002, conf=0.8))
inspect(rules)

# Turning output into required form
d<-as(rules, "data.frame")
out<-as.data.frame(d[1:20,1])
colnames(out)<-"Rules"

#Exporting SQL table
dbSendQuery(db,"drop table if exists apriori")
dbWriteTable(conn = db,name="apriori", value=out, row.names=FALSE)

#### IMPLEMENTATION OF LASSO REGRESSION ####

#Extract data from data set
result = dbSendQuery(db, "select * from ProductsVsCustomers_Pivot")
dataPCsC = fetch(result, n=-1)
result1 = dbSendQuery(db, "SELECT b.CustomerID Customer, sum(a.Quantity*a.UnitPrice) Amount, count(b.OrderID) N_Orders from order_details a left join orders b on a.OrderID=b.OrderID group by CustomerID order by Amount desc limit 20;")
dataPVsC = fetch(result1, n=-1)

#View(dataPVsC)
dataPVsC[,1]
y<-as.matrix(dataPCsC[,3])
x <- dataPCsC[,c(dataPVsC[,1])] # Using best 20 customers
row.names(x)<-dataPCsC[,2]
x[is.na(x)] <- 0.00000000001
z <- as.matrix(x)

## Lasso Coef using Package
library(lars)

set.seed(1)
lasso<-lars(log(z),log(y), type = "lasso",trace=TRUE, use.Gram = TRUE)
cv.lasso<-cv.lars(z,y, type="lasso")
limit<-min(cv.lasso$cv)+cv.lasso$cv.error[which.min(cv.lasso$cv)]
s.cv<-cv.lasso$index[min(which(cv.lasso$cv<limit))]
lasso.coef<-as.data.frame(round(coef(lasso, s = s.cv, mode="fraction")*100,digits = 4))
colnames(lasso.coef)<-c("Percentage")
vvv<-cbind(Customer=rownames(lasso.coef),lasso.coef)
rownames(vvv)<-NULL
TableFinal<-vvv[order(-vvv$Percentage),]

#Exporting SQL table
dbSendQuery(db,"drop table if exists top_customers")
dbWriteTable(conn = db,name="top_customers", value=TableFinal, row.names=FALSE)


