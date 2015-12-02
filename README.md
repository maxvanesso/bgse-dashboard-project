# BGSE Dashboard Project: Product Recommendation and Customer Analysis

### Overview

This project implements a product recommendation algorithm and carries out customer analysis using LASSO regression. 

The objectives of the project are:

- Develop a set of product recommendation rules, based on the Apriori algorithm
- Rank customers on the basis of their marginal contribution to revenues.

The ultimate goal is to provide information that can be used by a marketing department to develop more targeted campaigns.

### Structure

The core of the analysis is contained in these three files:

- `Customers_by_product.sql`
- `data_and_analysis.php`
- `analysis.R`

Note that some of the key `SQL` queries, to generate the data for the analysis but also to create a network graph, are contained in both the `R` file and the additional `Customers_by_product.sql` file. The latter is called by the setup script after the database is populated.

### Implementation

To develop the product recommendation system we have used the Apriori algorithm. We provide a link to the relevant Wikipedia article on the introductory page of the web application.

To develop the LASSO regression, we first have identified the top 20 customers in terms of total revenues generated. Then we have carried out a LASSO regression using the `lars` package with the objective to narrow down the number of customers with a significant marginal contribution to revenues. 

The 'Data' tab includes a network graph of the links between product categories. Note that the graph is generated dynamically each time the `./setup.sh run` command is given. The script saves a `.png` file in the `/web` sub-directory, which is then retrieved via `html` link.

### Required packages

The `R` analysis relies on the following packages. 

- `igraph`
- `arules`
- `lars`

## Acknowledgments

This project is based on code by: Guglielmo Bartolozzi, Gaston Besanson, Christian Brownlees, Stefano Costantini, Laura Cozma, Jordi Hello WOrdlasdalk
