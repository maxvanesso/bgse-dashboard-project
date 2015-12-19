<?php ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html>
<head>
	<title>MyApp</title>    
	<link rel="stylesheet" type="text/css" href="style.css" />
</head>
<script>
/**
 * Given an element, or an element ID, blank its style's display
 * property (return it to default)
 */
function show(element) {
    if (typeof(element) != "object")	{
	element = document.getElementById(element);
    }
    
    if (typeof(element) == "object") {
	element.style.display = '';
    }
}

/**
 * Given an element, or an element ID, set its style's display property
 * to 'none'
 */
function hide(element) {
    if (typeof(element) != "object")	{
	element = document.getElementById(element);
    }
    
    if (typeof(element) == "object") {
	element.style.display = 'none';
    }
}

function show_content(optionsId) {
	var ids = new Array('home','data','analysis');
	show(optionsId);
	document.getElementById(optionsId + '_link').className = 'active';

	for (var i = 0; i < ids.length; i++)
	{
	    if (ids[i] == optionsId) continue;
	    hide(ids[i]);
	    document.getElementById(ids[i] + '_link').className = '';
	}
}
</script>
<body>
	<div id="header"><h1>Group 7 - JR Cigar Data (Felix Gutmann, Marco Fayet, Max van Esso)</h1></div>

	<div id="menu">
		<a id="home_link" href="#" class="active" onclick="show_content('home'); return false;">Home</a> &middot;
		<a id="data_link" href="#" onclick="show_content('data'); update_data_charts(); return false;">Data</a> &middot;
		<a id="analysis_link" href="#" onclick="show_content('analysis'); return false;">Analysis</a> 
	</div>

	<div id="main">

		<div id="home">
			<h2>Home</h2>
			<h3>The Challenge</h3>
			
			<p>JR Cigars originally started as a small cigar shop in Manhattan. Today, the company is one of the leading US wholesalers and retailers of cigars and tobacco related products, with brick and mortar stores in North Carolina, New Jersey, D.C. and Michigan. They offer a wide selection of hand-rolled cigars from over 500 brands, ranging from 5 cents to $50.00 apiece.</p>

			<p>From a data science perspective, JR Cigars is interesting primarily because of its online and catalogue sales operations from which originate the bulk of its income stream. We were able to get a hold of 2 years worth of data containing:</p>
			
			<ul style="list-style-type:circle">
  				<li> Sales information : invoice data, date of purchase, volume of sales, cost of products </li>
  				<li> Client information : ID number, state of residence, age </li>
  				<li> Product information : product brand, manufacturer, product category</li>
			</ul>
			
			<p>The original dataset spans about a million rows for a total of 15 million entries. The challenge for us was to leverage this dataset into useful business intelligence, that could for example be leveraged by the company's managers to increase profitability.</p>

			<h3>The Solution</h3>
						
			<p>We decided to adopt a two pronged approach in terms of using and treating the data.</p>
				<ul style="list-style-type:circle">
					<li> First, we sought to provide insights as to what was actually going on in the dataset. What are the most sold brands? Where are customers located? Displaying the right kind of information is at the heart of business intelligence. This is the descriptive part which is covered in the Data Section of our dashboard.</li>
					<li> Second, we decided to produce inferential statistics in an attempt to boost future sales and revenue. The two products we designed are a recommender system, based on the purchase history of clients, and a time series analysis used to predict sales. These results can be found in the Analysis Section of our Dashboard.</li>
				</ul>
							
		</div>	

                <?php include 'data_and_analysis.php' ?>
	
	</div>

	

	<div id="footer">Project team: Felix Gutmann, Marco Fayet, Max van Esso</div>

</body>
</html>
<?php ?>
