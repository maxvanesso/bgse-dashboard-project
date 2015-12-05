<?php

function connect_to_db() {    
    $host = "ubuntu";
    $dbuser = "root";
    $dbpass = "root";
    $dbname = "cigar";

    $link = mysql_connect($host,$dbuser,$dbpass);

    if (!$link) {
        die('Could not connect: ' . mysql_error());
    }   
    return $link;  
}

function document_header() {
    $str = <<<MY_MARKER
<link rel='stylesheet' href='files/nv.d3.css' type='text/css'>
<script src='files/d3.v2.js' type='text/javascript' ></script>
<script src='files/nv.d3.js' type='text/javascript' ></script>
<script>
    var mycharts = [];
    function update_data_charts() {
        for (i = 0; i < mycharts.length; i++) {
            mycharts[i]();
        }
    }
</script>
MY_MARKER;
    echo $str;
}

function query_and_print_table($query,$title) {
    // Perform Query
    $result = mysql_query($query);

    // Check result
    // This shows the actual query sent to MySQL, and the error. Useful for debugging.
    if (!$result) {
        $message  = 'Invalid query: ' . mysql_error() . "\n";
        $message .= 'Whole query: ' . $query;
        die($message);
    }

    // Use result
    // Attempting to print $result won't allow access to information in the resource
    // One of the mysql result functions must be used
    // See also mysql_result(), mysql_fetch_array(), mysql_fetch_row(), etc.
    echo "<h2>" . $title . "</h2>";
    echo "<table align='center'>";
    echo "<thead><tr></tr>";
    $row = mysql_fetch_assoc($result);
    foreach ($row as $col => $value) {                
        echo "<th>" . $col . "</th>";
    }
    echo "</tr></thead>";

    // Write rows
    mysql_data_seek($result, 0);
    while ($row = mysql_fetch_assoc($result)) {
        echo "<tr>";
        foreach ($row as $e) {                
            echo "<td>" . $e . "</td>";
        }
        echo "</tr>";
    }
    echo "</table>";

    // Free the resources associated with the result set
    // This is done automatically at the end of the script
    mysql_free_result($result);
}


function query_and_print_graph($query,$title,$ylabel) {
    $id = "graph" . $GLOBALS['graphid'];
    $GLOBALS['graphid'] = $GLOBALS['graphid'] + 1;
    
    echo "<h2>" . $title . "</h2>";
    echo PHP_EOL,'<div id="'. $id . '"><svg style="height:300px"></svg></div>',PHP_EOL;

    // Perform Query
    $result = mysql_query($query);

    // Check result
    // This shows the actual query sent to MySQL, and the error. Useful for debugging.
    if (!$result) {
        $message  = 'Invalid query: ' . mysql_error() . "\n";
        $message .= 'Whole query: ' . $query;
        die($message);
    }

    $str = "<script type='text/javascript'>
        function " . $id . "Chart() {";
    $str = $str . <<<MY_MARKER
    nv.addGraph(function() {
        var chart = nv.models.discreteBarChart()
          .x(function(d) { return d.label })    //Specify the data accessors.
          .y(function(d) { return d.value })
          .staggerLabels(true)    //Too many bars and not enough room? Try staggering labels.
          .tooltips(false)        //Show tooltips
          .showValues(true)       //...instead, show the bar value right on top of each bar.
          .transitionDuration(350);
MY_MARKER;
    $str = $str . PHP_EOL . 'chart.yAxis.axisLabel("' . $ylabel . '").axisLabelDistance(30)';
    $str = $str . PHP_EOL . "d3.select('#" . $id . " svg')
          .datum(" . $id . "Data())
          .call(chart);";
    $str = $str . <<<MY_MARKER
      nv.utils.windowResize(chart.update);

      return chart;
    });
}    
MY_MARKER;
    $str = $str . PHP_EOL . $id . "Chart();" . PHP_EOL;
    $str = $str . PHP_EOL . "mycharts.push(". $id . "Chart)" . PHP_EOL;
    $str = $str . PHP_EOL . "function " . $id . "Data() {
 return  [ 
    {
      key:"; 
    $str = $str . '"' . $title . '", values: [';

    while ($row = mysql_fetch_array($result)) {
        $str = $str . '{ "label":"' . $row[0] . '","value":' . $row[1] . '},' . PHP_EOL;
    }    
    $str = $str . '] } ] }</script>';
    echo $str;

}

function query_and_print_series($query,$title,$label) {
    $id = "graph" . $GLOBALS['graphid'];
    $GLOBALS['graphid'] = $GLOBALS['graphid'] + 1;
    
    echo "<h2>" . $title . "</h2>";
    echo PHP_EOL,'<div align="center" id="'. $id . '"><svg style="height:500px; width:800px"></svg></div>',PHP_EOL;

    // Perform Query
    $result = mysql_query($query);

    // Check result
    // This shows the actual query sent to MySQL, and the error. Useful for debugging.
    if (!$result) {
        $message  = 'Invalid query: ' . mysql_error() . "\n";
        $message .= 'Whole query: ' . $query;
        die($message);
    }

    $str = "<script type='text/javascript'>
        function " . $id . "Chart() {";
    $str = $str . <<<MY_MARKER
    nv.addGraph(function() {
    var chart = nv.models.lineChart()
                .margin({left: 100})  //Adjust chart margins to give the x-axis some breathing room.
                .useInteractiveGuideline(true)  //We want nice looking tooltips and a guideline!
                .transitionDuration(350)  //how fast do you want the lines to transition?
                .showLegend(true)       //Show the legend, allowing users to turn on/off line series.
                .showYAxis(true)        //Show the y-axis
                .showXAxis(true)        //Show the x-axis
    ;

    chart.xAxis     //Chart x-axis settings
      .axisLabel('X')
      .tickFormat(d3.format(',r'));

    chart.yAxis     //Chart y-axis settings
      .axisLabel('Y')
      .tickFormat(d3.format('.02f'));

MY_MARKER;

    $str = $str . PHP_EOL . 'chart.yAxis.axisLabel("x").axisLabelDistance(30)';
    $str = $str . PHP_EOL . "d3.select('#" . $id . " svg')
          .datum(" . $id . "Data())
          .call(chart);";
    $str = $str . <<<MY_MARKER
      nv.utils.windowResize(chart.update);

      return chart;
    });
}    
MY_MARKER;

    $str = $str . PHP_EOL . $id . "Chart();" . PHP_EOL;
    $str = $str . PHP_EOL . "mycharts.push(". $id . "Chart)" . PHP_EOL;
    $str = $str . PHP_EOL . "function " . $id . "Data() { 
    var fx = [];";
  
    while ($row = mysql_fetch_array($result)) {
        $str = $str . "fx.push({x:" . $row[0] . ", y:" . $row[1] ."}); " . PHP_EOL;
    }    

    $str = $str . "
    //Line chart data should be sent as an array of series objects.
    return [
    {
      values: fx,
      key: '" . $label . " ',
      color: '#7777ff',
      area: true      //area - set to true if you want this line to turn into a filled area chart.
    }
  ];
}</script>";

    echo $str;

}


?>
