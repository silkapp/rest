<html>
  <head>
    <title>Silk API Documentation version $version$ for $resource; separator="/"$ </title>
    <link rel="stylesheet" type="text/css" href="$static$/bootstrap.min.css">
    <link rel="stylesheet" type="text/css" href="$static$/prettify.css">
    <link rel="stylesheet" type="text/css" href="$static$/docs.css">
    <script type="text/javascript" src="$static$/jquery-nightly.js"></script>
    <script type="text/javascript" src="$static$/bootstrap-modal.js"></script>
    <script type="text/javascript" src="$static$/prettify.js"></script>
    <script type="text/javascript" src="$static$/docs.js"></script>
  </head>
  <body>
    <div class="container-fluid">
      <div class="sidebar">
        <p>Silk API $version$</p>
        <div class="page-header">
          <h1>Navigation</h1>
        </div>
        <h3>Context</h3> $parents; separator="<span class=\"parent-separator\">></span>"$
        <h3>Sub-resources</h3>          
        <div class="span8">
          <ul>
            <li>$subresources; separator="</li><li>"$</li>
          </ul>
        </div>
      </div>
      <div class="content">
        <div class="row pageHeader resource-title"> 
          <div class="span16">
            <h1>$resource; separator="/"$</h1>
          </div>
        </div>
        <div class="row">
          <div class="span8">
            <h2>Description</h2>
            <p>$description$</p>
          </div>
          <div class="span8">
            <h2>Identifiers</h2>
            <p>$identifiers; separator="<br/>"$</p>
          </div>
        </div>
        <div class="row">
          <div class="span16">
            $urls$
          </div>
        </div>
      </div>
    </div>  
  </body>
</html>
