<html>
<head>
  <title>A basic widget based template engine</title>
</head>
<body>
<h1><server:text id="title" /></h1>
<p>Created by <server:text id="firstname" /> <server:text id="familyname" />.</p>
<h2>My TODO list:</h2>
<ul>
<server:repeater id="todo">
  <li><server:input id="item" /></li>
</server:repeater>
</ul>
</body>
</html>