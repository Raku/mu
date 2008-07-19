
graph:
	dot -Tpng   graph.dot > graph.png
	dot -Tcmapx graph.dot > graph.map
	perl -MCGI=:standard -e \
  'print start_html,img {src=>shift,usemap=>"#G",border=>0},<STDIN>,end_html' \
  graph.png < graph.map > graph.html
