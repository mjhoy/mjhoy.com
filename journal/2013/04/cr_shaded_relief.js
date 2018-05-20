var width = 960,
    height = 600;

var projection = d3.geo.albers()
  .center([0, 9.7])
  .rotate([84.2,0])
  .parallels([8, 11.5])
  //.scale(10200)
  .scale(12240)
  .translate([width / 2, height / 2]);

var path = d3.geo.path()
  .projection(projection);

var svg = d3.select("#svg-container").append("svg")
  .attr("class", "map")
  .attr("width", width)
  .attr("height", height);

d3.json("costarica_min_topo.json", function(error, data) {
  var costarica = topojson.object(data, data.objects.costarica);

  svg.append("image")
    //.attr("clip-path", "url(#clip)")
    .attr("xlink:href", "hill-relief.jpg")
    .attr("width", width)
    .attr("height", height)
    .attr("class", "bg");

  svg.selectAll(".cr-subunit")
    .data(costarica.geometries)
  .enter().append("path")
    .attr("id", function(d) { return "CRI"; })
    .attr("d", path);

  svg.append("clipPath")
    .attr("id", "clip")
  .append("use")
    .attr("xlink:href", "#CRI");

  svg.append("image")
    .attr("clip-path", "url(#clip)")
    .attr("xlink:href", "hill-relief.jpg")
    .attr("width", width)
    .attr("height", height)
    .attr("class", "fg");
});
