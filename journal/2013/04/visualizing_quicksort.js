var width = 960,
    height = 500;

var tree = d3.layout.tree()
  .size([920,450]);

var svg = d3.select("#container").append("svg")
  .attr("width", width)
  .attr("height", height);

var genId = (function () {
  var i = 0;
  return function () { return i++; };
})();

var slice = [].slice;

var speed = 300;

function choosePivot (node) {
  node.pivot = node.arr[0],
  node.pivot_index = 0;
}

function tryMerge(node) {
  var left = node.left ? undefined : [],
      right = node.right ? undefined : [];
  if(node.left && node.left.sorted) left = slice.call(node.left.arr);
  if(node.right && node.right.sorted) right = slice.call(node.right.arr);
  if(left && right) {
    node.arr = left.concat([node.pivot]).concat(right);
    node.sorted = true;
  } else {
    node.children.forEach(function(child) { step(child); });
  }
}

function partition(node) {
  var arr = node.arr,
      pivot = node.pivot,
      i = 1,
      j = 1,
      len = arr.length,
      temp = undefined;
  for(; j < len; j++) {
    if(arr[j] < pivot) {
      if(j > i) {
        temp = arr[j];
        arr[j] = arr[i];
        arr[i] = temp;
      }
      i++;
    }
  }
  if(i > 1) {
    arr[0] = arr[i - 1];
    arr[i - 1] = pivot;
  }
  node.pivot_index = i - 1;
}

function createChildNodes(node) {
  var arr = node.arr, len = arr.length, i = node.pivot_index;
  if(i > 0) {
    var first_half = arr.slice(0, i);
    var left = qsort_node(first_half);
    node.children = [left];
    node.left = left;
  }
  if(i < (len - 1)) {
    var second_half = arr.slice(i+1, len);
    var right = qsort_node(second_half);
    if(!node.children) node.children = [];
    node.children.push(right);
    node.right = right;
  }
}

function step(node) {
  var arr = node.arr,
      len = arr.length;
  if(node.children) {
    tryMerge(node);
    return;
  }
  if(len < 2) { // Base case
    node.sorted = true;
    return;
  }
  partition(node);
  createChildNodes(node);
}

function qsort_node(arr) {
  var node = {
    "arr" : arr,
    "id" : genId()
  };
  choosePivot(node);
  return node;
}

function radius(a) {
  return Math.sqrt(a / 2*Math.PI);
}

var update = function (root) {
  var nodes = tree.nodes(root),
      links = tree.links(nodes);

  var node = svg.selectAll(".node")
    .data(nodes, function (d) { return d.id; });

  node
    .transition()
    .duration(speed - 50)
    .attr("transform", function(d) { return "translate("+d.x + "," + (d.y+25) + ")"; });

  var group = node.enter().append("g")
    .attr("class", "node")
    .attr("transform", function(d) { return "translate("+d.x + "," + (d.y+25) + ")"; });

  node.append("circle")
    .attr("class", function (d) { return d.sorted ? "sorted" : "unsorted"; })
    .attr("r", function(d) { return radius(d.arr.length) + 2.0; });

  node.select("text").remove();
  var text = node.append("text");
  text.append("tspan") // Left half
    .text(function(d) {
      var c = d.arr.slice(0,d.pivot_index);
      return c.length ? c+"," : c;
    })
  text.append("tspan") // Pivot
    .text(function(d) { return d.pivot; })
    .attr("class", "pivot");
  text.append("tspan") // Right half
    .text(function(d) {
      var c = d.arr.slice(d.pivot_index+1);
      return c.length ? "," + c : c;
    });

  text
    .attr("class", function (d) { return d.sorted ? "sorted" : "unsorted"; })
    .attr("dx", function (d) { return (radius(d.arr.length) * 2.0) + 3.0 + "px"; })
    .attr("dy", "0.36em")
    .style("font-size", function (d) {
      if (d.arr.length > 20) return "7px";
      if (d.arr.length > 10) return "10px";
      return "12px";
    });

  var link = svg.selectAll(".link").data(links);

  link.transition()
    .duration(speed - 50)
    .style("stroke-opacity", 1)
    .attr("d", d3.svg.diagonal());

  link.enter().append("path")
    .attr("class", "link")
    .attr("transform", function(d) { return "translate(0," + 25 + ")"; })
    .attr("d", d3.svg.diagonal())
    .style("stroke-opacity", 1e-6);
};

function flush() {
  svg.selectAll(".node").remove();
  svg.selectAll(".link").remove();
}

function random_array() {
  var i, n = 3 + (Math.floor(Math.random() * 25)),
      arr = [];
  for(i = 0; i < n; i++) {
    arr.push(Math.floor(Math.random() * 99));
  }
  return arr;
}

// Initial.
var root_node = window.root_node = qsort_node(random_array());
update(root_node);

var timer;

function intervalFn () {
  step(root_node);
  update(root_node);
  if(root_node.sorted) {
    clearInterval(timer);
    setTimeout(function() {
    }, 4000);
  }
}

timer = setInterval(intervalFn, speed);

d3.select("#regenerate").on("click", function() {
  if(timer)clearInterval(timer);
  flush();
  root_node = qsort_node(random_array());
  timer = setInterval(intervalFn, speed);
});
function getUserValue () {
  var arr = document.getElementById("arrayVal").value;
  arr = arr.split(",").map(function(i) { return parseInt(i, 10); });
  flush();
  root_node = qsort_node(arr);
  timer = setInterval(intervalFn, speed);
  return false;
}
