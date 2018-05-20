(function() {
  var circle_width, h, path, perf, perf_keys, projection, render, setup_handlers, setup_keys, subj, subj_keys, update_info, w, year;
  projection = d3.geo.albers().origin([-71.65, 42.19]).scale(19000);
  path = d3.geo.path().projection(projection);
  w = 950;
  h = 580;
  window.x = d3.scale.linear().domain([0, 100]).range([20, w / 4]);
  window.y = d3.scale.linear().domain([0, 100]).range([0, h]);
  subj = "ELA";
  perf = "P+/A %";
  year = "2010";
  render = function(subj, perf, year) {
    return d3.selectAll("circle.point").transition().ease("cubic-in-out").attr('r', function(d) {
      if (d[year] && d[year][subj] && d[year][subj][perf]) {
        return circle_width(+d[year][subj][perf]);
      } else {
        return 0;
      }
    });
  };
  circle_width = function(d) {
    return (Math.sqrt(d) / Math.PI) * 2;
  };
  setup_keys = function(svg) {
    svg.selectAll("circle.key").data(d3.range(0, 100, 20)).enter().append("svg:circle").attr('class', 'key').attr('cx', function(d) {
      return x(100 - d);
    }).attr('cy', function(d) {
      return y(70);
    }).attr('r', circle_width).attr('fill', '#666');
    svg.selectAll("line.key").data([20, 80]).enter().append("svg:line").attr('class', 'key').attr('x1', function(d) {
      return Math.floor(x(d)) + 0.5;
    }).attr('x2', function(d) {
      return Math.floor(x(d)) + 0.5;
    }).attr('y1', y(73)).attr('y2', y(76));
    return svg.selectAll("text.key").data([[20, "100%"], [80, "20%"]]).enter().append("svg:text").attr('class', 'key').attr('x', function(d) {
      return x(d[0] - 5);
    }).attr('y', function(d) {
      return y(79);
    }).text(function(d) {
      return d[1];
    });
  };
  perf_keys = null;
  subj_keys = null;
  update_info = function() {
    var body, data, head;
    if (!perf_keys) {
      perf_keys = {};
      subj_keys = {};
      $('.performance li').each(function(i, el) {
        return perf_keys[$(el).data('key')] = $(el).text();
      });
      $('.subject li').each(function(i, el) {
        return subj_keys[$(el).data('key')] = $(el).text();
      });
    }
    data = $('circle.point.active')[0].__data__;
    head = $('#info h2');
    body = $('#info .body');
    body.html('');
    if (data[year] && data[year][subj] && data[year][subj][perf]) {
      body.text("" + data[year][subj][perf] + "% " + perf_keys[perf] + " in " + subj_keys[subj] + " in " + year);
    } else {
      body.text("No data for this entry.");
    }
    return head.text(data["denorm"]["school"]);
  };
  setup_handlers = function(svg) {
    return $('circle.point').click(function(e) {
      d3.selectAll('circle.point').classed('active', false);
      d3.select(e.currentTarget).classed('active', true);
      return update_info();
    });
  };
  jQuery(function() {
    var svg;
    $("li[data-key='" + subj + "']").addClass('active');
    $("li[data-key='" + perf + "']").addClass('active');
    $("li[data-key='" + year + "']").addClass('active');
    $('nav .subject li').click(function(e) {
      var s;
      if (s = $(e.currentTarget).data('key')) {
        $(e.currentTarget).addClass('active').siblings().removeClass('active');
        subj = s;
        render(subj, perf, year);
        return update_info();
      }
    });
    $('nav .performance li').click(function(e) {
      var s;
      if (s = $(e.currentTarget).data('key')) {
        $(e.currentTarget).addClass('active').siblings().removeClass('active');
        perf = s;
        render(subj, perf, year);
        return update_info();
      }
    });
    $('nav .year li').click(function(e) {
      var s;
      if (s = $(e.currentTarget).data('key')) {
        $(e.currentTarget).addClass('active').siblings().removeClass('active');
        year = s;
        render(subj, perf, year);
        return update_info();
      }
    });
    svg = d3.select('#chart-1').append("svg:svg").attr("width", w).attr("height", h);
    setup_keys(svg);
    return d3.json("/js/mcas/mass-geo.json", function(json) {
      svg.selectAll("path").data(json.features).enter().append("svg:path").attr("d", path).attr("class", "state");
      return d3.json("/js/mcas/g10_all.json", function(data) {
        svg.selectAll("circle.point").data(data).enter().append("svg:circle").attr('class', 'point').attr('cx', function(d) {
          var ll;
          if (d["denorm"] && d["denorm"]["geometry"]) {
            ll = d["denorm"]["geometry"];
            return projection([ll["lng"], ll["lat"]])[0];
          } else {
            return -100;
          }
        }).attr('cy', function(d) {
          var ll;
          if (d["denorm"] && d["denorm"]["geometry"]) {
            ll = d["denorm"]["geometry"];
            return projection([ll["lng"], ll["lat"]])[1];
          } else {
            return -100;
          }
        }).attr('fill-opacity', function(d) {
          return 0.6;
        });
        render(subj, perf, year);
        return setup_handlers();
      });
    });
  });
}).call(this);
