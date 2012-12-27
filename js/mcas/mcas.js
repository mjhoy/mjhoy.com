(function() {
  var curCol, curProf, curSubj, h, path, projection, render_mcas, setup_graph, svg, w, x, _parse_json, _update_info;
  curSubj = "ELA";
  curProf = "P+/A %";
  curCol = 100;
  projection = d3.geo.albers().origin([-71.5, 42.15]).scale(17000);
  path = d3.geo.path().projection(projection);
  w = 1000;
  h = 600;
  x = d3.scale.linear().domain([100, 200]).range([0, w]);
  svg = d3.select("#chart").append("svg:svg").attr("width", w).attr("height", h);
  d3.json("../js/mcas/mass-geo.json", function(json) {
    return svg.selectAll("path").data(json.features).enter().append("svg:path").attr("d", path);
  });
  _parse_json = function(json, geom) {
    var data, datum, header, headers, index, k, key, key_type, parsed, row, v, _base, _base2, _base3, _base4, _i, _len, _len2, _ref;
    headers = json[0];
    data = json.slice(1, -6);
    parsed = {};
    for (_i = 0, _len = data.length; _i < _len; _i++) {
      datum = data[_i];
      key = datum[1];
      key_type = datum[2];
      parsed[key] || (parsed[key] = {});
      (_base = parsed[key])[key_type] || (_base[key_type] = {});
      (_base2 = parsed[key])["name"] || (_base2["name"] = datum[0]);
      (_base3 = parsed[key])["code"] || (_base3["code"] = key);
      (_base4 = parsed[key])["geom"] || (_base4["geom"] = geom[key]);
      _ref = headers.slice(3);
      for (row = 0, _len2 = _ref.length; row < _len2; row++) {
        header = _ref[row];
        index = row + 3;
        parsed[key][key_type][header] = datum[index];
      }
    }
    return _.compact((function() {
      var _results;
      _results = [];
      for (k in parsed) {
        v = parsed[k];
        _results.push(v["MTH"] && v["SCI"] && v["ELA"] ? v : void 0);
      }
      return _results;
    })());
  };
  d3.json("../js/mcas/ma_district_geometry.json", function(geom) {
    return d3.json("../js/mcas/mcas_bydist_gr10_all.json", function(json) {
      var data;
      data = _parse_json(json, geom);
      setup_graph(data);
      return render_mcas(curSubj, curProf);
    });
  });
  setup_graph = function(data) {
    return svg.selectAll("circle.point").data(data).enter().append("svg:circle").attr('class', 'point');
  };
  render_mcas = window.render_mcas = function(subj, perf, hue) {
    if (hue == null) {
      hue = 100;
    }
    return svg.selectAll("circle.point").transition().ease("cubic-in-out").attr('class', 'point').attr('cx', function(d) {
      var pos;
      pos = d["geom"];
      return projection([pos["lng"], pos["lat"]])[0];
    }).attr('cy', function(d) {
      var pos;
      pos = d["geom"];
      return projection([pos["lng"], pos["lat"]])[1];
    }).attr('r', function(d) {
      var p, v;
      v = d[subj];
      if (v) {
        return p = +v[perf] / 5;
      } else {
        throw "Error: value for " + subj + " expected.";
      }
    }).attr('fill', function(d) {
      var p, v;
      v = d[subj];
      p = v[perf];
      return "hsl(" + hue + ", " + p + "%, 40%)";
    }).attr('fill-opacity', function(d) {
      var p, v;
      v = d[subj];
      p = +v[perf];
      return p / 100;
    });
  };
  _update_info = function(d) {
    var $n, key, out, p, v, _i, _len, _ref;
    $n = $('#info');
    out = '<p>' + d.name + '</p>';
    out += '<p>' + d.code + '</p>';
    _ref = ['ELA', 'MTH', 'SCI'];
    for (_i = 0, _len = _ref.length; _i < _len; _i++) {
      key = _ref[_i];
      v = d[key];
      p = v["P+/A %"];
      p = p;
      out += '<p><strong>' + key + ' prof.</strong>: ' + p + '%</p>';
    }
    return $n.html(out);
  };
  jQuery(function() {
    var handleSubj, _render;
    handleSubj = function(str) {
      return function(e) {
        ($('header .subjects a')).removeClass('active');
        ($(e.currentTarget)).addClass('active');
        e.preventDefault();
        curSubj = str;
        return render_mcas(str, curProf, 100);
      };
    };
    _render = function() {
      return render_mcas(curSubj, curProf, curCol);
    };
    ($('.proficiencies a')).click(function(e) {
      var $ln;
      $ln = $(e.target);
      $ln.siblings().removeClass('active');
      $ln.addClass('active');
      curProf = $ln.data('key');
      curCol = $ln.data('color');
      e.preventDefault();
      return _render();
    });
    ($('.subjects a')).click(function(e) {
      var $ln;
      $ln = $(e.target);
      $ln.siblings().removeClass('active');
      $ln.addClass('active');
      curSubj = $ln.data('key');
      e.preventDefault();
      return _render();
    });
    ($('.subjects a:first')).addClass('active');
    ($('.proficiencies a:first')).click();
    return ($('#chart')).click(function(e) {
      if (e.target.nodeName === "circle") {
        _update_info(e.target.__data__);
        d3.select('#chart').selectAll('.point').classed('active', false);
        return d3.select(e.target).classed('active', true);
      }
    });
  });
}).call(this);
