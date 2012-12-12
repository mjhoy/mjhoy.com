(function() {
  Raphael.fn.comma = function (x, y) {
    return this.path("M-12.087-10.269c-1.066-7.127,6.048-13.766,12.784-13.799c8.725-0.043,19.277,8.929,17.654,24.554 C16.293,20.301-4.583,24.836-15.762,23.968c-4.356-0.963-3.009-2.939,0.319-2.646c4.763,0.276,14.935-3.807,13.854-10.487 C-2.218,6.941-10.748-1.342-12.087-10.269z").translate(x, y).attr({x:x, y:y});
  }
  Raphael.fn.elizabethanStrap = function (x, y) {
    return this.path("M-57.5,33.501v-14.57c0,0,11.235-14.573,11.67-16.602c0.435-2.029-0.362-7.825-0.87-8.841 c-0.508-1.016-1.376-0.292-1.232-1.233c0.145-0.941,1.233-1.665,1.74-1.376c0.506,0.289,0.323,0.795,0.488,1.013 c0.165,0.218,3.136,1.96,5.311,1.597c2.175-0.364,6.162-4.207,6.162-5.583c0-1.376-0.146-2.032-0.726-2.103 c-0.579-0.071-1.16,0.36-1.522,0s0.65-2.609,1.449-2.028c0.798,0.581,3.843,3.333,8.627,3.262s7.319-5.073,7.683-5.872 c0.364-0.798,0.073-7.903-0.362-9.062s-1.74-3.116-2.392-3.116s-3.624,6.595-8.046,6.16c-4.422-0.435-7.755-5.726-7.755-8.481 c0-2.756,6.634-14.572,18.721-14.499C-6.469-47.76,0.051-39.209,3.531-35.078c3.376,4.006,9.302,14.431,9.771,27.638 c0.016,0.408,0.071,4.315,0.108,4.73c0.326,3.588,0.486,9.752-0.601,11.417c-1.088,1.665-6.886,5.58-8.843,5.58 c-1.958,0-11.308-1.882-11.308-6.884c0-5.001,3.118-8.773,6.089-8.556c2.972,0.217,1.305,1.957,0.145,1.957 c-1.161,0-4.254,1.254-4.35,5.654c-0.095,4.4,6.451,5.654,8.699,5.8c2.248,0.146,4.496-1.668,5.51-2.392 c1.014-0.724,2.029-1.447,2.463-2.463s1.087-7.829,0.218-9.714c-0.87-1.886-3.626-6.014-8.046-7.465 c-4.421-1.451-11.814-2.684-16.454,0.724c-4.64,3.408-8.048,8.695-8.119,14.424c-0.072,5.729,3.987,12.541,7.757,15.657 c3.77,3.116,10.873,7.248,20.224,6.959s12.613-2.393,14.788-4.566c2.174-2.175,7.249-6.959,9.206-12.396s4.205-11.312,4.205-13.846 s-0.508-4.274-0.726-5.145c-0.218-0.87-0.146-1.05,0.726-1.05c0.871,0,0.98,0,1.523,1.087c0.544,1.087,4.977,3.915,11.718,4.023 l-0.002,37.407H-57.5z").translate(x, y);
  }
}());

/*
 * init 
 *
 * init raphael, dom events.
 */

function init () {
  var parts = [
    // --- Part 1
    {
      init: function () {
        var paper = Raphael('page_1_box', 200, 150);
        var rect  = paper.rect(90, 30, 50, 50);
        $('#page_1_rotate_click').click(function() {
          rect.rotate(60);
        });
      }
    },

    // --- Part 2
    {
      init: function () { 
        var paper = Raphael('page_2_box', 200, 150);
        var rect  = paper.rect(90, 30, 50, 50);
        var lastRect = rect;
        $('#page_2_magical_click').click(function() {
          var newRect = lastRect.clone().rotate(60);
          lastRect = newRect;
        });
      }
    },

    // --- Part 3
    {
      init: function () {
        var paper = Raphael('page_3_box', 400, 100);
        var rect  = paper.rect(5, 20, 50, 50);
        var fn = function (obj) { obj.translate(100, 0); };
        var iterate = function(obj, n, fn) {
          if (!(n > 0)) {
            return;
          }
          newObj = obj.clone();
          fn(newObj);
          iterate(newObj, n - 1, fn);
        };
        iterate(rect, 3, fn);
      }
    },

    // --- Part 4
    {
      init: function () {
        var paper = Raphael('page_4_box', 250, 250);
        var a = paper.rect(120, 100, 20, 25).attr({fill: 'black'});
        var a2 = a.iterate(5, function () {
          this.rotateAround(72, 0, 30);
        });
        var b = a.clone().translate(-38, -18);
        var b2 = b.iterate(11, function () {
          this.rotateAround(30, 38, 48);
        });

        var paper2 = Raphael('page_4_box_2', 400, 100);
        var strap = paper2.elizabethanStrap(50, 50).attr({fill:'black'});

        var paper2 = Raphael('page_4_box_3', 400, 100);
        (function () {
          var r = paper2.elizabethanStrap(50, 50).attr({fill:'black'});
          var r1 = r.iterate(1, function() {
            this.scale(-1, 1);
            this.translate(100, 0);
          });
        }());

        var paper3 = Raphael('page_4_box_4', 400, 200);
        (function () {
          var r = paper3.elizabethanStrap(150, 150).attr({fill:'black'});
          var r1 = r.iterate(1, function() {
            this.scale(-1, 1);
            this.translate(100, 0);
          });
          r2 = r1.iterate(1, function() {
            this.rotateAround(180, 50, -45);
            this.translate(-100, 0);
          });
        }());

        var paper4 = Raphael('page_4_box_5', 700, 200);
        (function () {
          var r = paper4.elizabethanStrap(10, 150).attr({fill:'black'});
          var r1 = r.iterate(1, function() {
            this.scale(-1, 1);
            this.translate(100, 0);
          });
          r2 = r1.iterate(1, function() {
            this.rotateAround(180, 50, -45);
            this.translate(-100, 0);
          });
          r3 = r2.iterate(4, function() {
            this.translate(200, 0);
          });
        }());
      }
    }
  ];
  var i, length;
  for (i = 0, length = parts.length; i < parts.length; i++) {
    parts[i].init();
  }
  
}

$(document).ready(function() {
  $('article').addClass('js');
  init();
});
