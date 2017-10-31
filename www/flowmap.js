// [ ] : list
// { } : object = hash table = dictionary

var countries = {};
var flowmaps = {};

// Load country boundaries and names
d3.queue()
  .defer(d3.json, 'data/countries-50m.topo.json')
  .defer(d3.tsv, 'data/country-names.tsv')
  .awaitAll(function(err, data) {
    // Extract polygons
    countries = topojson.feature(data[0], data[0].objects.countries);

    // Assign country names to each polygons
    var idToNameMap = {};
    var names = data[1];
    names.forEach(function(name) {
      idToNameMap[name.id] = name.name;
    });
    countries.features.forEach(function(feature) {
      feature.name = idToNameMap[feature.id];
    });
  });

// Define R shiny custom output binding
var flowmapBinding = {
  find: function(scope) {
    return $(scope).find('.flowmap-output');
  },
  getId: function(el) {
    var id = el.getAttribute('id');
    return id;
  },
  showProgress: function(el, complete) {
  },
  onValueChange: function(el, data) {
    console.log(el);
    //console.log(el.getAttribute('id'));
    var map;
    // data get test
    var view_lng = 108;//108.277199; //data[0].from_lng; //37.5728438
    var view_lat = 14;//14.058324; //data[0].from_lat; //126.9746921
    //console.log(view_lng, view_lat);
    var color_paths_fill;
    if(el.getAttribute('id')=="flowmap0") {
      color_paths = 'rgba(245, 249, 118, 0.8)';
      color_arrow = 'rgba(245, 249, 118, 1)';
      //color_paths = 'rgba(8, 29, 88, 0.5)';
      //color_arrow = 'rgba(8, 29, 88, 1)';
    }
    else if(el.getAttribute('id')=="flowmap1") {
      color_paths = 'rgba(34, 94, 168, 0.8)';
      color_arrow = 'rgba(34, 94, 168, 1)';
    }

    d3.select(el).selectAll('div.flowmap-container').remove();

    // first-time rendering
    d3.select(el).selectAll('div.flowmap-container').data([0]).enter()
      .append('div')
      .attr('class', 'flowmap-container')
      .style('height', '100%')
      .each(function() {
        // Initialize leaflet
        var map = L.map(this);
        L.tileLayer('//cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png',
        {
          maxZoom: 18,
          attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy; <a href="https://carto.com/attribution">CARTO</a>'
        }).addTo(map);
        map.setView([view_lat, view_lng], 4);

        // Initialize D3 custom layer
        function projectPoint(x, y) {
          var point = map.latLngToLayerPoint(new L.LatLng(y, x));
          this.stream.point(point.x, point.y);
        }
        var transform = d3.geoTransform({point: projectPoint});
        var path = d3.geoPath().projection(transform);

        var svg = d3.select(map.getPanes().overlayPane).append("svg");
        
        // For arrow
        var defs = svg.append("svg:defs");
        defs.append("svg:marker")
            .attr("id", "arrow_" + el.getAttribute('id'))
            .attr("refX", 2)
            .attr("refY", 0)
            .attr("viewBox", "0 -5 4 10")            					
            .attr("markerWidth", 2.5)
            .attr("markerHeight", 2.5)
            .attr("orient", "auto")
            .append("path")
					    .attr("d", "M0,-5L4,0L0,5")
					    .style("fill", color_arrow);

        var g = svg.append("g").attr("class", "leaflet-zoom-hide");
        var gCountries = g.append('g').attr('class', 'countries');
        var selCountries = gCountries.selectAll("path").data(countries.features).enter()
          .append("path")
          .attr('stroke', 'none')
          .attr('fill', function(feature) {
            var colors = {};
            if(el.getAttribute('id')=="flowmap0"){
              data.forEach(myfunction = function (item){
                colors[item.from_country] = 'rgba(29, 145, 192, 0.5)';
                colors[item.to_country] = 'rgba(37, 37, 37, 0.5)';
              });
            }
            else if(el.getAttribute('id')=="flowmap1"){
              data.forEach(myfunction = function (item){
                colors[item.from_country] = 'rgba(64, 127, 86, 0.5)';
                colors[item.to_country] = 'rgba(29, 145, 192, 0.5)';
              });
            }
            //console.log(colors);
            return colors[feature.name] || 'none';
          });
        var gRoutes = g.append('g').attr('class', 'routes');

        // Rerender custom layer
        function render() {
              if(el.getAttribute('id')=="flowmap0") {
      color_paths = 'rgba(245, 249, 118, 0.8)';
      color_arrow = 'rgba(245, 249, 118, 1)';
      //color_paths = 'rgba(8, 29, 88, 0.5)';
      //color_arrow = 'rgba(8, 29, 88, 1)';
    }
    else if(el.getAttribute('id')=="flowmap1") {
      color_paths = 'rgba(34, 94, 168, 0.8)';
      color_arrow = 'rgba(34, 94, 168, 1)';
    }
          // Set bounding box
          var bounds = path.bounds(countries);
          var topLeft = bounds[0];
          var bottomRight = bounds[1];
          svg
            .attr("width", bottomRight[0] - topLeft[0])
            .attr("height", bottomRight[1] - topLeft[1])
            .style("left", topLeft[0] + "px")
            .style("top", topLeft[1] + "px");
          g
            .attr("transform", "translate(" + -topLeft[0] + "," + -topLeft[1] + ")");

          // Update countries
          selCountries
            .attr("d", path);

          // Update routes
          function mytoArc(fromlng, fromlat, tolng, tolat) {
            var p0 = [fromlat, fromlng];
            var p2 = [tolat, tolng];
            var p1 = [(p0[0] + p2[0]) * 0.5, (p0[1] + p2[1]) * 0.5];
            var dist = Math.sqrt(Math.pow(p0[0] - p2[0], 2) + Math.pow(p0[1] - p2[1], 2));
            p1[1] += dist * 0.2;
            //약간 짧은 도착점으로
            p2[0] -= (p2[0]-p0[0])/dist * 1.5;
            p2[1] -= (p2[1]-p0[1])/dist * 1.5;
            return [p0, p1, p2];
          }
        
          var selRoutes = gRoutes.selectAll('path').data(data);
          selRoutes.enter()
            .append('path')
            .merge(selRoutes)
            .attr('stroke-linecap', 'none')
            .attr('stroke', color_paths)
            .attr('stroke-width', function(data) {
              if(el.getAttribute('id')=="flowmap0")return Math.log(data.width)*1.5;
              else if(el.getAttribute('id')=="flowmap1")return Math.log(data.width)*1.5;        
            }) //data.width
            .attr('fill', 'none')
            .attr("marker-end", "url(#arrow_" + el.getAttribute('id') + ")")
            .attr('d', function(data) {
              //console.log(data);
              var route = mytoArc(data.from_lng, data.from_lat, data.to_lng, data.to_lat);
              var pixels = route.map(function(r) {
                return map.latLngToLayerPoint(new L.LatLng(r[0], r[1]));
              });
              var line = d3.line()
                .x(function(d) {return d.x;})
                .y(function(d) {return d.y;})
                .curve(d3.curveBundle);
              return line(pixels);
            });
          selRoutes.exit().remove();
        }
        map.on('moveend', render);
        render();
      });
  },
  onValueError: function(el, err) {
    console.log(['onValueError', el, err]);
  }
};

Shiny.outputBindings.register(flowmapBinding, "ak.flowmapBinding");
