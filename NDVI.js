var table = ee.FeatureCollection("users/wslaughter/DC_Sites"),
    roi = /* color: #00ffff */ee.FeatureCollection(
        [ee.Feature(
            ee.Geometry.Point([-121.3748811811312, 39.2430636173422]),
            {
              "system:index": "0"
            })]),
    DeerCreekRegion =
    /* color: #bf04c2 */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-121.30554791500677, 39.32509605227831],
          [-121.30554791500677, 39.18579284269031],
          [-120.86334820797552, 39.18579284269031],
          [-120.86334820797552, 39.32509605227831]]], null, false),
    site_one = /* color: #81d612 */ee.Feature(
        ee.Geometry.Point([-120.8865048079969, 39.295700932078745]),
        {
          "system:index": "0"
        }),
    Sites = ee.FeatureCollection("users/wslaughter/DC_Sites"),
    site_two = /* color: #98ff00 */ee.Geometry.Point([-121.00196609379975, 39.26830373600965]),
    site_four = /* color: #55ee22 */ee.Geometry.Point([-121.0320011179284, 39.2603980804492]),
    site_thirteen = /* color: #ffc82d */ee.Geometry.Point([-121.00951347754754, 39.25906893190296]),
    site_twelve = /* color: #00ffff */ee.Geometry.Point([-121.01080093787469, 39.2603980804492]),
    site_eleven = /* color: #bf04c2 */ee.Geometry.Point([-121.01383718144993, 39.26152454947051]),
    site_five = /* color: #ff6050 */ee.Geometry.Point([-121.10898189018967, 39.24712029464026]),
    site_six = /* color: #00ff00 */ee.Geometry.Point([-121.18725947808029, 39.23919701307321]),
    site_eight = /* color: #0000ff */ee.Geometry.Point([-121.24421978156349, 39.230303464708584]),
    site_sixteen = /* color: #e2e200 */ee.Geometry.Point([-121.242331506417, 39.229106718772364]),
    site_nine = /* color: #00d2d2 */ee.Geometry.Point([-121.24688053290626, 39.228175902268134]),
    site_fourteen = /* color: #ff00ff */ee.Geometry.Point([-121.27933278284391, 39.22974077836986]),
    site_ten = /* color: #ff9999 */ee.Geometry.Point([-121.26877560816129, 39.228145099792286]),
    site_fifteen = /* color: #99ff99 */ee.Geometry.Point([-121.17723867549213, 39.201041867586746]),
    site_three = /* color: #d63000 */ee.Geometry.Point([39.26072, -121.02411]),
    site_seven = /* color: #98ff00 */ee.Geometry.Point([39.2333, -121.2228]),
    site_seventeen = /* color: #0b4a8b */ee.Geometry.Point([39.26045, -121.01891]),
    site_eighteen = /* color: #ffc82d */ee.Geometry.Point([39.23554, -121.22048]);



// Add DC Sites to the map
Map.addLayer(Sites)

// Center the map on Deer Creek Sites.
Map.centerObject(Sites, 12);

// Retrieve LANDSAT imagery, (Tier 1, LANDSAT 5)
// We need just Landsat for our regioin of interest (roi)

//LandSat
var landsat5Collection = ee.ImageCollection("LANDSAT/LE07/C01/T1")

// Load Landsat 5 input imagery
var image = ee.Image((landsat5Collection)
    // Filter to get only images under the region of interest.
    .filterBounds(roi)
    // Filter to get only one year of images.
    .filterDate('2001-01-01', '2020-02-29')
    // Select just the optical bands
    .select(['B[1-7]'])
    // Sort by scene cloudiness, ascending.
    .sort('CLOUD_COVER')
    // Get the first (least cloudy) scene.
    .first());

  // print
  print(image, 'DC Area L5 image')

  // Vis
  Map.addLayer(image,{},  'L5 Image');

  // True color
  Map.addLayer(image, {bands: ['B4', 'B3', 'B2'], max: 0.5, gamma: 2}, 'Better L8 Image');

// NDVI Attempt

// Compute the Normalized Difference Vegetation Index (NDVI).
var nir = image.select('B5');
var red = image.select('B4');
var ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI');

// Display the result.
Map.centerObject(image, 9);
var ndviParams = {min: -1, max: 1, palette: ['blue', 'white', 'green']};
Map.addLayer(ndvi, ndviParams, 'NDVI image');
Map.addLayer(Sites)

// Build a map f(x), to add NDVI to every image in my collection
// NDVI 2, "standard" band calc
var addNDVI2 = function(image) {
  var ndvi = image.normalizedDifference(['B5', 'B4']).rename('NDVI2');
  return image.addBands(ndvi);
};
// NDVI, also band calc, other method
var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B4', 'B3']).rename('NDVI');
  return image.addBands(ndvi);
};
// Cloud masking function
var addFmask = function(image) {
  var datamask = image.select('BQA');
  var cloudMask = datamask.neq(1).and(datamask.neq(2)).and(datamask.neq(3)).and(datamask.neq(4));
  return image.updateMask(cloudMask);
};

// Test one of the addNDVI functions on a single image.
var ndvi = addNDVI(image).select('NDVI');

// Confirm that above f(x) worked
var withNDVI = landsat5Collection.map(addNDVI);

/// Map now has NDVI calculated for all pixels.
/// You could use inspector to retrive NDVI pixel data for each Site loc.

// Create charts for NDVI calculated, cloud masked, reduced
// For each time series, for each site

var collection1 = ee.ImageCollection('LANDSAT/LE07/C01/T1')
    .filterBounds(roi)
    .filterDate('2001-01-01', '2020-02-29')
    .select(['B3', 'B4', 'BQA'])
    .map(addFmask)
    .map(addNDVI).select('NDVI');

var series1 = ui.Chart.image.seriesByRegion(
    collection1, roi, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L5_Sites_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series1)


// Site One: extraction attempt, 30m buffer for now
// Export CSV from chart, and there ya go

var geom1 = ee.Geometry.Point(-120.8865048079969,39.295700932078745).buffer(30) ;

var series_site1 = ui.Chart.image.seriesByRegion(
    collection1, geom1, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site1_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site1)

Export.table.toDrive({collection: collection1, selectors: 'date, NDVI'});
// Site 2

var geom2 = ee.Geometry.Point(-121.00196609379975,39.26830373600965).buffer(30) ;

var series_site2 = ui.Chart.image.seriesByRegion(
    collection1, geom2, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site2_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site2)

// Site 3

var geom3 = ee.Geometry.Point(-121.02411,39.26072).buffer(30) ;

var series_site3 = ui.Chart.image.seriesByRegion(
    collection1, geom3, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site3_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site3)

// Site 4             (Github Commit Test)

var geom4 = ee.Geometry.Point(-121.0320011179284,39.2603980804492).buffer(30) ;

var series_site4 = ui.Chart.image.seriesByRegion(
    collection1, geom4, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site4_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site4)

// Site 5 (Git Commit Test)

var geom5 = ee.Geometry.Point(-121.10898189018967,39.24712029464026).buffer(30) ;

var series_site5 = ui.Chart.image.seriesByRegion(
    collection1, geom5, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site5_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site5)

// Site 6

var geom6 = ee.Geometry.Point(-121.18725947808029,39.23919701307321).buffer(30) ;

var series_site6 = ui.Chart.image.seriesByRegion(
    collection1, geom6, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site6_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site6)

// Site 7

var geom7 = ee.Geometry.Point(-121.2228,39.2333).buffer(30) ;

var series_site7 = ui.Chart.image.seriesByRegion(
    collection1, geom7, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site7_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site7)

// Site 8

var geom8 = ee.Geometry.Point(-121.24421978156349,39.230303464708584).buffer(30) ;

var series_site8 = ui.Chart.image.seriesByRegion(
    collection1, geom8, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site8_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site8)

// Site 9

var geom9 = ee.Geometry.Point(-121.24688053290626,39.228175902268134).buffer(30) ;

var series_site9 = ui.Chart.image.seriesByRegion(
    collection1, geom9, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site9_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site9)

//Site 10


var geom10 = ee.Geometry.Point(-121.26877560816129,39.228145099792286).buffer(30) ;

var series_site10 = ui.Chart.image.seriesByRegion(
    collection1, geom10, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site10_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site10)

// Site 11

var geom11 = ee.Geometry.Point(-121.01383718144993,39.26152454947051).buffer(30) ;

var series_site11 = ui.Chart.image.seriesByRegion(
    collection1, geom11, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site11_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site11)

// Site 12

var geom12 = ee.Geometry.Point(-121.01080093787469,39.2603980804492).buffer(30) ;

var series_site12 = ui.Chart.image.seriesByRegion(
    collection1, geom12, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site12_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site12)

// Site 13

var geom13 = ee.Geometry.Point(-121.00951347754754,39.25906893190296).buffer(30) ;

var series_site13 = ui.Chart.image.seriesByRegion(
    collection1, geom13, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site13_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site13)

// Site 14

var geom14 = ee.Geometry.Point(-121.27933278284391,39.22974077836986).buffer(30) ;

var series_site14 = ui.Chart.image.seriesByRegion(
    collection1, geom14, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site14_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site14)

// Site 15


var geom15 = ee.Geometry.Point(-121.17723867549213,39.201041867586746).buffer(30) ;

var series_site15 = ui.Chart.image.seriesByRegion(
    collection1, geom15, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site15_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site15)

// Site 16

var geom16 = ee.Geometry.Point(-121.242331506417,39.229106718772364).buffer(30) ;

var series_site16 = ui.Chart.image.seriesByRegion(
    collection1, geom16, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site16_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site16)

// Site 17

var geom17 = ee.Geometry.Point(-121.01891,39.26045).buffer(30) ;

var series_site17 = ui.Chart.image.seriesByRegion(
    collection1, geom17, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site17_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site17)

// Site 18

var geom18 = ee.Geometry.Point(-121.22048,39.23554).buffer(30) ;

var series_site18 = ui.Chart.image.seriesByRegion(
    collection1, geom18, ee.Reducer.mean(), 'NDVI', 30, 'system:time_start', 'label')
        .setChartType('ScatterChart')
        .setOptions({
          title: 'L7_DC_Site18_2001-01-01_2020-02-29',
          vAxis: {title: 'NDVI'},
          lineWidth: 1,
          pointSize: 4,
});

print(series_site18)
