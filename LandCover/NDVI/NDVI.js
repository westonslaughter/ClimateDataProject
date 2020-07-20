// NDVI Extraction Script Update
// Authors: DP, WS
// Sierra Streams Institute
// New Script 7/20/2020

// Load in SSI's Deer Creek monitoring
// site catchments, sites, and watershed outline
var roi = DC_watershed
Map.centerObject(roi, 11);

// Add DC Site Catchments to the map
Map.addLayer(catchments);


// Retrieve LANDSAT imagery, (Tier 1, LANDSAT 7)
// We need just Landsat for our regioin of interest (roi)
// LandSat 7 Import


// NDVI Functions

// Build a map f(x), to add NDVI to every image in my collection
// NDVI, "standard" band calc

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

// Make collection of LANDSAT with our parameters

var collection1 = landsat7Collection
    // Filter to DC watershed
    .filterBounds(roi)
    // Filter to 2001-2020 period
    .filterDate('2001-01-01', '2020-02-29')
    // Select only relevant bands
        // B3: Red, B4: Infrared, BQA: Image Quality Metric
    .select(['B3', 'B4', 'BQA'])
    // Apply quality function
    .map(addFmask)
    // Calculate and add NDVI column
    .map(addNDVI).select('NDVI');

// Now, we need to get the mean NDVI during the growing
// season for each year.

// First, we will isolate the growing season in our IC
var icGrowing = collection1
    .filter(ee.Filter.calendarRange(4,10,'month'));

print(icGrowing,'icGrowing')


// Single function
var meanGreen = function(x) {
  var y = ee.ImageCollection(x).select('NDVI');
  return y.mean();
};

var icMeans = icGrowing.map(meanGreen)
print(icMeans)

// Iteration

// settings for the years to filter on
var interval = 1;
var increment = 'year';
var start = '2000-01-01';
// make a list of start years
var startDate = ee.Date(start);
var secondDate = startDate.advance(interval, increment).millis();
var increase = secondDate.subtract(startDate.millis());
var list = ee.List.sequence(startDate.millis(), ee.Date('2020-01-01').millis(), increase);
print(list)


// make a composite (mean image) of the images of March and clip to the geometry
var composites = ee.ImageCollection.fromImages(list.map(function(startYear){
  var filtCol = icGrowing.filterDate(ee.Date(startYear), ee.Date(startYear).advance(interval, increment));
  var meanImage = filtCol.mean().clip(catchments);
  // add the mean to every image
  var meanVal = meanImage.reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: catchments,
    scale: 500
  });
  return ee.Image(meanImage.setMulti(meanVal)).set('system:time_start', ee.Date(startYear).millis());
}));

print(composites)

// var collectionMax = collection1.reduce({
//   reducer: ee.Reducer.max(),
//   geometry: catchments,
//   scale: 30,
//   maxPixels: 1e13});

// Export.table.toDrive({
// collection: collectionMax,
// description: 'catchment results NDVI'
// });
     
