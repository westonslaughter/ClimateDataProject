// NDVI Extraction Script Update
// Authors: DP, WS
// Sierra Streams Institute
// New Script 7/20/2020

// Load in SSI's Deer Creek monitoring
// site catchments, sites, and watershed outline
var roi = DC_watershed
Map.centerObject(roi, 11);

// Add DC Site Catchments to the map
//Map.addLayer(catchments, {color: '000000'}, 'catchments');


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
 ///////////////////////////////////buffer NDVI to riparian zone///////////////////////
//import NHD flowline data, clipped to DC watershed
//import NHD waterbodies (scotts flat and LWW)

//merge waterbodies and flowline to 1 water layer
var water = NHD.merge(NHD_waterbodies);
//Map.addLayer(water, {color: 'e6ffe6'}, 'water');

//create buffer function for water
var bufferBy = function (size){
  return function(feature){
    return feature.buffer(size);
  };
};

//apply 30m buffer to water layer
var water_buffer = ee.FeatureCollection(water).map(bufferBy(30));
//Map.addLayer(water_buffer, {color: 'Fa6fe0'}, 'water buffer');


//compute the difference - spatial overlay
var wb_geo = water_buffer.geometry();
var water_geo = water.geometry();

var riparianZone = ee.Geometry(wb_geo).difference(ee.Geometry(water_geo)).dissolve(); //dissolve gets rid of overlaps
Map.addLayer(riparianZone, {color: '002b80'}, 'riparian zone');

// Now, we need to get the mean NDVI during the growing
// season for each year.

// First, we will isolate the growing season in our IC
var icGrowing = collection1
    .filter(ee.Filter.calendarRange(4,10,'month'));
    //FILTER TO RIPARIAN ZONE--NOT WORKING FOR ME --CAN I CLIP LATER ON?
//print('icGrowing', icGrowing);
Map.addLayer(icGrowing, {}, 'icGrowing');

//////////////get max of the mean in each catchment: 2001 test/////////////////////
//var CatMap = catchments.map( function(y){

  //var ic2001 = icGrowing.filterDate('2001-01-01', '2001-12-31').select('NDVI');
  //var image2001 = ic2001.reduceToImage(['NDVI'], ee.Reducer.mean());

    //var meanCat = image2001.reduceRegion({
   //reducer: ee.Reducer.mean(),
   //geometry: ee.FeatureCollection(y),
     //scale: 30,
     //maxPixels: 1e13
 //});

//return y.set('mean NDVI 2001', meanCat);
//});

//print('function test', CatMap);
//Map.addLayer(CatMap, {color: '0aFFa0'}, 'function test');

/////////////////////////////////iteration///////////////////////////
//(copied from stack exchange)
/* Creates a collection of mosaics with a given temporal interval.
 *
 * collection - the collection from which to make composites.
 * start - the date of the first composite (either a string or an ee.Date)
 * count - the number of composites to make
 * interval - The time between composites, in units of "units".
 * units - The units of step (day, week, month, year; see ee ee.Date.advance)
 */
var temporalCollection = function(collection, start, count, interval, units) {
  // Create a sequence of numbers, one for each time interval.
  var sequence = ee.List.sequence(0, ee.Number(count).subtract(1));

  var originalStartDate = ee.Date(start);

  return ee.ImageCollection(sequence.map(function(i) {
    // Get the start date of the current sequence.
    var startDate = originalStartDate.advance(ee.Number(interval).multiply(i), units);

    // Get the end date of the current sequence.
    var endDate = originalStartDate.advance(
      ee.Number(interval).multiply(ee.Number(i).add(1)), units);

    return collection.filterDate(startDate, endDate)
        .reduce(ee.Reducer.mean().combine({
          reducer2: ee.Reducer.minMax(),
          sharedInputs: true
        }));
  }));
};

var YearlyNDVI = temporalCollection(collection1, ee.Date('2001-01-01'), 21, 1, 'year');

var check = ee.Image(YearlyNDVI.first()).clipToCollection(roi);
print('check', check);
//Map.addLayer(check, {bands: 'NDVI_mean', min: -1, max: 1}, 'check');
//need to make a color pallet for this layer

////////////////////now map mean to catchment layer: start with just 1 year////////////////
var ndvi_2001 = YearlyNDVI.first().select('NDVI_mean');
print('ndvi_2001', ndvi_2001);

var catMap_mean = catchments.map(function(x){
  var ndvi_reduce = ee.Image(ndvi_2001).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ee.FeatureCollection(x),
    scale: 30,
    maxPixels: 1e13
  });

  return x.set(ndvi_reduce);
});

print('catMap_mean', catMap_mean);

////////////////////////////////////now iterate this into catchments/////////////////////////////
//stack YearlyNDVI so that all separate images become separate bands in same image
var stack = function(collection){
  var first = ee.Image(collection.first());
  var appendBands = function(image, previous){
    return ee.Image(previous).addBands(image);
  };
  return ee.Image(collection.iterate(appendBands, first));
};

var yNDVI_stack = stack(YearlyNDVI);
Map.addLayer(yNDVI_stack);
print('yNDVIm_stack', yNDVI_stack);

//success!!
//now reduce these bands into each catchment
var catMap = catchments.map(function(x){
  var ndvi_reduce = ee.Image(yNDVI_stack).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ee.FeatureCollection(x),
    scale: 30,
    maxPixels: 1e13
  });

  return x.set(ndvi_reduce);
});

print('ndvis in catchment', catMap);
Map.addLayer(catMap, {color: 'a14fde'}, 'catMap');


///need to make sure this actually does correspond to the right dates--go back to the
//temporal collection bit



//Export to a table

//Export.table.toDrive({
//collection: YearlyNDVI,
//description: 'YearlyNDVI_test'
//});
