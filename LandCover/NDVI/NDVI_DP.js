// NDVI Extraction Script
// Authors: DP, WS
// Sierra Streams Institute
// New Script 7/20/2020
//last edited 8/12/20

////////Part 1. Data prep//////////
//Step 1. Load the data: import catchments, DC_watershed, NHD (NHD flowline, aka streams), and NHD waterbodies
var roi = DC_watershed
Map.centerObject(roi, 11);

//'catchments' is a Feature Collection layer created in ArcGIS containing the upstream catchment
//from each site to the next upstream site. The column 'gridcode' indicates which
//site the catchment corresponds to.

//If you want to visualize catchments:
//Map.addLayer(catchments, {color: '000000'}, 'catchments');

// Retrieve LANDSAT imagery, (Tier 1, LANDSAT 7)

// Step 2. NDVI and Landsat7 processing

// 2a. Build a function to add NDVI to every image in the Landsat collection

var addNDVI = function(image) {
  var ndvi = image.normalizedDifference(['B4', 'B3']).rename('NDVI');
  return image.addBands(ndvi);
};

// 2b. Remove clouds with Fmask:
var addFmask = function(image) {
  var datamask = image.select('BQA');
  var cloudMask = datamask.neq(1).and(datamask.neq(2)).and(datamask.neq(3)).and(datamask.neq(4));
  return image.updateMask(cloudMask);
};

//2c. Landsat7 data prep:

var collection1 = landsat7Collection
    // Filter to DC watershed
    .filterBounds(roi)
    // Filter to 2001-2020 period
    .filterDate('2001-01-01', '2020-02-29')
    // Select only relevant bands
        // B3: Red, B4: NIR, BQA: Image Quality Metric
    .select(['B3', 'B4', 'BQA'])
    // Apply cloud masking function
    .map(addFmask)
    // Calculate and add NDVI column
    .map(addNDVI).select('NDVI');


  //Step 3. water layer prep

//merge NHDwaterbodies and NHD (flowline) to one water layer
var water = NHD.merge(NHD_waterbodies);

//optional visualization:
//Map.addLayer(water, {color: 'e6ffe6'}, 'water');

 ///////////Part 2. Get riparian zone///////////////////////

//Step 1. create buffer function for water
var bufferBy = function (size){
  return function(feature){
    return feature.buffer(size);
  };
};

//Step 2. apply 30m buffer to water layer
var water_buffer = ee.FeatureCollection(water).map(bufferBy(30));

//Map.addLayer(water_buffer, {color: 'Fa6fe0'}, 'water buffer');

//Step 3. Get riparian zone

//3a. Convert buffer layer to correct data type (geometry)
var wb_geo = water_buffer.geometry();
var water_geo = water.geometry();

//3b. Use 'difference' spatial overlay (similar to ArcGIS) to remove actual water, leaving only the buffer
var riparianZone = ee.Geometry(wb_geo).difference(ee.Geometry(water_geo)).dissolve(); //dissolve gets rid of holes
Map.addLayer(riparianZone, {color: '002b80'}, 'riparian zone');

//**Since the stream is a line, it doesn't get removed. Is this a problem?

/////// Part 3. Meat and potatoes: Get the min, max and mean NDVI during the growing season for each year,
///////in each catchment, only within the riparian zone///////

// Step 1. isolate the growing season in our Image Collection (IC)//
var icGrowing = collection1
    .filter(ee.Filter.calendarRange(4,10,'month'));
//print('icGrowing', icGrowing);
Map.addLayer(icGrowing, {}, 'icGrowing');

//Step 2. Get yearly min, max, mean NDVI for each year

//(code copied from stack exchange)
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

//test one year to see if this worked:
var check = ee.Image(YearlyNDVI.first()).clipToCollection(roi);
print('check', check);
//optional viz:
//Map.addLayer(check, {bands: 'NDVI_mean', min: -1, max: 1}, 'check');

//Step 3. Summarize YearlyNDVI in catchments

//3a. Stack YearlyNDVI so that all IC images become separate bands in same image--this will allow reducing in next step
  //also clip to riparian zone

var stack = function(collection){
  var first = ee.Image(collection.first());
  var appendBands = function(image, previous){
    return ee.Image(previous).addBands(image);
  };
  return ee.Image(collection.iterate(appendBands, first));
};

var yNDVI_stack = stack(YearlyNDVI).clip(riparianZone);
//Map.addLayer(yNDVI_stack);
print('yNDVIm_stack', yNDVI_stack);

//3b. Reduce yNDVI_stack into catchments
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

//Step 4. Export results to a table

//Export.table.toDrive({
//collection: catMap,
//description: 'draft_ndvi_results'
//});

//All done!//
