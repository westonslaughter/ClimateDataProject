// NBR
// Authors: DP
// Sierra Streams Institute
// New Script 8/3/2020
//Questions marked with **********

// Load in SSI's Deer Creek monitoring
// site catchments, sites, and watershed outline
var roi = DC_watershed
Map.centerObject(roi, 11);

// Add DC Site Catchments to the map
//Map.addLayer(catchments, {color: '000000'}, 'catchments');


// Retrieve LANDSAT imagery, (Tier 1, LANDSAT 7)
// We need just Landsat for our region of interest (roi)
// LandSat 7 Import


// NBR Functions
//(B4-B7)/(B4+B7)

// Build a map f(x), to add NDVI to every image in my collection
// NDVI, "standard" band calc

var addNBR = function(image) {
  var nbr = (image.select('B4').subtract(image.select('B7')))
      .divide((image.select('B4').add(image.select('B7'))))
      .rename('NBR');
  return image.addBands(nbr);
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
        // B4: NIR, B7: SWIR, BQA: Image Quality Metric
    .select(['B4', 'B7', 'BQA'])
    // Apply quality function
    .map(addFmask)
    // Calculate and add NBR column
    .map(addNBR).select('NBR');

 //skip riparian zone calc because NBR is for whole catchment

// ***********Do we need to isolate growing season in NBR?
//Feel like it makes more sense to do year-round
//var icGrowing = collection1
  //  .filter(ee.Filter.calendarRange(4,10,'month'));

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
      .reduce(ee.Reducer.mean());
  }));
};

var YearlyNBR = temporalCollection(collection1, ee.Date('2001-01-01'), 21, 1, 'year');

var check = ee.Image(YearlyNBR.first()).clipToCollection(roi);
print('check', check);
var nbr_params = {min: -1, max: 1, palette: ['blue', 'red', 'white']};
Map.addLayer(check, nbr_params, 'check');
//need to make a color pallet for this layer

////////////////////////////////////now iterate this into catchments/////////////////////////////
//stack YearlyNDVI so that all separate images become separate bands in same image
var stack = function(collection){
  var first = ee.Image(collection.first());
  var appendBands = function(image, previous){
    return ee.Image(previous).addBands(image);
  };
  return ee.Image(collection.iterate(appendBands, first));
};

var yNBR_stack = stack(YearlyNBR).clipToCollection(DC_watershed);
//Map.addLayer(yNBR_stack);
print('yNBRm_stack', yNBR_stack);

//now reduce these bands into each catchment
var catMap = catchments.map(function(x){
  var nbr_reduce = ee.Image(yNBR_stack).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: ee.FeatureCollection(x),
    scale: 30,
    maxPixels: 1e13
  });

  return x.set(nbr_reduce);
});

print('nbrs in catchment', catMap);
//Map.addLayer(catMap);


///need to make sure this actually does correspond to the right dates--go back to the
//temporal collection bit


//Export to a table

//Export.table.toDrive({
//collection: catMap,
//description: 'draft_nbr_results'
//});
