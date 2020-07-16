var roi = DC_watershed
Map.centerObject(roi, 11);

//Get NLCD image from 2001
var DC_2001 = ee.ImageCollection("USGS/NLCD")
.select('landcover')
.filterDate('2001-01-01')
.filter(ee.Filter.bounds(roi));

//Get NLCD image from 2016
var DC_2016 = ee.ImageCollection("USGS/NLCD")
.select('landcover')
.filterDate('2016-01-01')
.filterBounds(roi);

var landcoverVis = {
  min: 0.0,
  max: 95.0,
  palette: [
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '466b9f',
    'd1def8',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    'dec5c5',
    'd99282',
    'eb0000',
    'ab0000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    'b3ac9f',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '68ab5f',
    '1c5f2c',
    'b5c58f',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    'af963c',
    'ccb879',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    'dfdfc2',
    'd1d182',
    'a3cc51',
    '82ba9e',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    'dcd939',
    'ab6c28',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    '000000',
    'b8d9eb',
    '000000',
    '000000',
    '000000',
    '000000',
    '6c9fb8'
  ],
};
Map.addLayer(DC_2001, landcoverVis, 'DC_2001');
Map.addLayer(DC_2016, landcoverVis, 'DC_2016');

var DC_outline = DC_watershed.style
({color: 'ff1a1a', 'fillColor': 'FFFFFF00', 'lineType': 'solid'});

//Map.addLayer(DC_outline);

///////PART 2. FIND % EACH LC WITHIN THE WATERSHED////////

  //Calculate DC watershed area in km2

    var DC_watershed_area = DC_watershed.map(function(feature){
    var num = ee.Number.parse(feature.get('AreaSqKm'));
    return feature.set('AreaSqKm', num);
  });

  print('DC_area', DC_watershed_area.aggregate_stats('AreaSqKm'));
  var area = 231.7;

//Reduce DC_2001 and DC_2016 to an image
var DC_2001_image = DC_2001.reduce(ee.Reducer.median());
print('DC 2001 image', DC_2001_image);

var DC_2016_image = DC_2016.reduce(ee.Reducer.median());

//isolate just one LC: forest=41,42,43 for NLCD. Need forest=1, all else=0


  var forest_mask_2001 = DC_2001_image.eq(41).add(DC_2001_image.eq(42))
  .add(DC_2001_image.eq(43))
  .clipToCollection(DC_watershed);
     print(forest_mask_2001, {color:'000FF00'}, 'forest_mask');

  //calculate the area of each forest pixel
  var forest_area2001 = forest_mask_2001.multiply(ee.Image.pixelArea().divide(1000*1000));
     print('forest_area 2001', forest_area2001);

  //cast result of reduce region as an image
     var forest_sum2001 = forest_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13});

     var forest_cast = forest_sum2001.toImage()
      .toFloat()
      .clipToCollection(DC_watershed);

  print('forest cast', forest_cast);
  Map.addLayer(forest_cast, {color:'000000'}, 'forest cast');

  ////now make it a number
  var forest_num = ee.Number(forest_cast.select('landcover_median')
  .reduceRegion(ee.Reducer.first(), DC_watershed, 10)
  .get('landcover_median'));

  print("forest number", forest_num);

  ////now get the percentage
  var forest_2001percent = forest_num.divide(area).multiply(100);
  print('forest 2001 percent', forest_2001percent);

////////////////////// Make this a function ///////////////////////////////
//mapped over x geometry (catchments, a Feature Collection)//
//(ultimately, will want to run this over totalsheds layer)

  //In catchments, convert Shape_area from a float to a Number.
  //Also convert area from m2 to km2
  var catchments_area = catchments.map(function(feature){
    var num = ee.Number.parse(feature.get('Shape_Area')).divide(1000*1000);
    return feature.set('Shape_Area', num);
  });
  print('catchment areas', catchments_area);

//FOREST FUNCTION (2001)
var catchments_area = catchments.map(function(feature){
    var num = ee.Number.parse(feature.get('Shape_Area')).divide(1000*1000);
    return feature.set('Shape_Area', num);
  });

var forest_function = catchments.map (function(x) {

var forest_mask = DC_2001_image.eq(41).add(DC_2001_image.eq(42))
  .add(DC_2001_image.eq(43))
  .clipToCollection(ee.FeatureCollection(x));

var area = forest_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13});

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw forest area 2001', number);
});
print('catchment forest results', forest_function);

///////////////////////REPEAT THIS PROCESS FOR ALL OTHER LC's///////////////////////

//WATER FUNCTION (2001)
var water_function = forest_function.map( function(x) {

var water_mask = DC_2001_image.eq(11)
     .clipToCollection(ee.FeatureCollection(x));

var area = water_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw water area 2001', number);
});


//urban (21-24)
var urban_function = water_function.map( function(x) {

var urban_mask = DC_2001_image.eq(21).add(DC_2001_image.eq(22))
.add(DC_2001_image.eq(23))
.add(DC_2001_image.eq(24))
     .clipToCollection(ee.FeatureCollection(x));

var area = urban_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw urban area 2001', number);
});

//agriculture (81, 82)
var ag_function = urban_function.map( function(x) {

var ag_mask = DC_2001_image.eq(81).add(DC_2001_image.eq(82))
     .clipToCollection(ee.FeatureCollection(x));

var area = ag_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw ag area 2001', number);
});
 //0% ag//

//grassland (71)
var grassland_function = ag_function.map( function(x) {

var grass_mask = DC_2001_image.eq(71)
     .clipToCollection(ee.FeatureCollection(x));

var area = grass_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw grassland area 2001', number);
});

//scrub (51, 52)
var scrub_function = grassland_function.map( function(x) {

var scrub_mask = DC_2001_image.eq(51).add(DC_2001_image.eq(52))
     .clipToCollection(ee.FeatureCollection(x));

var area = scrub_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw scrub area 2001', number);
});

print('2001 results', scrub_function);

/////////////////REPEAT FOR 2016//////////////////////////

var forest16_function = scrub_function.map (function(x) {

var forest_mask = DC_2016_image.eq(41).add(DC_2016_image.eq(42))
  .add(DC_2016_image.eq(43))
  .clipToCollection(ee.FeatureCollection(x));

var area = forest_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13});

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw forest area 2016', number);
});

///////////////////////REPEAT THIS PROCESS FOR ALL OTHER LC's///////////////////////

//WATER FUNCTION (2001)
var water16_function = forest16_function.map( function(x) {

var water_mask = DC_2016_image.eq(11)
     .clipToCollection(ee.FeatureCollection(x));

var area = water_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw water area 2016', number);
});


//urban (21-24)
var urban16_function = water16_function.map( function(x) {

var urban_mask = DC_2016_image.eq(21).add(DC_2016_image.eq(22))
.add(DC_2016_image.eq(23))
.add(DC_2016_image.eq(24))
     .clipToCollection(ee.FeatureCollection(x));

var area = urban_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw urban area 2016', number);
});

//agriculture (81, 82)
var ag16_function = urban16_function.map( function(x) {

var ag_mask = DC_2016_image.eq(81).add(DC_2016_image.eq(82))
     .clipToCollection(ee.FeatureCollection(x));

var area = ag_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw ag area 2016', number);
});
 //0% ag//

//grassland (71)
var grassland16_function = ag16_function.map( function(x) {

var grass_mask = DC_2016_image.eq(71)
     .clipToCollection(ee.FeatureCollection(x));

var area = grass_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw grassland area 2016', number);
});

//scrub (51, 52)
var scrub16_function = grassland16_function.map( function(x) {

var scrub_mask = DC_2016_image.eq(51).add(DC_2016_image.eq(52))
     .clipToCollection(ee.FeatureCollection(x));

var area = scrub_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13
     });

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('raw scrub area 2016', number);
});

////////////Export final feature collection to drive/////////////////
Export.table.toDrive({
 collection: scrub16_function,
 description: 'catchment results'
});
