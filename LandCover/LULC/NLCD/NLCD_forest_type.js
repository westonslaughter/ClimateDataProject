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

//Add DC_catchments layer
Map.addLayer(catchments, {color: '9c2828', 'fillColor': 'ffffff00'}, 'catchments');

var sites = Map.addLayer(DC_sites, {color:'ABCD6C', 'fillColor': 'ABCD6C'}, 'sites');


///////FIND % OF EACH LANDCOVER CLASS WITHIN THE WATERSHED////////

//Reduce DC_2001 and DC_2016 to an image
var DC_2001_image = DC_2001.reduce(ee.Reducer.median());
print('DC 2001 image', DC_2001_image);

var DC_2016_image = DC_2016.reduce(ee.Reducer.median());

//isolate just one LC: forest=41,42,43. Need forest=1, all else=0
  //In catchments, convert Shape_area from a float to a Number.
  //Also convert from m2 to km2
  //this is to use in the percentage calc --still working on as a fx
  var catchments_area = catchments.map(function(feature){
    var num = ee.Number.parse(feature.get('Shape_Area')).divide(1000*1000);
    return feature.set('Shape_Area', num);
  });
  print('catchment areas km2', catchments_area);

//CONIFEROUS FOREST FUNCTION (42) (2001)

var con_function = catchments_area.map (function(x) {

var con_mask = DC_2001_image.eq(42)
  .clipToCollection(ee.FeatureCollection(x));

var area = con_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13});

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('con forest area 2001 km2', number);
});
print('catchment forest results', con_function);


//Deciduous forest (41) (2001)
var dec_function = con_function.map( function(x) {

var dec_mask = DC_2001_image.eq(41)
     .clipToCollection(ee.FeatureCollection(x));

var area = dec_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

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

return x.set('deciduous area 2001 km2', number);
});

//Mixed Forest (43) (2001)
var mix_function = dec_function.map( function(x) {

var mix_mask = DC_2001_image.eq(43)
     .clipToCollection(ee.FeatureCollection(x));

var area = mix_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

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

return x.set('mixed area 2001 km2', number);
});

/////////////////REPEAT FOR 2016//////////////////////////

var con16_function = mix_function.map (function(x) {

var con_mask = DC_2016_image.eq(42)
  .clipToCollection(ee.FeatureCollection(x));

var area = con_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

var sum = area.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: ee.FeatureCollection(x),
       scale: 30,
       maxPixels: 1e13});

var cast_image = sum.toImage().toFloat().clipToCollection(ee.FeatureCollection(x));

var number = ee.Number(cast_image.select('landcover_median')
 .reduceRegion(ee.Reducer.first(), ee.FeatureCollection(x), 30)
  .get('landcover_median'));

return x.set('coniferous area 2016 km2', number);
});

//deciduous
var dec16_function = con16_function.map( function(x) {

var dec_mask = DC_2016_image.eq(41)
     .clipToCollection(ee.FeatureCollection(x));

var area = dec_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

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

return x.set('deciduous area 2016 km2', number);
});

//mixed
var mix16_function = dec16_function.map( function(x) {

var mix_mask = DC_2016_image.eq(43)
     .clipToCollection(ee.FeatureCollection(x));

var area = mix_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

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

return x.set('mixed area 2016 km2', number);
});

////////////Export final feature collection to drive///////////////
//Export.table.toDrive({
//collection: mix16_function,
//description: 'forest type catchment results'
//});
