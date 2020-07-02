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

Map.addLayer(DC_outline);

///////PART 2. FIND % EACH LC WITHIN THE WATERSHED////////

  //Calculate DC watershed area in km2

    var DC_watershed_area = DC_watershed.map(function(feature){
    var num = ee.Number.parse(feature.get('AreaSqKm'));
    return feature.set('AreaSqKm', num);
  });
  print('DC_Area', DC_watershed_area.aggregate_stats('AreaSqKm'));

  var area = 231.7;

//isolate just one LC: forest=41,42,43 for NLCD. Need forest=1, all else=0
  //Reduce DC_2001 to an image
var DC_2001_image = DC_2001.reduce(ee.Reducer.median());

  var forest_mask_2001 = DC_2001_image.eq(41).add(DC_2001_image.eq(42))
  .add(DC_2001_image.eq(43))
  .clipToCollection(DC_watershed);
     Map.addLayer(forest_mask_2001, {color:'000FF00'}, 'forest_mask');

  //calculate the area of each forest pixel
  var forest_area2001 = forest_mask_2001.multiply(ee.Image.pixelArea().divide(1000*1000));

  //sum the value of the forest pixels in DC watershed
     var forest_sum2001 = forest_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

     print('forest area 2001', forest_sum2001, 'km2');

  //Forest area = 117.9km2. Now we can calculate the percentage
  //of area in DC watershed that is forest:

  var percent_forest2001 = ee.Number(forest_sum2001).divide(area)*100;

  print('percent forest 2001', percent_forest2001);

//REPEAT THIS PROCESS FOR ALL OTHER LC's

//water (original: 11)
var water_mask2001 = DC_2001_image.eq(11)
     .clipToCollection(DC_watershed);
     Map.addLayer(water_mask2001, {color:'000000'}, 'water_mask');

var water_area2001 = water_mask2001.multiply(ee.Image.pixelArea().divide(1000*1000));

var water_sum2001 = water_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });
   print('water area 2001', water_sum2001, 'km2');

   var percent_water2001 = 3.76/(area)*(100);
  print('percent water 2001', percent_water2001);

//urban (21-24)
var urban_mask2001 = DC_2001_image.eq(21).add(DC_2001_image.eq(22))
.add(DC_2001_image.eq(23))
.add(DC_2001_image.eq(24))
     .clipToCollection(DC_watershed);
     Map.addLayer(urban_mask2001, {color:'F00000'}, 'urban_mask');

var urban_area2001 = urban_mask2001.multiply(ee.Image.pixelArea().divide(1000*1000));

var urban_sum2001 = urban_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });
   print('urban area 2001', urban_sum2001, 'km2');

  var percent_urban2001 = 24.64/(area)*100;
  print('percent urban 2001', percent_urban2001);

//agriculture (81, 82)
var ag_mask2001 = DC_2001_image.eq(81).add(DC_2001_image.eq(82))
     .clipToCollection(DC_watershed);
     Map.addLayer(ag_mask2001, {color:'000F00'}, 'ag_mask2001');

var ag_area2001 = ag_mask2001.multiply(ee.Image.pixelArea().divide(1000*1000));

var ag_sum2001 = ag_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

  print('ag area 2000', ag_sum2001, 'km2');

 //0% ag//

//grassland (71)
var grass_mask2001 = DC_2001_image.eq(71)
     .clipToCollection(DC_watershed);
     Map.addLayer(grass_mask2001, {color:'00000F'}, 'grass_mask2001');

var grass_area2001 = grass_mask2001.multiply(ee.Image.pixelArea().divide(1000*1000));

var grass_sum2001 = grass_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

  print('grassland area 2001', grass_sum2001, 'km2');

  var percent_grass2001 = 14.82/(area)*100;
  print('percent grassland 2001', percent_grass2001);

//scrub (51, 52)
var scrub_mask2001 = DC_2001_image.eq(51).add(DC_2001_image.eq(52))
     .clipToCollection(DC_watershed);
     Map.addLayer(scrub_mask2001, {color:'00000F'}, 'scrub_mask2001');

var scrub_area2001 = scrub_mask2001.multiply(ee.Image.pixelArea().divide(1000*1000));

var scrub_sum2001 = scrub_area2001.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

  print('scrub area 2001', scrub_sum2001, 'km2');

  var percent_scrub2001 = 33.16/(area)*100;
  print('percent scrub 2001', percent_scrub2001);


///////////REPEAT THESE MASKS FOR 2016///////////////////

//forest
var DC_2016_image = DC_2016.reduce(ee.Reducer.median());

  var forest_mask_2016 = DC_2016_image.eq(41).add(DC_2016_image.eq(42))
  .add(DC_2016_image.eq(43))
  .clipToCollection(DC_watershed);

  var forest_area2016 = forest_mask_2016.multiply(ee.Image.pixelArea().divide(1000*1000));

  var forest_sum2016 = forest_area2016.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

     print('forest area 2016', forest_sum2016, 'km2');

  var percent_forest2016 = 168.58/(area)*100;
  print('percent forest 2016', percent_forest2016);


  //water//
var water_mask2016 = DC_2016_image.eq(11)
     .clipToCollection(DC_watershed);

var water_area2016 = water_mask2016.multiply(ee.Image.pixelArea().divide(1000*1000));

var water_sum2016 = water_area2016.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });
   print('water area 2016', water_sum2016, 'km2');

   var percent_water2016 = 4.07/(area)*(100);
  print('percent water 2016', percent_water2016);

//urban (21-24)
var urban_mask2016 = DC_2016_image.eq(21).add(DC_2016_image.eq(22))
.add(DC_2016_image.eq(23))
.add(DC_2016_image.eq(24))
     .clipToCollection(DC_watershed);

var urban_area2016 = urban_mask2016.multiply(ee.Image.pixelArea().divide(1000*1000));

var urban_sum2016 = urban_area2016.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });
   print('urban area 2016', urban_sum2016, 'km2');

  var percent_urban2016 = 25.18/(area)*100;
  print('percent urban 2016', percent_urban2016);

//agriculture (81, 82)
var ag_mask2016 = DC_2016_image.eq(81).add(DC_2016_image.eq(82))
     .clipToCollection(DC_watershed);
var ag_area2016 = ag_mask2016.multiply(ee.Image.pixelArea().divide(1000*1000));

var ag_sum2016 = ag_area2016.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

  print('ag area 2016', ag_sum2016, 'km2');

 //0% ag//

//grassland (71)
var grass_mask2016 = DC_2016_image.eq(71)
     .clipToCollection(DC_watershed);

var grass_area2016 = grass_mask2016.multiply(ee.Image.pixelArea().divide(1000*1000));

var grass_sum2016 = grass_area2016.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

  print('grassland area 2016', grass_sum2016, 'km2');

  var percent_grass2016 = 14.22/(area)*100;
  print('percent grassland 2016', percent_grass2016);

//scrub (51, 52)
var scrub_mask2016 = DC_2016_image.eq(51).add(DC_2016_image.eq(52))
     .clipToCollection(DC_watershed);

var scrub_area2016 = scrub_mask2016.multiply(ee.Image.pixelArea().divide(1000*1000));

var scrub_sum2016 = scrub_area2016.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

  print('scrub area 2016', scrub_sum2016, 'km2');

  var percent_scrub2016 = 19.61/(area)*100;
  print('percent scrub 2016', percent_scrub2016);


/////////PART 3. LULC IN SITE CATCHMENTS  ////////////
Map.addLayer(catchments, {color: '9c2828', 'fillColor': 'ffffff00'}, 'catchments');

var sites = Map.addLayer(DC_sites, {color:'ABCD6C', 'fillColor': 'ABCD6C'}, 'sites');

//Create a new variable for each separate watershed

function getCols(Catchments){
  print(Catchments.columns);
}
catchments.limit(0).evaluate(getCols);

var shed1 = catchments.filter(ee.Filter.eq('gridcode', 1));
var shed2 = catchments.filter(ee.Filter.eq('gridcode', 2));
var shed3 = catchments.filter(ee.Filter.eq('gridcode', 3));
var shed17 = catchments.filter(ee.Filter.eq('gridcode', 17));
var shed11 = catchments.filter(ee.Filter.eq('gridcode', 11));
var shed12 = catchments.filter(ee.Filter.eq('gridcode', 12));
var shed13 = catchments.filter(ee.Filter.eq('gridcode', 13));
var shed4 = catchments.filter(ee.Filter.eq('gridcode', 4));
var shed5 = catchments.filter(ee.Filter.eq('gridcode', 5));
var shed6 = catchments.filter(ee.Filter.eq('gridcode', 6));
var shed15 = catchments.filter(ee.Filter.eq('gridcode', 15));
var shed18 = catchments.filter(ee.Filter.eq('gridcode', 18));
var shed7 = catchments.filter(ee.Filter.eq('gridcode', 7));
var shed8 = catchments.filter(ee.Filter.eq('gridcode', 8));
var shed16 = catchments.filter(ee.Filter.eq('gridcode', 16));
var shed9 = catchments.filter(ee.Filter.eq('gridcode', 9));
var shed10 = catchments.filter(ee.Filter.eq('gridcode', 10));
var shed14 = catchments.filter(ee.Filter.eq('gridcode', 14));

//Get cumulative watersheds for each site

var totalsheds2 = shed1.merge(shed2);
var totalsheds12 = shed12.merge(shed13);
var totalsheds11 = totalsheds12.merge(shed11);
var totalsheds17 = totalsheds11.merge(totalsheds2).merge(shed17);

var totalsheds3 = totalsheds17.merge(shed3);
var totalsheds4 = totalsheds3.merge(shed4);
var totalsheds5 = totalsheds4.merge(shed5);
var totalsheds6 = totalsheds5.merge(shed6);
var totalsheds18 = totalsheds6.merge(shed18);
var totalsheds7 = totalsheds18.merge(shed7);
var totalsheds8 = totalsheds7.merge(shed8);
var totalsheds16 = shed16.merge(shed15);
var totalsheds9 = totalsheds16.merge(totalsheds8);
var totalsheds10 = totalsheds9.merge(shed10);
var DCsheds = totalsheds10.merge(shed14);

//next step: analyze LULC within each catchment. Reduce region.
