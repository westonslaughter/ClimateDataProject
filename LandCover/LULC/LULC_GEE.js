<<<<<<< HEAD
var L7: ImageCollection "USGS Landsat 7 Collection 1 Tier 1 and Real-Time data Raw Scenes"
var DC_watershed: Table 'users/dorothy345/DC_watershed'
var DC_sites: Table 'users/dorothy345/DC_catchment_sites_forupload'
var catchments: Table 'users/dorothy345/DC_catchments_shp'

var roi= DC_watershed;
Map.centerObject(roi, 11);

//filter Landsat images to low cloud cover, deer creek watershed
var DCFiltered = L7.filterBounds(DC_watershed)
  .filterMetadata("CLOUD_COVER", "less_than", 1);

//Get Landsat7 images from 2001
var DC_2000 = DCFiltered.filterDate('2000-01-01', '2000-12-31')
    .median();

Map.addLayer(DC_2000, imageVisParam, 'DC2000')

//Get Landsat7 image from 2019
var DC_2019 = DCFiltered.filterDate('2019-01-01', '2019-12-31')
    .median();

Map.addLayer(DC_2019, imageVisParam2, 'DC2019');

var DC_outline = DC_watershed.style
({color: '6de96f', 'fillColor': 'FFFFFF00', 'lineType': 'solid'});

//PART 1. IMAGE CLASSIFICATION
//create training data: water = 0, forest = 1, urban = 2, openLand = 3
//for now, openLand includes ag, scrub.

//merge the training classes together
var training_points = Water.merge(forest).merge(urban).merge(openLand).merge(bare_soil);
var training_data = DC_2000.sampleRegions({
  collection: training_points,
  properties: ['LC'], scale: 30});

  print(training_data);

//create a classifier. RF worked better than smileCart.10 trees
  var classifierRF = ee.Classifier.smileRandomForest(10)
    classifierRF = classifierRF.train({
      features: training_data,
      classProperty: 'LC',
      inputProperties: ['B2', 'B3', 'B4']
    });

  //classify the image
    var DC_ClassifiedRF = DC_2000.classify(classifierRF)
    Map.addLayer(DC_ClassifiedRF, imageVisParam12, 'DC_classifiedRF');
    Map.addLayer(DC_outline, imageVisParam10, 'DC Outline');

  //RF confusion matrix:
   print(classifierRF.confusionMatrix(), 'RF Confusion matrix');

  //try to get validation data: (From GEE tutorial)
  //1. get a single L7 image (the least cloudy)

  //STILL WORKING ON THIS. stole from stack xchange
    //get validation data
    var validation = DC_2000.addBands(DCFiltered).sample({
      numPixels: 5000,
      seed: 1
    }).filter(ee.Filter.neq('B2', null));

    var validated = validation.classify(classifierRF);

  //get confusion matrix of expected accuracy
    var testAccuracy = validated.errorMatrix('LC', 'classification');
    print('Validation error matrix:', testAccuracy);
    print('Validation overall accuracy:', testAccuracy.accuracy());

//PART 2. FIND % EACH LC WITHIN THE WATERSHED
  //Calculate watershed area in km2

    var DC_watershed_area = DC_watershed.map(function(feature){
    var num = ee.Number.parse(feature.get('AreaSqKm'));
    return feature.set('AreaSqKm', num);
  });
  print('DC_Area', DC_watershed_area.aggregate_stats('AreaSqKm'));

  var area = 231.7;

  //isolate just one LC: in this case, forest=1, all else=0.
     var forest_mask = DC_ClassifiedRF.eq(1)
     .clipToCollection(DC_watershed);
     Map.addLayer(forest_mask, {color:'000000'}, 'forest_mask');

  //calculate the area of each forest pixel
     var forest_area2000 = forest_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

  //sum the value of the forest pixels in DC watershed
     var forest_sum2000 = forest_area2000.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

     print('forest area 2000', forest_sum2000, 'km2');

  //Forest area = 117.9km2. Now we can calculate the percentage
  //of area in DC watershed that is forest:

  var percent_forest2000 = 117.9/(area)*100;
  print('percent forest 2000', percent_forest2000);

//REPEAT THIS PROCESS FOR ALL OTHER LC's

//PART 3. LULC IN SITE CATCHMENTS
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

//step 3. Get cumulative watersheds for each site

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

//next step: analyze LULC within each catchment. Reduce region again.
=======
var L7: ImageCollection "USGS Landsat 7 Collection 1 Tier 1 and Real-Time data Raw Scenes"
var DC_watershed: Table 'users/dorothy345/DC_watershed'
var DC_sites: Table 'users/dorothy345/DC_catchment_sites_forupload'
var catchments: Table 'users/dorothy345/DC_catchments_shp'

var roi= DC_watershed;
Map.centerObject(roi, 11);

//filter Landsat images to low cloud cover, deer creek watershed
var DCFiltered = L7.filterBounds(DC_watershed)
  .filterMetadata("CLOUD_COVER", "less_than", 1);

//Get Landsat7 images from 2001
var DC_2000 = DCFiltered.filterDate('2000-01-01', '2000-12-31')
    .median();

Map.addLayer(DC_2000, imageVisParam, 'DC2000')

//Get Landsat7 image from 2019
var DC_2019 = DCFiltered.filterDate('2019-01-01', '2019-12-31')
    .median();

Map.addLayer(DC_2019, imageVisParam2, 'DC2019');

var DC_outline = DC_watershed.style
({color: '6de96f', 'fillColor': 'FFFFFF00', 'lineType': 'solid'});

//PART 1. IMAGE CLASSIFICATION
//create training data: water = 0, forest = 1, urban = 2, openLand = 3
//for now, openLand includes ag, scrub.

//merge the training classes together
var training_points = Water.merge(forest).merge(urban).merge(openLand).merge(bare_soil);
var training_data = DC_2000.sampleRegions({
  collection: training_points,
  properties: ['LC'], scale: 30});

  print(training_data);

//create a classifier. RF worked better than smileCart.10 trees
  var classifierRF = ee.Classifier.smileRandomForest(10)
    classifierRF = classifierRF.train({
      features: training_data,
      classProperty: 'LC',
      inputProperties: ['B2', 'B3', 'B4']
    });

  //classify the image
    var DC_ClassifiedRF = DC_2000.classify(classifierRF)
    Map.addLayer(DC_ClassifiedRF, imageVisParam12, 'DC_classifiedRF');
    Map.addLayer(DC_outline, imageVisParam10, 'DC Outline');

  //RF confusion matrix:
   print(classifierRF.confusionMatrix(), 'RF Confusion matrix');

  //try to get validation data: (From GEE tutorial)
  //1. get a single L7 image (the least cloudy)

  //STILL WORKING ON THIS. stole from stack xchange
    //get validation data
    var validation = DC_2000.addBands(DCFiltered).sample({
      numPixels: 5000,
      seed: 1
    }).filter(ee.Filter.neq('B2', null));

    var validated = validation.classify(classifierRF);

  //get confusion matrix of expected accuracy
    var testAccuracy = validated.errorMatrix('LC', 'classification');
    print('Validation error matrix:', testAccuracy);
    print('Validation overall accuracy:', testAccuracy.accuracy());

//PART 2. FIND % EACH LC WITHIN THE WATERSHED
  //Calculate watershed area in km2

    var DC_watershed_area = DC_watershed.map(function(feature){
    var num = ee.Number.parse(feature.get('AreaSqKm'));
    return feature.set('AreaSqKm', num);
  });
  print('DC_Area', DC_watershed_area.aggregate_stats('AreaSqKm'));

  var area = 231.7;

  //isolate just one LC: in this case, forest=1, all else=0.
     var forest_mask = DC_ClassifiedRF.eq(1)
     .clipToCollection(DC_watershed);
     Map.addLayer(forest_mask, {color:'000000'}, 'forest_mask');

  //calculate the area of each forest pixel
     var forest_area2000 = forest_mask.multiply(ee.Image.pixelArea().divide(1000*1000));

  //sum the value of the forest pixels in DC watershed
     var forest_sum2000 = forest_area2000.reduceRegion({
       reducer: ee.Reducer.sum(),
       geometry: DC_watershed,
       scale: 30,
       maxPixels: 1e13
     });

     print('forest area 2000', forest_sum2000, 'km2');

  //Forest area = 117.9km2. Now we can calculate the percentage
  //of area in DC watershed that is forest:

  var percent_forest2000 = 117.9/(area)*100;
  print('percent forest 2000', percent_forest2000);

//REPEAT THIS PROCESS FOR ALL OTHER LC's

//PART 3. LULC IN SITE CATCHMENTS
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

//next step: analyze LULC within each catchment. Reduce region again.
>>>>>>> 4f79aef821a38f333778a4ca9939118932aa91f2
