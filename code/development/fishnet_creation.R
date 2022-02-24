bbox = st_as_sf(st_as_sfc(st_bbox(aoi)))
grid = st_make_grid(bbox, cellsize = 0.25)
