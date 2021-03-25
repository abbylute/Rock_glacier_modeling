% for Rock Glacier Study, convert matfiles to rasters



dir = '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/';

% headwall 5
hw = matfile([dir,'Topography/headwall_WUS_210m.mat']);
lat = hw.lat;
lon = hw.lon;
hw = hw.hw;
R = georasterref('RasterSize',size(hw), ...
    'LatitudeLimits',[min(lat(:)) max(lat(:))],'LongitudeLimits',[min(lon(:)) max(lon(:))]);
fn = [outdir,'Topography/headwall_210m_WUS.tif'];
geotiffwrite(fn, flipud(hw), R)


% headwall 3
hw = matfile([dir,'Topography/headwall_WUS_210m_3x3.mat']);
lat = hw.lat;
lon = hw.lon;
hw = hw.hw;
R = georasterref('RasterSize',size(hw), ...
    'LatitudeLimits',[min(lat(:)) max(lat(:))],'LongitudeLimits',[min(lon(:)) max(lon(:))]);
fn = [dir,'Topography/headwall_210m_WUS_3x3.tif'];
geotiffwrite(fn, flipud(hw), R)

