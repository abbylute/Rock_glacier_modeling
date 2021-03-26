% for Rock Glacier Study, convert matfiles to rasters


%% ON LAPTOP
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



%% ON TESLA
era = 'PRE';
cdir = ['/mnt/ceph/alute/DATA/WRF/downscaled/WUS/',era,'/summaries/'];
outdir = '/mnt/ceph/alute/Rock_glacier_research/Data/climate_rasters/';

% lonlat
ll = matfile('/mnt/ceph/alute/DATA/Mapping/WUS_NED_210m.mat');
lon = ll.lon;
lat = ll.lat;
clear ll

R = georasterref('RasterSize',size(lon), ...
    'LatitudeLimits',[min(lat(:)) max(lat(:))],'LongitudeLimits',[min(lon(:)) max(lon(:))]);


% temperatures
tt = matfile([cdir,'climate/WUS/tmean.mat']);
tt = tt.tmean;
tmin = min(tt,[],3);
tmax = max(tt,[],3);
tmean = mean(tt,3);

fn = [outdir,'tmin_210m_WUS.tif'];
geotiffwrite(fn, flipud(tmin), R)

fn = [outdir,'tmax_210m_WUS.tif'];
geotiffwrite(fn, flipud(tmax), R)

fn = [outdir,'tmean_210m_WUS.tif'];
geotiffwrite(fn, flipud(tmean), R)

clear tmin tmax tmean

% temperature oscillations
tt = matfile([cdir,'climate/WUS/tschange.mat']);
tt = tt.tschange;

fn = [outdir,'tschange_210m_WUS.tif'];
geotiffwrite(fn, flipud(tt), R)

clear tt


% precipitation
ppt = matfile([cdir,'climate/WUS/ppt.mat']);
ppt = sum(ppt.ppt,3);

fn = [outdir,'ppt_210m_WUS.tif'];
geotiffwrite(fn, flipud(ppt), R)


% solar
sw = matfile([cdir,'climate/WUS/swdown.mat']);
sw = mean(sw.swdown,3);

fn = [outdir,'sw_210m_WUS.tif'];
geotiffwrite(fn, flipud(sw), R)







