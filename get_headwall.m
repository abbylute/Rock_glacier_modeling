
mdir = '/Volumes/WDPassport/';
dem = matfile([mdir,'DATA/DEM/NED/new/WUS_NED_210m.mat']);
elev = dem.elev;
lat = dem.lat;
lon = dem.lon;
figure(1);clf;imagesc(elev);


slope = matfile([mdir,'Rock_glacier_research/WUS/Data/Topography/slope_210m_WUS.mat']);
xy = slope.xy;
slope = slope.slope;

% reshape into a grid
gridside = sum(xy(:,1)==xy(1,1));

slope = reshape(slope,size(slope,1)/gridside,gridside)';
figure(1);imagesc(slope);


% Calculate headwall metric
% "To quantify the area of potential debris supply we propose to calculate 
% a headwall metric as the percentage of pixels within a given radius 
% (e.g., 400m) of the target pixel that are above the target pixel and 
% exceed a slope threshold (e.g., 30°)." 
buf = 1;%2;
slope_thres = 30;
hw = ones(size(elev)).*NaN;

for r = 3:(size(elev,1)-buf)
    disp(r)
    for c = 3:(size(elev,2)-buf)
        % 1. extract elev and slope within a buffer of each cell
        elevx = elev((r-buf):(r+buf),(c-buf):(c+buf));
        slopex = slope((r-buf):(r+buf),(c-buf):(c+buf));
        npix = sum(~isnan(elevx),'all');

        % 2. calculate hw
        if npix == 0
            hw(r,c) = NaN;
        else
            hw(r,c) = sum(elevx > elev(r,c) & slopex > slope_thres & ~isnan(elevx), 'all') ./ npix;
        end
    end % end cols
end % end rows

save([mdir,'Rock_glacier_research/WUS/Data/Topography/headwall_WUS_210m_3x3.mat'],'lat','lon','hw')


