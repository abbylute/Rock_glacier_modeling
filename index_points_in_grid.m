% find the indices of the background and rock glaciers points within the
% 210m grid

mdir = '/home/abby/Rock_glacier_research/WUS/Data/';

rg = readtable([mdir,'Rock_glaciers/active_rgs.csv']);
rgxy = table2array(rg(:,2:3));
bg = table2array(readtable([mdir,'Domain/points_to_model.csv']));

clonlat = matfile('/home/abby/DATA/Mapping/WUS_NED_210m.mat');
lon = clonlat.lon;
lat = clonlat.lat;

% find indices of background points in lat/lon grid
tic
[~,bg_col] = ismember(round(bg(:,1),5),round(lon(1,:),5));
[~,bg_row] = ismember(round(bg(:,2),5),round(lat(:,1),5));
toc
save([mdir,'Domain/index_bg_points_in_grid.mat'],'bg_row','bg_col');
figure(1);clf;plot(bg(1:100,1),lon(1,bg_col(1:100)),'.');
figure(1);clf;plot(bg(1:100,2),lat(bg_row(1:100),1),'.');
bg(1,:)
lon(bg_row(1),bg_col(1)) 
lat(bg_row(1),bg_col(1))

% find indices of rockglacier points in lat/lon grid
tic
rg_row = nan(size(rgxy,1),1);
rg_col = nan(size(rgxy,1),1);
for rr = 1:size(rgxy,1)
    dlat = abs(rgxy(rr,2)-lat(:,1));
    dlon = abs(rgxy(rr,1)-lon(1,:));
    
    rg_row(rr,1) = find(dlat == min(dlat));
    rg_col(rr,1) = find(dlon == min(dlon));
    
end
toc
% check that this actually found grid cells (0=no match)
min(rg_row)
min(rg_col)

save([mdir,'Rock_glaciers/index_rg_points_in_grid.mat'],'rg_row','rg_col');
figure(1);clf;plot(rgxy(:,1),lon(1,rg_col),'.');
figure(1);clf;plot(rgxy(:,2),lat(rg_row,1),'.');


















