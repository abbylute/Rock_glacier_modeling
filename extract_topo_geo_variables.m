
mdir = '/Volumes/WDPassport/Rock_glacier_research/WUS/';
outdir = [mdir,'Data/Maxent_tables/'];

rgtab = readtable([mdir,'Data/Rock_glaciers/active_rgs.csv']);
bgtab = readtable([mdir,'Data/Domain/points_to_model.csv']);


rg = matfile([mdir,'Data/Rock_glaciers/index_rg_points_in_grid.mat']);
rg_row = rg.rg_row;
rg_col = rg.rg_col;
bg = matfile([mdir,'Data/Domain/index_bg_points_in_grid.mat']);
bg_row = bg.bg_row;
bg_col = bg.bg_col;

gridrows = 10802;
gridcols = 11831;


%--- Extract aspect ---%
asp = matfile([mdir,'Data/Topography/aspect_210m_WUS.mat']);
aspect = asp.aspect;
aspect = reshape(aspect,gridcols,gridrows)';
figure(1);clf;imagesc(aspect);colorbar();

bg_aspect = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_aspect(ii) = aspect(bg_row(ii),bg_col(ii));
end

rg_aspect = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_aspect(ii) = aspect(rg_row(ii),rg_col(ii));
end

clear asp aspect


%--- Extract slope ---%
slo = matfile([mdir,'Data/Topography/slope_210m_WUS.mat']);
slope = slo.slope;
slope = reshape(slope,gridcols,gridrows)';
figure(1);clf;imagesc(slope);colorbar();

bg_slope = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_slope(ii) = slope(bg_row(ii),bg_col(ii));
end

rg_slope = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_slope(ii) = slope(rg_row(ii),rg_col(ii));
end

clear slo slope


%--- Extract headwall ---%
hw5 = matfile([mdir,'Data/Topography/headwall_WUS_210m.mat']);
hw5 = hw5.hw;

hw3 = matfile([mdir,'Data/Topography/headwall_WUS_210m_3x3.mat']);
hw3 = hw3.hw;

bg_hw5 = nan(size(bg_col,1),1);
bg_hw3 = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_hw5(ii) = hw5(bg_row(ii),bg_col(ii));
    bg_hw3(ii) = hw3(bg_row(ii),bg_col(ii));
end

rg_hw5 = nan(size(rg_col,1),1);
rg_hw3 = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_hw5(ii) = hw5(rg_row(ii),rg_col(ii));
    rg_hw3(ii) = hw3(rg_row(ii),rg_col(ii));
end

clear hw5 hw3

% using mean or median, there is a greater contrast between bg and rg
% headwall values using the 5 cell hw calculation than the 3 cell hw
% calculation.
mean(bg_hw5)-mean(rg_hw5)
mean(bg_hw3)-mean(rg_hw3)


%--- extract lithology ---%
li = matfile('/Volumes/WDPassport/DATA/Geology/Lithology/Generalizedlith/WUS_rocktypes_210m.mat');
lith = li.lith;
lith = reshape(lith,gridcols,gridrows)';
figure(1);clf;imagesc(lith);colorbar();

bg_lith = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_lith(ii) = lith(bg_row(ii),bg_col(ii));
end

rg_lith = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_lith(ii) = lith(rg_row(ii),rg_col(ii));
end

clear li lith


%--- Save ---%
rgtab.aspect = rg_aspect;
rgtab.slope = rg_slope;
rgtab.hw5 = rg_hw5;
rgtab.hw3 = rg_hw3;
rgtab.lith = rg_lith;

bgtab.Id = (1:size(bg_row,1))';
bgtab = bgtab(:,{'Id','lon','lat'});
bgtab.aspect = bg_aspect;
bgtab.slope = bg_slope;
bgtab.hw5 = bg_hw5;
bgtab.hw3 = bg_hw3;
bgtab.lith = bg_lith;

writetable(rgtab, [outdir,'rg_topo_geo.txt']);
writetable(bgtab, [outdir,'bg_topo_geo.txt']);


