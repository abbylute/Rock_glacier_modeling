% Compile topo, geo, climate, and snow variables into tables to input to
% Maxent

era = 'PGW';
indir = '/home/abby/Rock_glacier_research/WUS/Data/Maxent_tables/';
outdir = indir;


% load rg files
topogeo = readtable([indir,'PRE/rg_topo_geo.txt']);
clim = readtable([indir,era,'/rg_climate.txt']);
snow = readtable([indir,era,'/rg_snow.txt']);

% compile and save rg data
rgtab = [topogeo, clim, snow];
writetable(rgtab, [outdir,era,'/sample.txt']);


% load bg files
topogeo = readtable([indir,'PRE/bg_topo_geo.txt']);
clim = readtable([indir,era,'/bg_climate.txt']);
snow = readtable([indir,era,'/bg_snow.txt']);

% compile and save bg data
bgtab = [topogeo, clim, snow];
writetable(bgtab, [outdir,era,'/background.txt']);

% randomly sample some background pixels to use in maxent
ns = 10000; % number of samples
y = datasample(1:size(bgtab,1), ns, 'Replace', false);
bgtab1 = bgtab(y,:);
writetable(bgtab1, [outdir,era,'/background_',num2str(ns),'.txt']);
