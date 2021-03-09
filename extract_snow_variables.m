% Extract snow variables for bg and rg locations

era = 'PRE';
cdir = ['/home/abby/DATA/WRF/downscaled/WUS/',era,'/summaries/'];
rdir = '/home/abby/Rock_glacier_research/WUS/Data/';
outdir = [rdir,'Maxent_tables/'];

% load points
rg = matfile([rdir,'Rock_glaciers/index_rg_points_in_grid.mat']);
rg_col = rg.rg_col;
rg_row = rg.rg_row;
bg = matfile([rdir,'Domain/index_bg_points_in_grid.mat']);
bg_col = bg.bg_col;
bg_row = bg.bg_row;

% snow variables to extract:
% 


