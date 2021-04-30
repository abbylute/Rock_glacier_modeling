% Extract snow variables for bg and rg locations

era = 'PGW';
cdir = ['/home/abby/DATA/WRF/downscaled/WUS/',era,'/summaries/snow/WUS/'];
rdir = '/home/abby/Rock_glacier_research/WUS/Data/';
outdir = [rdir,'Maxent_tables/',era,'/'];
outmatdir = [rdir,'Snow/',era,'/'];

% load points
rg = matfile([rdir,'Rock_glaciers/index_rg_points_in_grid.mat']);
rg_col = rg.rg_col;
rg_row = rg.rg_row;
bg = matfile([rdir,'Domain/index_bg_points_in_grid.mat']);
bg_col = bg.bg_col;
bg_row = bg.bg_row;

% snow variables to extract:
% annual SFE
% snow duration
% max SWE
% nosnowdays


%--- Extract SFE ---%
tt = matfile([cdir,'snowfall.mat']);
tt = tt.snowfall;
tt = nansum(tt,3);

bg_sfe = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_sfe(ii) = tt(bg_row(ii),bg_col(ii));
end

rg_sfe = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_sfe(ii) = tt(rg_row(ii),rg_col(ii));
end

save([outmatdir,'sfe.mat'],'tt');
clear tt



%--- Extract Duration ---%
tt = matfile([cdir,'duration.mat']);
tt = tt.duration;

bg_duration = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_duration(ii) = tt(bg_row(ii),bg_col(ii));
end

rg_duration = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_duration(ii) = tt(rg_row(ii),rg_col(ii));
end

save([outmatdir,'duration.mat'],'tt');
clear tt



%--- Extract maxswe ---%
tt = matfile([cdir,'maxswe.mat']);
tt = tt.maxswe;

bg_maxswe = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_maxswe(ii) = tt(bg_row(ii),bg_col(ii));
end

rg_maxswe = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_maxswe(ii) = tt(rg_row(ii),rg_col(ii));
end

save([outmatdir,'maxswe.mat'],'tt');
clear tt



%--- Extract nosnowdays ---%
tt = matfile([cdir,'snowdays.mat']);
tt = tt.nosnowdays;

bg_nosnowdays = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_nosnowdays(ii) = tt(bg_row(ii),bg_col(ii));
end

rg_nosnowdays = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_nosnowdays(ii) = tt(rg_row(ii),rg_col(ii));
end

save([outmatdir,'nosnowdays.mat'],'tt');
clear tt


%--- Save ---%
rgtab = table();
rgtab.sfe = rg_sfe;
rgtab.duration = rg_duration;
rgtab.maxswe = rg_maxswe;
rgtab.nosnowdays = rg_nosnowdays;

bgtab = table();
bgtab.sfe = bg_sfe;
bgtab.duration = bg_duration;
bgtab.maxswe = bg_maxswe;
bgtab.nosnowdays = bg_nosnowdays;

writetable(rgtab, [outdir,'rg_snow.txt']);
writetable(bgtab, [outdir,'bg_snow.txt']);

