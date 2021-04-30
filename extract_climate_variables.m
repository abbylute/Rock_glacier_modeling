% Extract climate variables for bg and rg locations

era = 'PGW';
cdir = ['/home/abby/DATA/WRF/downscaled/WUS/',era,'/summaries/climate/WUS/'];
rdir = '/home/abby/Rock_glacier_research/WUS/Data/';
outdir = [rdir,'Maxent_tables/',era,'/'];
outmatdir = [rdir,'Climate/',era,'/'];

% load points
rg = matfile([rdir,'Rock_glaciers/index_rg_points_in_grid.mat']);
rg_col = rg.rg_col;
rg_row = rg.rg_row;
bg = matfile([rdir,'Domain/index_bg_points_in_grid.mat']);
bg_col = bg.bg_col;
bg_row = bg.bg_row;

% climate variables to extract:
% mean, min, and max of monthly tmean
% annual oscillations around 0°C
% annual ppt
% mean annual solar radiation



%--- Extract temperatures ---%
tt = matfile([cdir,'tmean.mat']);
tt = tt.tmean;
tmin = min(tt,[],3);
tmax = max(tt,[],3);
tmean = mean(tt,3);
clear tt

bg_tmin = nan(size(bg_col,1),1);
bg_tmax = nan(size(bg_col,1),1);
bg_tmean = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_tmin(ii) = tmin(bg_row(ii),bg_col(ii));
    bg_tmax(ii) = tmax(bg_row(ii),bg_col(ii));
    bg_tmean(ii) = tmean(bg_row(ii),bg_col(ii));
end

rg_tmin = nan(size(rg_col,1),1);
rg_tmax = nan(size(rg_col,1),1);
rg_tmean = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_tmin(ii) = tmin(rg_row(ii),rg_col(ii));
    rg_tmax(ii) = tmax(rg_row(ii),rg_col(ii));
    rg_tmean(ii) = tmean(rg_row(ii),rg_col(ii));
end


save([outmatdir,'tmin.mat'],'tmin');
save([outmatdir,'tmax.mat'],'tmax');
save([outmatdir,'tmean.mat'],'tmean');
clear tmin tmax tmean


%--- Extract T oscillations ---%
tt = matfile([cdir,'tschange.mat']);
tt = tt.tschange;

bg_tschange = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_tschange(ii) = tt(bg_row(ii),bg_col(ii));
end

rg_tschange = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_tschange(ii) = tt(rg_row(ii),rg_col(ii));
end

save([outmatdir,'tschange.mat'],'tt');
clear tt


%--- Extract ppt ---%
ppt = matfile([cdir,'ppt.mat']);
ppt = sum(ppt.ppt,3);

bg_ppt = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_ppt(ii) = ppt(bg_row(ii),bg_col(ii));
end

rg_ppt = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_ppt(ii) = ppt(rg_row(ii),rg_col(ii));
end

save([outmatdir,'ppt.mat'],'ppt');
clear ppt


%--- Extract SW ---%
sw = matfile([cdir,'swdown.mat']);
sw = mean(sw.swdown,3);

bg_sw = nan(size(bg_col,1),1);
for ii = 1:size(bg_col,1)
    bg_sw(ii) = sw(bg_row(ii),bg_col(ii));
end

rg_sw = nan(size(rg_col,1),1);
for ii = 1:size(rg_col,1)
    rg_sw(ii) = sw(rg_row(ii),rg_col(ii));
end

save([outmatdir,'swdown.mat'],'sw');
clear sw


%--- Save ---%
rgtab = table();
rgtab.tmin = rg_tmin;
rgtab.tmax = rg_tmax;
rgtab.tmean = rg_tmean;
rgtab.tschange = rg_tschange;
rgtab.ppt = rg_ppt;
rgtab.sw = rg_sw;

bgtab = table();
bgtab.tmin = bg_tmin;
bgtab.tmax = bg_tmax;
bgtab.tmean = bg_tmean;
bgtab.tschange = bg_tschange;
bgtab.ppt = bg_ppt;
bgtab.sw = bg_sw;

writetable(rgtab, [outdir,'rg_climate.txt']);
writetable(bgtab, [outdir,'bg_climate.txt']);

