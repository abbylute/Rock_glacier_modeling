% Evaluate collinearity of variables to input to Maxent


indir = '/home/abby/Rock_glacier_research/WUS/Data/Maxent_tables/';
bg = readtable([indir,'background.txt']);

% get variable names
nms = bg.Properties.VariableNames;
f = ismember(nms, {'Id','lon','lat','hw3','lith'});

% prepare matrix for correlations
bgm = table2array(bg);
bgm = bgm(:,~f);

% compute correlations
cc = corr(bgm);

% plot
figure(1);clf;
imagesc(cc);
colorbar();
set(gca,'CLim',[-1,1]);
colormap jet
xticks(1:size(bgm,2));
yticks(1:size(bgm,2));
xticklabels(nms(~f));
yticklabels(nms(~f));




