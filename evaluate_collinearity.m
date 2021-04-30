% Evaluate collinearity of variables to input to Maxent

era = 'PGW';
indir = ['/home/abby/Rock_glacier_research/WUS/Data/Maxent_tables/',era,'/'];
bg = readtable([indir,'background.txt']);

% get variable names
nms = bg.Properties.VariableNames;
f = ismember(nms, {'Id','lon','lat','lith'});

% prepare matrix for correlations
bgm = table2array(bg);
bgm = bgm(:,~f);

% compute correlations
cc = corr(bgm);

% identify combinations with |r|>.7
ff = find(abs(cc) > .7);
[ro,co] = ind2sub(size(cc),ff);


% plot
figure(1);clf;
imagesc(cc);
colorbar();
set(gca,'CLim',[-1,1]);
colormap(flip(jet))
xticks(1:size(bgm,2));
yticks(1:size(bgm,2));
xticklabels(nms(~f));
yticklabels(nms(~f));
hold on;
plot(ro,co,'wx','MarkerSize',10);
xtickangle(45);
print(['/home/abby/Rock_glacier_research/WUS/Figures/correlation_matrix_',era,'.png'],'-dpng','-r400');

