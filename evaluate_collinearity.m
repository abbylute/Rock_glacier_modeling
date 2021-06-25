% Evaluate collinearity of variables to input to Maxent

indir = '/home/abby/Rock_glacier_research/WUS/Data/Maxent_tables/';

%% load preindustrial data
bg_pre = readtable([indir,'PRE/background.txt']);
bg_pre.rain = bg_pre.ppt - bg_pre.sfe;

% get variable names
nms = bg_pre.Properties.VariableNames;
f = ismember(nms, {'Id','lon','lat','lith'});
% option to include only the variables we used in modeling
%f = ismember(nms, {'Id','lon','lat','lith','hw3','tmin','tmax','maxswe','ppt'}); 

% prepare matrix for correlations
bg_prem = table2array(bg_pre);
bg_prem = bg_prem(:,~f);


%% load historical data
bg_ctrl = readtable([indir,'CTRL/background.txt']);
bg_ctrl.rain = bg_ctrl.ppt - bg_ctrl.sfe;

% get variable names
nms = bg_ctrl.Properties.VariableNames;
f = ismember(nms, {'Id','lon','lat','lith'});
% option to include only the variables we used in modeling
%f = ismember(nms, {'Id','lon','lat','lith','hw3','tmin','tmax','maxswe','ppt'}); 

% prepare matrix for correlations
bg_ctrlm = table2array(bg_ctrl);
bg_ctrlm = bg_ctrlm(:,~f);


%% load future data
bg_pgw = readtable([indir,'PGW/background.txt']);
bg_pgw.rain = bg_pgw.ppt - bg_pgw.sfe;

% get variable names
nms = bg_pgw.Properties.VariableNames;
f = ismember(nms, {'Id','lon','lat','lith'});
% option to include only the variables we used in modeling
%f = ismember(nms, {'Id','lon','lat','lith','hw3','tmin','tmax','maxswe','ppt'}); 

% prepare matrix for correlations
bg_pgwm = table2array(bg_pgw);
bg_pgwm = bg_pgwm(:,~f);


%% clean up variable names
nms = {'Id','lon','lat','aspect','slope','headwall5','headwall3','rocktype','tmin','tmax','tmean','freeze-thaw','precip','solar','sfe','duration','maxswe','nosnowdays','rain'};


%% compute correlations
cpre = corr(bg_prem);
cctrl = corr(bg_ctrlm);
cpgw = corr(bg_pgwm);

% identify combinations with |r|>.7
ff = find(abs(cpre) > .7);
[ropre,copre] = ind2sub(size(cpre),ff);

ff = find(abs(cctrl) > .7);
[roctrl,coctrl] = ind2sub(size(cctrl),ff);

ff = find(abs(cpgw) > .7);
[ropgw,copgw] = ind2sub(size(cpgw),ff);


%% plot
xst = .17;
xwi = .83;

figure(1);clf;
fig = gcf; fig.PaperUnits = 'inches'; fig.PaperPosition = [0 0 4.5 11];%[0 0 10 4.5];
subplot('position', [xst 0.69 xwi .28]);
%subplot(3,1,1);
imagesc(cpre);
colorbar();
set(gca,'CLim',[-1,1]);
colormap(flip(jet))
set(gca,'xticklabel',[])
%xticks(1:size(bg_prem,2));
yticks(1:size(bg_prem,2));
%xticklabels(nms(~f));
yticklabels(nms(~f));
a = get(gca,'YTickLabel');
set(gca,'YTickLabel',a,'fontsize',8);
hold on;
plot(ropre,copre,'wx','MarkerSize',16,'LineWidth',3);
%xtickangle(45);
title('Pre-industrial','FontSize',15)

hold on;

subplot('position', [xst 0.38 xwi .28]);
%subplot(3,1,2);
imagesc(cctrl);
colorbar();
set(gca,'CLim',[-1,1]);
colormap(flip(jet))
set(gca,'xticklabels',[]);
%xticks(1:size(bg_ctrlm,2));
yticks(1:size(bg_ctrlm,2));
%xticklabels(nms(~f));
yticklabels(nms(~f));
a = get(gca,'YTickLabel');
set(gca,'YTickLabel',a,'fontsize',8);
hold on;
plot(roctrl,coctrl,'wx','MarkerSize',16,'LineWidth',3);
%xtickangle(45);
title('Present','FontSize',15)

hold on;

subplot('position', [xst 0.07 xwi .28]);
%subplot(3,1,3);
imagesc(cpgw);
colorbar();
set(gca,'CLim',[-1,1]);
colormap(flip(jet))
xticks(1:size(bg_pgwm,2));
yticks(1:size(bg_pgwm,2));
xticklabels(nms(~f));
yticklabels(nms(~f));
a = get(gca,'XTickLabel');
set(gca,'XTickLabel',a,'fontsize',8);
hold on;
plot(ropgw,copgw,'wx','MarkerSize',16,'LineWidth',3);
xtickangle(45);
title('Future','FontSize',15);

print(['/home/abby/Rock_glacier_research/WUS/Figures/correlation_matrix.png'],'-dpng','-r400');

