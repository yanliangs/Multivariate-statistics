library(mixOmics)
library(ggplot2)

##读入文件
#变量：X
phylum <- read.delim('D:\\Rdata\\plsda\\phylum_table.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
phylum <- data.frame(t(phylum))

#样品分组：Y
group <- read.delim('D:\\Rdata\\plsda\\group.txt', sep = '\t', stringsAsFactors = FALSE)


##PLS-DA 分析

phylum <- phylum[group$names, ]
#选取3个成分
plsda_result <-plsda(phylum, group$group, ncomp=3)

#简要查看结果
plsda_result
#或
names(plsda_result)
#查看坐标轴解释量
plsda_result$explained_variance$X
plsda_result$explained_variance$Y

#查看样品坐标
plsda_result$variates$X

#使用 plotIndiv() 绘制 PLS-DA 分析结果
plotIndiv(plsda_result, ind.names = TRUE, style = 'ggplot2')


#提取坐标轴解释量（前两轴）
plsda_result_eig <- (plsda_result$explained_variance$X)[1:2]

#提取样品点坐标（前两轴）
sample_site <- data.frame(plsda_result$variates)[1:2]

#为样品点坐标添加分组信息
sample_site$names <- rownames(sample_site)
names(sample_site)[1:2] <- c('plsda1', 'plsda2')
sample_site <- merge(sample_site, group, by = 'names', all.x = TRUE)


#使用 ggplot2 简单绘制 PLS-DA 结果图
plsda_plot <- ggplot(sample_site, aes(plsda1, plsda2, color = group, label = names)) +
  geom_point(size =3,alpha = 0.6) + 
  stat_ellipse(show.legend = F) +	#添加 95% 置信椭圆
  scale_color_manual(values = c('#1D7ACC', '#F67433', '#00815F')) +
  theme(panel.grid = element_line(color = 'grey50'), panel.background = element_rect(color = 'black', fill = 'transparent')) + 
  theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +
  labs(x = paste('PLS-DA axis1 ( explained variance ', round(100 * plsda_result_eig[1], 2), '% )', sep = ''), y = paste('PLS-DA axis2 ( explained variance ', round(100 * plsda_result_eig[2], 2), '% )', sep = ''))

#可选输出各样品的 PLS-DA 分析结果
write.table(sample_site, 'plsda_sample.txt', row.names = FALSE, sep = '\t', quote = FALSE)
#输出作图结果，存在设置的file
ggsave('plsda_plot.pdf', plsda_plot, width = 6, height = 5)
ggsave('plsda_plot.png', plsda_plot, width = 6, height = 5)
