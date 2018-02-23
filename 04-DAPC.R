library(poppr)
library(ggcompoplot)
library(tidyverse)
library(cowplot)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- readRDS(here::here("data", "full-genclone-object.rds"))

if (interactive()){
  set.seed(2017-12-14)
  CDXVAL <- xvalDapc(
    tab(CD, NA.method = "mean"),
    grp = pop(CD),
    n.pca.max = 30,
    n.rep = 500,
    n.pc = seq.int(15)
  )
}

CD_DAPC <- dapc(CD, n.pca = 7L, n.da = 4L)
CD_DAPC
DAPCdf <- CD_DAPC$ind.coord[, 1:2] %>%
  as.data.frame() %>%
  tibble::rownames_to_column("GenoID") %>%
  tibble::as_tibble() %>%
  dplyr::inner_join(strata(CD)) %>%
  tibble::add_column(MLG = mll(CD)) %>%
  dplyr::group_by(MLG) %>%
  dplyr::mutate(`N` = n()) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Population) %>% 
  dplyr::mutate(mean1 = mean(LD1, na.rm = TRUE), mean2 = mean(LD2, na.rm = TRUE))

DAPCgg <- ggplot(DAPCdf, aes(x = LD1, y = LD2, color = Population)) +
  geom_hline(yintercept = 0, color = "grey30") +
  geom_vline(xintercept = 0, color = "grey30") +
  geom_point(aes(size = N)) +
  # geom_segment(aes(x = mean1, y = mean2, xend = LD1, yend = LD2), alpha = 0.5) +
  stat_ellipse(type = "norm", level = 0.666, alpha = 0.75) +
  scale_color_manual(values = other(CD)$palette) +
  theme_bw(base_size = 16, base_family = "Helvetica") +
  coord_fixed() +
  theme(aspect.ratio = 1) +
  theme(legend.box = "horizontal")

DAplot <- enframe(CD_DAPC$eig, name = "DA eigenvalues", value = "eig") %>%
  tibble::add_column(used = c(rep(LETTERS[1:3], each = 2), "C")) %>%
  ggplot(aes(x = `DA eigenvalues`, y = eig, fill = used)) +
  geom_col(color = "black") +
  theme_bw(base_size = 14, base_family = "Helvetica") +
  scale_fill_grey(end = 1) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "none") + 
  theme(aspect.ratio = 1) +
  theme(plot.background = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(title = element_text(size = 8, face = "bold"))

gd <- ggdraw() +
  draw_plot(DAPCgg) +
  draw_plot(DAplot, width = 0.28, height = 0.28,
            x = 0.045, y = 0.7)
ggsave(gd, filename = here::here("figs/DAPC-scatterplot.pdf"),
       width = 7.20472,
       height = 7.20472 * (0.55),
       units = "in"
       )
ggsave(gd, filename = here::here("figs/DAPC-scatterplot.tiff"),
       width = 7.20472,
       height = 7.20472 * (0.55),
       units = "in",
       dpi = 1200
       )


pdf(here::here("figs/DAPC-barplot.pdf"), width = 7.20472, height = 3.464565, pointsize = 5, colormodel = "cmyk")
dev.control("enable")
gg <- ggcompoplot(CD_DAPC, CD, cols = 4, pal = other(CD)$palette)
gg$data$population <- factor(gg$data$population, names(other(CD)$palette))
gg$data$oldPopulation <- factor(gg$data$oldPopulation, names(other(CD)$palette))
gg + 
  theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "top") 
dev.copy(device = tiff, here::here("figs/DAPC-barplot.tiff"), width = 7.20472, height = 3.464565, pointsize = 5, units = "in", res = 1200)
dev.off()
dev.off()
options(encoding = enc)
getOption("encoding")
