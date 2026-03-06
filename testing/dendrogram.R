if(FALSE){
  require(patchwork)
  p_balances = ggplot(data=pivot_longer(dplot, -group)) +  geom_vline(xintercept = 0, col = 'blue', linetype = 'dotted') + geom_boxplot(aes(x = value, y = group, fill = group), outliers = FALSE) + guides(fill = 'none') + scale_y_discrete(limits = rev) + facet_wrap(~name, nrow = 1) + theme_test() + theme(strip.text = element_text(colour = 'blue'), strip.background = element_rect(fill = 'white')) + labs(y = NULL, x = NULL)

  p_balances_vert = ggplot(data=pivot_longer(dplot, -group)) +
    geom_vline(xintercept = 0, col = 'blue', linetype = 'dotted') +
    geom_boxplot(aes(y = value, x = group, fill = group), outliers = FALSE) + #guides(fill = 'none') +
    # scale_y_discrete(limits = rev) +
    facet_wrap(~name, ncol = 1) + theme_test() + theme(strip.text = element_text(colour = 'blue'), strip.background = element_rect(fill = 'white')) + labs(y = NULL, x = NULL)
  p_balances / p_dendrogram + plot_layout(heights = c(1, 6))
  (p_dendrogram + coord_flip() + guides(fill = 'none') + p_balances_vert) + plot_layout(widths = c(6, 1))
}
