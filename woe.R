# Weight of Evidence and Information value
woe.dat <- tbl_df(bl.final) %>% select(y, bl_pa, bl_da)

total.goods <- sum(woe.dat$y)
total.bads <- sum(!woe.dat$y)

cat.pa <- c(0, 30, 50, 100)
cat.da <- c(0, 76, 100)

woe.dat <- woe.dat %>% mutate(pa.class = cut(bl_pa, breaks = cat.pa, labels = F),
                      da.class = cut(bl_da, breaks = cat.da, labels = F))

# PA woe
woe.agg.pa <- woe.dat %>% group_by(pa.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
    mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
           woe = goods.per / bads.per, diff = goods.per - bads.per,
           iv = round(diff * woe * 100, 2))

# DA woe
woe.agg.da <- woe.dat %>% group_by(da.class) %>% summarise(goods = sum(y), bads = sum(!y)) %>%
    mutate(goods.per = goods / total.goods, bads.per = bads / total.bads,
           woe = goods.per / bads.per, diff = goods.per - bads.per,
           iv = round(diff * woe * 100, 2))