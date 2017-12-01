d1_multi <- fa.multi(d1_all, nfactors = 3, nfact2 = 2, rotate = "oblimin", scores = "tenBerge")
# fa.sort(d1_multi$f1)
# fa.sort(d1_multi$f2)
fa.multi.diagram(d1_multi)

d2_multi <- fa.multi(d2_all, nfactors = 3, nfact2 = 2, rotate = "oblimin", scores = "tenBerge")
# fa.sort(d2_multi$f1)
# fa.sort(d2_multi$f2)
fa.multi.diagram(d2_multi)

d3_multi <- fa.multi(d3_all, nfactors = 3, nfact2 = 2, rotate = "oblimin", scores = "tenBerge")
# fa.sort(d3_multi$f1)
# fa.sort(d3_multi$f2)
fa.multi.diagram(d3_multi)

d4_multi <- fa.multi(d4_all, nfactors = 3, nfact2 = 2, rotate = "oblimin", scores = "tenBerge")
# fa.sort(d4_multi$f1)
# fa.sort(d4_multi$f2)
fa.multi.diagram(d4_multi)

d_slide_multi <- fa.multi(d_slide_all %>% select(-age, -character), nfactors = 3, nfact2 = 2, rotate = "oblimin", scores = "tenBerge")
# fa.sort(d_slide_multi$f1)
# fa.sort(d_slide_multi$f2)
fa.multi.diagram(d_slide_multi)




